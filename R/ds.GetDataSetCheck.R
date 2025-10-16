
#' ds.GetDataSetCheck
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Checks out a data set (\code{list} of \code{data.frames}) on servers and return a coherent summary across servers. Options:
#' \enumerate{
#'    \item The data set can be inspected naively without passing any additional information about it
#'    \item Meta data like required table and feature names as well as eligible value sets can be passed explicitly
#'    \item A Module identifier from a list of registered modules can be passed ('CCP' / 'P21' / ...), which leads to meta data being taken from a linked package }
#'
#' Linked to server-side AGGREGATE method \code{GetDataSetCheckDS()}
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param RequiredTableNames Optional \code{character vector} - Names of required tables
#' @param RequiredFeatureNames Optional \code{list} of \code{character vectors} - Names of required features - Default: \code{names(Table)}
#' @param EligibleValueSets Optional \code{list} of \code{character vectors} containing sets of eligible values for corresponding feature.
#' @param Module Optional \code{string} identifying a defined data set (Examples: 'CCP' / 'P21')
#' @param Stage Optional \code{string} - Indicating transformation stage of addressed data set. This is relevant for which names and values to look up in passed meta data. Options: 'Raw' / 'Curated'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing compiled meta data about data set tables:
#'         \itemize{\item TableStatus
#'                  \item TableRowCounts
#'                  \item FeatureExistence
#'                  \item FeatureTypes
#'                  \item NonMissingValueCounts
#'                  \item NonMissingValueRates
#'                  \item EligibleValueCounts
#'                  \item EligibleValueRates}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetDataSetCheck <- function(DataSetName,
                               RequiredTableNames = NULL,
                               RequiredFeatureNames = NULL,
                               EligibleValueSets = NULL,
                               Module = "CCP",
                               Stage = "Raw",
                               DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSetName <- "RawDataSet"
  # Module <- "CCP"
  # Stage <- "Raw"
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(DataSetName))
  if (!is.null(RequiredTableNames)) { assert_that(is.character(RequiredTableNames)) }
  if (!is.null(RequiredFeatureNames)) { assert_that(is.list(RequiredFeatureNames)) }
  if (!is.null(EligibleValueSets)) { assert_that(is.list(EligibleValueSets)) }
  if (!is.null(Module)) { assert_that(is.string(Module), Module %in% names(dsFredaClient::Meta.Modules),
                                      is.string(Stage), Stage %in% c("Raw", "Curated", "Augmented")) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Server function call to get list of lists
#-------------------------------------------------------------------------------
  DataSetCheck <- DSI::datashield.aggregate(conns = DSConnections,
                                            expr = call("GetDataSetCheckDS",
                                                        DataSetName.S = DataSetName,
                                                        RequiredTableNames.S = RequiredTableNames,
                                                        RequiredFeatureNames.S = RequiredFeatureNames,
                                                        EligibleValueSets.S = EligibleValueSets,
                                                        Module.S = Module,
                                                        Stage.S = Stage))

#-------------------------------------------------------------------------------
# Transform into cumulated report objects
#-------------------------------------------------------------------------------

  # Create data frame containing "traffic light" info about existence/completeness of data set tables
  TableStatus <- DataSetCheck %>%
                      map(function(ServerDataSetCheck)
                          {
                              ServerTableStatus <- ServerDataSetCheck %>%
                                                    map_chr(function(TableInfo)
                                                            {
                                                                Status <- "grey"

                                                                if (TableInfo$TableExists == FALSE) { Status <- "red" }
                                                                else if (TableInfo$TableExists == TRUE & TableInfo$TableComplete == TRUE) { Status <- "green" }
                                                                else if (TableInfo$TableExists == TRUE & TableInfo$TableComplete == FALSE) { Status <- "yellow" }

                                                                CountExistingFeatures <- sum(TableInfo$FeatureCheckOverview$Exists)
                                                                CountTotalFeatures <- nrow(TableInfo$FeatureCheckOverview)

                                                                if (Status != "grey") { Status <- paste0(Status, " (", CountExistingFeatures, "/", CountTotalFeatures, ")") }

                                                            }) %>%
                                                    rbind() %>%
                                                    as_tibble()
                          }) %>%
                      list_rbind(names_to = "ServerName") %>%
                      mutate(CheckRDSTables = case_when(if_all(-ServerName, ~ str_starts(.x, "green")) ~ "green",
                                                        if_any(-ServerName, ~ str_starts(.x, "red")) ~ "red",
                                                        if_any(-ServerName, ~ str_starts(.x, "yellow")) ~ "yellow",
                                                        TRUE ~ "grey"))


  # Create list of data frames (one per RDS table) containing table row counts at different servers
  TableRowCounts <- DataSetCheck %>%
                        list_transpose() %>%
                        map(function(TableInfo)
                            {
                                TableInfo %>%
                                     map(\(ServerTableInfo) tibble(RowCount = ServerTableInfo$RowCount)) %>%
                                     list_rbind(names_to = "ServerName")
                            })


  # Create list of data frames (one per RDS table) containing info about existence of table features
  FeatureExistence <- DataSetCheck %>%
                          list_transpose() %>%
                          map(function(TableInfo)
                              {
                                  TableInfo %>%
                                      map(\(ServerTableInfo) ServerTableInfo$FeatureCheckOverview %>% select(Feature, Exists)) %>%
                                      list_rbind(names_to = "ServerName") %>%
                                      pivot_wider(names_from = Feature,
                                                  values_from = Exists)
                              })


  # Create list of data frames (one per RDS table) containing table's feature types
  FeatureTypes <- DataSetCheck %>%
                      list_transpose() %>%
                      map(function(TableInfo)
                          {
                              TableInfo %>%
                                  map(\(ServerTableInfo) ServerTableInfo$FeatureCheckOverview %>% select(Feature, Type)) %>%
                                  list_rbind(names_to = "ServerName") %>%
                                  pivot_wider(names_from = Feature,
                                              values_from = Type)
                          })


  # Create list of data.frames (one per RDS table) containing feature-specific non-missing value counts
  NonMissingValueCounts <- DataSetCheck %>%
                              list_transpose() %>%
                              map(function(TableInfo)
                                  {
                                      TableInfo %>%
                                          map(\(ServerTableInfo) ServerTableInfo$FeatureCheckOverview %>% select(Feature, NonMissingValueCount)) %>%
                                          list_rbind(names_to = "ServerName") %>%
                                          pivot_wider(names_from = Feature,
                                                      values_from = NonMissingValueCount)
                                  })


  # Create list of data.frames (one per RDS table) containing feature-specific non-missing value rates
  NonMissingValueRates <- NonMissingValueCounts %>%
                              imap(function(TableInfo, tablename)
                                   {
                                      TableInfo %>%
                                          left_join(TableRowCounts[[tablename]], by = join_by(ServerName)) %>%      # Get row counts from 'TableCheckOverview'
                                          mutate(across(-c(ServerName, RowCount), ~ .x / RowCount)) %>%
                                          select(-RowCount)
                                   })


  # Create list of data frames (one per RDS table) containing feature-specific eligible value counts
  EligibleValueCounts <- DataSetCheck %>%
                              list_transpose() %>%
                              map(function(TableInfo)
                                  {
                                      TableInfo %>%
                                          map(\(ServerTableInfo) ServerTableInfo$FeatureCheckOverview %>% select(Feature, EligibleValueCount)) %>%
                                          list_rbind(names_to = "ServerName") %>%
                                          pivot_wider(names_from = Feature,
                                                      values_from = EligibleValueCount)
                                  })


  # Create list of data frames (one per RDS table) containing feature-specific eligible value rates
  EligibleValueRates <- EligibleValueCounts %>%
                            imap(function(TableInfo, tablename)
                                 {
                                    TableInfo %>%
                                        left_join(TableRowCounts[[tablename]], by = join_by(ServerName)) %>%      # Get row counts from 'TableCheckOverview'
                                        mutate(across(-c(ServerName, RowCount), ~ .x / RowCount)) %>%
                                        select(-RowCount)
                                 })


#-------------------------------------------------------------------------------
  return(list(TableStatus = TableStatus,
              TableRowCounts = TableRowCounts,
              FeatureExistence = FeatureExistence,
              FeatureTypes = FeatureTypes,
              NonMissingValueCounts = NonMissingValueCounts,
              NonMissingValueRates = NonMissingValueRates,
              EligibleValueCounts = EligibleValueCounts,
              EligibleValueRates = EligibleValueRates))
}
