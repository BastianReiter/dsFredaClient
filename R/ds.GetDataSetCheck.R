
#' ds.GetDataSetCheck
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Check out a data set (\code{list} of \code{data.frames}) on servers and return a coherent summary across servers.
#'
#' Linked to server-side AGGREGATE method \code{GetDataSetCheckDS()}
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param RequiredTableNames \code{character vector} - Names of tables that are expected/required to be in the data set - Default: Names of elements in list evaluated from \code{DataSetName.S}
#' @param RequiredFeatureNames \code{list} of \code{character vectors} - Features that are expected/required in each table of the data set - Default: Names of features in respective table
#' @param AssumeCCPDataSet \code{logical} - Whether or not the data set to be checked out is one of the main data sets used in CCPhos - Default: FALSE
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing compiled meta data about data set tables:
#'         \itemize{\item TableStatus
#'                  \item TableRowCounts
#'                  \item FeatureExistence
#'                  \item FeatureTypes
#'                  \item NonMissingValueCounts
#'                  \item NonMissingValueRates }
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetDataSetCheck <- function(DataSetName,
                               RequiredTableNames = NULL,
                               RequiredFeatureNames = NULL,
                               AssumeCCPDataSet = FALSE,
                               DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringr)
  require(tidyr)

  # --- For Testing Purposes ---
  # DataSetName <- "RawDataSet"
  # AssumeCCPDataSet <- TRUE
  # RequiredTableNames = NULL
  # RequiredFeatureNames = NULL
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check argument eligibility
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!(is.character(DataSetName)))
  {
      stop("Error: Argument 'DataSetName.S' must be a character string.", call. = FALSE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Server function call to get list of lists
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  DataSetCheck <- DSI::datashield.aggregate(conns = DSConnections,
                                            expr = call("GetDataSetCheckDS",
                                                        DataSetName.S = DataSetName,
                                                        RequiredTableNames.S = RequiredTableNames,
                                                        RequiredFeatureNames.S = RequiredFeatureNames,
                                                        AssumeCCPDataSet.S = AssumeCCPDataSet))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transform into cumulated report objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


  # Create list of data frames (one per RDS table) containing feature-specific non-missing value counts
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


  # Create list of data frames (one per RDS table) containing feature-specific non-missing value rates
  NonMissingValueRates <- NonMissingValueCounts %>%
                              imap(function(TableInfo, tablename)
                                  {
                                      TableInfo %>%
                                          left_join(TableRowCounts[[tablename]], by = join_by(ServerName)) %>%      # Get row counts from 'TableCheckOverview'
                                          mutate(across(-c(ServerName, RowCount), ~ .x / RowCount)) %>%
                                          select(-RowCount)
                                  })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return statement
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(TableStatus = TableStatus,
              TableRowCounts = TableRowCounts,
              FeatureExistence = FeatureExistence,
              FeatureTypes = FeatureTypes,
              NonMissingValueCounts = NonMissingValueCounts,
              NonMissingValueRates = NonMissingValueRates))
}
