
#' ds.GetTableCheck
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Check out a table (\code{data.frame} or \code{tibble}) on servers and return informative meta data.
#'
#' Linked to server-side AGGREGATE method \code{GetTableCheckDS()}
#'
#' @param TableName \code{string} - Name of \code{data.frame} or \code{tibble}
#' @param RequiredFeatureNames \code{character vector} - Optional names of required features - Default: \code{names()} applied to Table evaluated from \code{TableName}
#' @param GetTemplate \code{logical} - If set to \code{TRUE}, the function returns a template incorporating required feature names without actually checking an existing table
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment. Default: \code{FALSE}
#'
#' @return A \code{list} containing compiled meta data about table:
#'         \itemize{\item TableCheckOverview
#'                  \item FeatureExistence
#'                  \item FeatureTypes
#'                  \item NonMissingValueCounts
#'                  \item NonMissingValueRates }
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetTableCheck <- function(TableName,
                             RequiredFeatureNames = NULL,
                             GetTemplate = FALSE,
                             DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringr)
  require(tidyr)

  # --- For Testing Purposes ---
  # TableName <- "CDS_Staging"
  # RequiredFeatureNames <- filter(dsCCPhos::Meta_Features, TableName_Curated == "Staging")$FeatureName_Curated
  # GetTemplate <- FALSE
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check argument eligibility
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!(is.character(TableName)))
  {
      stop("Error: Argument 'TableName' must be a character string.", call. = FALSE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Server function call to get list of lists
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  TableCheck <- DSI::datashield.aggregate(conns = DSConnections,
                                          expr = call("GetTableCheckDS",
                                                      TableName.S = TableName,
                                                      RequiredFeatureNames.S = RequiredFeatureNames,
                                                      GetTemplate.S = GetTemplate))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transform into cumulative report objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Transpose list for easier handling
  TableCheck <- TableCheck %>%
                    list_transpose()


  # Create overview data.frame containing whole-table parameters
  TableCheckOverview <- tibble(ServerName = names(DSConnections),
                               TableExists = TableCheck$TableExists,
                               TableComplete = TableCheck$TableComplete,
                               RowCount = TableCheck$RowCount) %>%
                            rowwise() %>%
                                mutate(MissingFeatures = paste0(TableCheck$MissingFeatures[[ServerName]], collapse = ", ")) %>%
                            ungroup()


  # Summarize server-specific 'FeatureCheck' tables for an additional cumulative table
  FeatureCheckOverview.All <- TableCheck$FeatureCheckOverview %>%
                                  list_rbind() %>%
                                  group_by(Feature) %>%
                                      summarize(ServerName = "All",
                                                Exists = case_when(all(Exists == TRUE) ~ TRUE,
                                                                   .default = FALSE),
                                                Exists.Info = case_when(all(Exists == TRUE) ~ "Uniform (TRUE)",
                                                                        all(Exists == FALSE) ~ "Uniform (FALSE)",
                                                                        .default = "Varied"),
                                                Type = case_when(length(unique(Type)) == 1 ~ unique(Type),
                                                                 .default = "Varied"),
                                                Type.Info = case_when(length(unique(Type)) == 1 ~ paste0("Uniform (", unique(Type), ")"),
                                                                      .default = "Varied"),
                                                NonMissingValueCount.Sum = sum(NonMissingValueCount, na.rm = TRUE),
                                                NonMissingValueRate = ifelse(!is.na(NonMissingValueCount.Sum) & NonMissingValueCount.Sum > 0,
                                                                             sum(NonMissingValueCount * NonMissingValueRate, na.rm = TRUE) / NonMissingValueCount.Sum,
                                                                             0)) %>%
                                  ungroup() %>%
                                  rename(NonMissingValueCount = "NonMissingValueCount.Sum")

  # Add cumulative table to list of server-specific 'FeatureCheck' tables
  FeatureCheckOverview <- c(list(All = FeatureCheckOverview.All),
                            TableCheck$FeatureCheckOverview)


  # Create additional tabular views of different feature-specific aspects
  FeatureExistence <- FeatureCheckOverview %>%
                          map(\(FeatureCheckTable) FeatureCheckTable %>% select(Feature, Exists)) %>%
                          list_rbind(names_to = "ServerName") %>%
                          pivot_wider(names_from = Feature,
                                      values_from = Exists)

  FeatureTypes <- FeatureCheckOverview %>%
                      map(\(FeatureCheckTable) FeatureCheckTable %>% select(Feature, Type)) %>%
                      list_rbind(names_to = "ServerName") %>%
                      pivot_wider(names_from = Feature,
                                  values_from = Type)

  NonMissingValueCounts <- FeatureCheckOverview %>%
                              map(\(FeatureCheckTable) FeatureCheckTable %>% select(Feature, NonMissingValueCount)) %>%
                              list_rbind(names_to = "ServerName") %>%
                              pivot_wider(names_from = Feature,
                                          values_from = NonMissingValueCount)

  NonMissingValueRates <- FeatureCheckOverview %>%
                              map(\(FeatureCheckTable) FeatureCheckTable %>% select(Feature, NonMissingValueRate)) %>%
                              list_rbind(names_to = "ServerName") %>%
                              pivot_wider(names_from = Feature,
                                          values_from = NonMissingValueRate)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return statement
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(TableCheckOverview = TableCheckOverview,
              FeatureCheckOverview = FeatureCheckOverview,
              FeatureExistence = FeatureExistence,
              FeatureTypes = FeatureTypes,
              NonMissingValueCounts = NonMissingValueCounts,
              NonMissingValueRates = NonMissingValueRates))
}
