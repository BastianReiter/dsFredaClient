
#' ds.GetFrequencyTable
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Get table of absolute and relative value frequencies for a nominal / ordinal feature.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetFrequencyTableDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param GroupingFeatureName \code{string} - Name of optional grouping feature from the same table
#' @param MaxNumberCategories \code{integer} - Maximum number of categories analyzed individually before frequencies are cumulated in 'Other' category. - Default: 10
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing:
#'         \itemize{\item AbsoluteFrequencies (\code{tibble}: Absolute value frequencies)
#'                  \item RelativeFrequencies (\code{tibble}: Relative value frequencies)}
#' @export
#'
#' @author Bastian Reiter
ds.GetFrequencyTable <- function(TableName,
                                 FeatureName,
                                 GroupingFeatureName = NULL,
                                 MaxNumberCategories = 10,
                                 DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringr)
  require(tibble)
  require(tidyr)

  # --- For Testing Purposes ---
  # TableName <- "ADS_Patients"
  # FeatureName <- "TNM_T"
  # GroupingFeatureName <- NULL
  # MaxNumberCategories <- 5
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if addressed objects (Table and Feature) are eligible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

  # Get data type of feature in question
  FeatureType <- TableMetaData$FirstEligible$DataTypes[FeatureName]

  # Stop function if referred feature is of class 'numeric' or similar
  if (FeatureType %in% c("double", "integer", "numeric")) { stop(paste0("Error: The referred feature '", FeatureName, "' is of class '", FeatureType, "' and therefore not suitable."), call. = FALSE) }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Separate returns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ServerReturns: Obtain sample statistics for each server calling dsCCPhos::GetFrequencyTableDS()
  ls_ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                              expr = call("GetFrequencyTableDS",
                                                          TableName.S = TableName,
                                                          FeatureName.S = FeatureName,
                                                          GroupingFeatureName.S = GroupingFeatureName))


  # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Cumulation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ServerNames <- names(DSConnections)

  # Convert Server returns into tibble containing separate frequency tables
  df_FrequencyTable <- ls_ServerReturns %>%
                            list_rbind(names_to = "Server") %>%
                            pivot_wider(names_from = Server,
                                        names_glue = "{Server}_{.value}",
                                        names_vary = "slowest",
                                        values_from = c(AbsoluteFrequency, RelativeFrequency)) %>%
                            mutate(All_AbsoluteFrequency = rowSums(pick(paste0(ServerNames, "_AbsoluteFrequency")), na.rm = TRUE),
                                   All_RelativeFrequency = All_AbsoluteFrequency / sum(All_AbsoluteFrequency),
                                   .after = Value) %>%
                            arrange(desc(All_AbsoluteFrequency))

  # If the number of unique values exceeds 'MaxNumberCategories', cumulate less frequent categories under 'Other' category
  if (!is.null(MaxNumberCategories))
  {
    if (nrow(df_FrequencyTable) > MaxNumberCategories)
    {
         FrequenciesKeep <- df_FrequencyTable %>%
                                slice_head(n = MaxNumberCategories)

         FrequenciesCumulate <- df_FrequencyTable %>%
                                    slice_tail(n = nrow(df_FrequencyTable) - MaxNumberCategories) %>%
                                    select(-Value) %>%
                                    colSums(na.rm = TRUE) %>%
                                    as_tibble_row() %>%
                                    mutate(Value = "Other")

         df_FrequencyTable <- bind_rows(FrequenciesKeep,
                                        FrequenciesCumulate)
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Restructuring output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Select columns containing absolute frequencies and transpose tibble using combination of pivot_longer() and pivot_wider()
  df_AbsoluteFrequencies <- df_FrequencyTable %>%
                                select(Value,
                                       contains("AbsoluteFrequency")) %>%
                                rename_with(.fn = \(colnames) str_remove(colnames, "_AbsoluteFrequency"),
                                            .cols = contains("AbsoluteFrequency")) %>%
                                pivot_longer(cols = -Value,
                                             names_to = "Server") %>%
                                pivot_wider(names_from = Value,
                                            values_from = value)

  # Select columns containing relative frequencies and transpose tibble using combination of pivot_longer() and pivot_wider()
  df_RelativeFrequencies <- df_FrequencyTable %>%
                                select(Value,
                                       contains("RelativeFrequency")) %>%
                                rename_with(.fn = \(colnames) str_remove(colnames, "_RelativeFrequency"),
                                            .cols = contains("RelativeFrequency")) %>%
                                pivot_longer(cols = -Value,
                                             names_to = "Server",
                                             values_to = "RelativeFrequency") %>%
                                pivot_wider(names_from = Value,
                                            values_from = RelativeFrequency)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return statement
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(AbsoluteFrequencies = df_AbsoluteFrequencies,
              RelativeFrequencies = df_RelativeFrequencies))
}
