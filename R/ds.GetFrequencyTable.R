
#' ds.GetFrequencyTable
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Get table of absolute and relative value frequencies for a nominal / ordinal feature.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetFrequencyTableDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param GroupingFeatureName \code{string} - Name of optional grouping feature from the same table
#' @param MaxNumberCategories \code{integer} - Maximum number of categories analyzed individually before frequencies are cumulated in 'Other' category. - Default: 10
#' @param RemoveNA \code{logical} - Indicating whether missing values should be removed prior to frequency calculation - Default: \code{FALSE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing:
#'         \itemize{\item AbsoluteFrequencies (\code{tibble}: Absolute value frequencies)
#'                  \item RelativeFrequencies (\code{tibble}: Relative value frequencies)}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetFrequencyTable <- function(TableName,
                                 FeatureName,
                                 GroupingFeatureName = NULL,
                                 MaxNumberCategories = 10,
                                 RemoveNA = FALSE,
                                 DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName <- "CCP.ADS.Diagnosis"
  # FeatureName <- "TNM.T"
  # GroupingFeatureName <- "TNM.M"
  # MaxNumberCategories <- 10
  # RemoveNA <- FALSE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(FeatureName),
              is.count(MaxNumberCategories),
              is.flag(RemoveNA))
  if (!is.null(GroupingFeatureName)) { assert_that(is.string(GroupingFeatureName))
                                       assert_that(GroupingFeatureName != FeatureName,
                                                   msg = "Values for 'GroupingFeatureName' and 'FeatureName' can not be identical.") }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check if addressed objects (Table and Feature) are eligible
#-------------------------------------------------------------------------------

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

  # Get data type of feature in question
  FeatureType <- TableMetaData$FirstEligible$DataTypes[FeatureName]

  # Stop function if referred feature is of class 'numeric' or similar
  if (FeatureType %in% c("double", "integer", "numeric")) { stop(paste0("Error: The referred feature '", FeatureName, "' is of class '", FeatureType, "' and therefore not suitable."), call. = FALSE) }


#-------------------------------------------------------------------------------
# Separate Server-specific returns
#-------------------------------------------------------------------------------

  # Obtain sample statistics for each server calling dsFreda::GetFrequencyTableDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetFrequencyTableDS",
                                                         TableName.S = TableName,
                                                         FeatureName.S = FeatureName,
                                                         GroupingFeatureName.S = GroupingFeatureName,
                                                         RemoveNA.S = RemoveNA))


  # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


#-------------------------------------------------------------------------------
# Cumulation
#-------------------------------------------------------------------------------

  ServerNames <- names(DSConnections)

  # Convert Server returns into tibble containing separate frequency tables
  FrequencyTable <- ServerReturns %>%
                        list_rbind(names_to = "Server")

  # Stop function and return NULL if there are only empty server returns
  if (length(FrequencyTable) == 0 || nrow(FrequencyTable) == 0) { return(NULL) }

  # ...else continue
  FrequencyTable <- FrequencyTable %>%
                        pivot_wider(names_from = Server,
                                    names_glue = "{Server}.{.value}",
                                    names_vary = "slowest",
                                    values_from = c(AbsoluteFrequency, RelativeFrequency)) %>%
                        mutate(All.AbsoluteFrequency = rowSums(pick(ends_with(".AbsoluteFrequency")), na.rm = TRUE),
                               All.RelativeFrequency = All.AbsoluteFrequency / sum(All.AbsoluteFrequency)) %>%
                        arrange(desc(All.AbsoluteFrequency))

  # If the number of unique values exceeds 'MaxNumberCategories', cumulate less frequent categories under 'Other' category
  if (!is.null(MaxNumberCategories))
  {
      CountCategories <- FrequencyTable %>%
                              { if (!is.null(GroupingFeatureName)) { group_by(., !!sym(GroupingFeatureName)) } else {.} } %>%
                              summarize(NumberCategories = n())

      if (any(CountCategories$NumberCategories > MaxNumberCategories))
      {
           FrequenciesKeep <- FrequencyTable %>%
                                  { if (!is.null(GroupingFeatureName)) { group_by(., !!sym(GroupingFeatureName)) } else {.} } %>%
                                  slice_head(n = MaxNumberCategories)

           FrequenciesCumulate <- FrequencyTable %>%
                                      { if (!is.null(GroupingFeatureName)) { group_by(., !!sym(GroupingFeatureName)) } else {.} } %>%
                                      slice((MaxNumberCategories + 1):max((MaxNumberCategories + 1), CountCategories$NumberCategories[cur_group_id()])) %>%
                                      summarize(!!sym(FeatureName) := "Other",
                                                across(ends_with("Frequency"), ~ sum(.x, na.rm = TRUE)))     # valid for both 'Absolute' and 'Relative' frequencies

           FrequencyTable <- bind_rows(FrequenciesKeep,
                                       FrequenciesCumulate)
      }
  }


#-------------------------------------------------------------------------------
# Restructuring output
#-------------------------------------------------------------------------------

  # Select columns containing absolute frequencies and transpose tibble using combination of pivot_longer() and pivot_wider()
  AbsoluteFrequencies <- FrequencyTable %>%
                              select({{ GroupingFeatureName }},
                                     {{ FeatureName }},
                                     contains("AbsoluteFrequency")) %>%
                              rename_with(.fn = \(colnames) str_remove(colnames, ".AbsoluteFrequency"),
                                          .cols = contains("AbsoluteFrequency")) %>%
                              pivot_longer(cols = -c({{ GroupingFeatureName }},
                                                     {{ FeatureName }}),
                                           names_to = "Server",
                                           values_to = "AbsoluteFrequency") %>%
                              pivot_wider(names_from = {{ FeatureName }},
                                          values_from = AbsoluteFrequency)

  # Select columns containing relative frequencies and transpose tibble using combination of pivot_longer() and pivot_wider()
  RelativeFrequencies <- FrequencyTable %>%
                              select({{ GroupingFeatureName }},
                                     {{ FeatureName }},
                                     contains("RelativeFrequency")) %>%
                              rename_with(.fn = \(colnames) str_remove(colnames, ".RelativeFrequency"),
                                          .cols = contains("RelativeFrequency")) %>%
                              pivot_longer(cols = -c({{ GroupingFeatureName }},
                                                     {{ FeatureName }}),
                                           names_to = "Server",
                                           values_to = "RelativeFrequency") %>%
                              pivot_wider(names_from = {{ FeatureName }},
                                          values_from = RelativeFrequency)


#-------------------------------------------------------------------------------
  return(list(AbsoluteFrequencies = AbsoluteFrequencies,
              RelativeFrequencies = RelativeFrequencies))
}
