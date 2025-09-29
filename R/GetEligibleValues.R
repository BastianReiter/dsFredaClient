
#' GetEligibleValues
#'
#' Get a set of eligible values for a feature, if defined in meta data.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param Stage \code{string} - Either 'Curated' (Default) or 'Raw'. Both stages might have different sets of eligible values.
#'
#' @return \code{vector} of eligible values
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetEligibleValues <- function(TableName,
                              FeatureName,
                              Stage = "Curated")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)

  # --- For Testing Purposes ---
  # TableName = "Surgery"
  # FeatureName = "Intention"
  # Stage = "Curated"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  EligibleValues <- dsCCPhosClient::Meta_Values %>%
                        filter(Table == TableName,
                               Feature == FeatureName) %>%
                        { if (Stage == "Raw") { pull(., Value_Raw) }
                          else { pull(., Value_Curated) } }

  return(EligibleValues)
}
