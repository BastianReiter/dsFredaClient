
#' GetEligibleValues
#'
#' Get a set of eligible values for a feature, if defined in meta data.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param ValuesMetaData \code{data.frame} - 'Meta.Values'
#' @param Stage \code{string} - Either 'Refined' (Default) or 'Source'. Both stages might have different sets of eligible values.
#'
#' @return \code{vector} of eligible values
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetEligibleValues <- function(TableName,
                              FeatureName,
                              ValuesMetaData,
                              Stage = "Refined")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName = "Surgery"
  # FeatureName = "Intention"
  # ValuesMetaData = dsCCPhos::Meta.Values
  # Stage = "Refined"

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(FeatureName),
              is.data.frame(ValuesMetaData),
              is.string(Stage))

#-------------------------------------------------------------------------------

  EligibleValues <- ValuesMetaData %>%
                        filter(Table == TableName,
                               Feature == FeatureName) %>%
                        { if (Stage == "Source") { pull(., Value.Source) }
                          else { pull(., Value.Refined) } }

#-------------------------------------------------------------------------------
  return(EligibleValues)
}
