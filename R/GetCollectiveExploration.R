
#' GetCollectiveExploration
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#'
#'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing overview and details of server-side workspace objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetCollectiveExploration <- function(OrderList = NULL,
                                     InputWorkspaceInfo = NULL,
                                     DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # OrderList <- list(ADS.Diagnosis = c("Grading",
  #                                     "PatientAgeAtDiagnosis",
  #                                     "TNM.T"))
  # InputWorkspaceInfo <- ServerWorkspaceInfo
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  if (!is.null(OrderList)) { assert_that(is.list(OrderList)) }
  if (!is.null(InputWorkspaceInfo)) { assert_that(is.list(InputWorkspaceInfo)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------


  if (!is.null(InputWorkspaceInfo) && is.null(OrderList))
  {
      SuitableTables <- InputWorkspaceInfo$Overview$All %>%
                            filter(Exists == TRUE,
                                   Exists.Info == "Uniform",
                                   Class == "data.frame",
                                   Class.Info == "Uniform") %>%
                            pull(Object)

      OrderList <- InputWorkspaceInfo$ObjectDetails$All[SuitableTables] %>%
                        imap()

  }


  Exploration <- OrderList %>%
                      imap(function(FeatureNames, tablename)
                           {
                              FeatureNames %>%
                                  map(\(featurename) dsFredaClient::ExploreFeature(TableName = tablename,
                                                                                   FeatureName = featurename,
                                                                                   DSConnections = DSConnections)) %>%
                                  set_names(FeatureNames)
                           })

#-------------------------------------------------------------------------------
  return(Exploration)
}
