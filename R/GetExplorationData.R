
#' GetExplorationData
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
GetExplorationData <- function(OrderList = NULL,
                               InputWorkspaceInfo = NULL,
                               TableSelection = NULL,
                               DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # OrderList <- list(ADS.Diagnosis = c("Grading",
  #                                     "PatientAgeAtDiagnosis",
  #                                     "TNM.T"))
  # OrderList <- NULL
  # InputWorkspaceInfo <- ServerWorkspaceInfo
  # TableSelection <- c("CCP.ADS.Diagnosis",
  #                     "CCP.ADS.DiseaseCourse",
  #                     "CCP.ADS.Events",
  #                     "CCP.ADS.Patient",
  #                     "CCP.ADS.Therapy")
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  if (!is.null(OrderList)) { assert_that(is.list(OrderList)) }
  if (!is.null(InputWorkspaceInfo)) { assert_that(is.list(InputWorkspaceInfo)) }
  if (!is.null(TableSelection)) { assert_that(is.character(TableSelection)) }

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

      if (!is.null(TableSelection)) { SuitableTables <- SuitableTables[SuitableTables %in% TableSelection] }

      OrderList <- InputWorkspaceInfo$ObjectDetails$All[SuitableTables] %>%
                        imap(function(TableInfo, tablename)
                             {
                                TableInfo %>%
                                    filter(Type %in% c("character", "logical", "integer", "numeric", "double")) %>%
                                    pull(Feature)
                             })
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
