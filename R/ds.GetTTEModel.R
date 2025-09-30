
#' ds.GetTTEModel
#'
#' Get Time-to-Event model
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetSurvModelDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the features of concern
#' @param TimeFeature \code{string} - Name of time feature
#' @param EventFeature \code{string} - Name of event feature
#' @param ModelType \code{string} - Function name of different TTE models implemented in \code{survival} package:
#'                                  \itemize{\item 'survfit'
#'                                           \item 'survdiff'
#'                                           \item 'coxph'}
#' @param CovariateA \code{string} - Name of optional CovariateA
#' @param CovariateB \code{string} - Name of optional CovariateB
#' @param CovariateC \code{string} - Name of optional CovariateC
#' @param MinFollowUpTime \code{integer} - Optional minimum of observed follow up time
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of Time-to-Event models
#'
#' @export
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetTTEModel <- function(TableName,
                           TimeFeature,
                           EventFeature,
                           ModelType = "survfit",
                           CovariateA = NULL,
                           CovariateB = NULL,
                           CovariateC = NULL,
                           MinFollowUpTime = 1,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)

  # --- For Testing Purposes ---
  # TableName <- "ADS_Patients"
  # TimeFeature <- "TimeFollowUp"
  # EventFeature <- "IsDocumentedDeceased"
  # ModelType <- "coxph"
  # CovariateA <- NULL
  # CovariateB <- NULL
  # CovariateC <- NULL
  # MinFollowUpTime <- 20
  # DSConnections <- CCPConnections

  # --- Argument Assertions ---
  assert_that(is.string(TableName),
              is.string(TimeFeature),
              is.string(EventFeature),
              is.string(ModelType),
              is.count(MinFollowUpTime))
  if (!is.null(CovariateA)) { assert_that(is.string(CovariateA)) }
  if (!is.null(CovariateB)) { assert_that(is.string(CovariateB)) }
  if (!is.null(CovariateC)) { assert_that(is.string(CovariateC)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check if addressed objects (Table and Feature) are eligible
#-------------------------------------------------------------------------------

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  if (is.null(TableMetaData$FirstEligible$Class)) { stop("Error: The referred table object does not seem to exist on any server.", call. = FALSE)}
  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


#-------------------------------------------------------------------------------
# Server returns
#-------------------------------------------------------------------------------

  # ServerReturns: Obtain survival model for each server calling dsCCPhos::GetSurvModelDS()
  ls_ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                              expr = call("GetTTEModelDS",
                                                          TableName.S = TableName,
                                                          TimeFeature.S = TimeFeature,
                                                          EventFeature.S = EventFeature,
                                                          ModelType.S = ModelType,
                                                          CovariateA.S = CovariateA,
                                                          CovariateB.S = CovariateB,
                                                          CovariateC.S = CovariateC,
                                                          MinFollowUpTime.S = MinFollowUpTime))

#-------------------------------------------------------------------------------
# Cumulation
#-------------------------------------------------------------------------------

  #ServerNames <- names(DSConnections)


#-------------------------------------------------------------------------------
  return(ls_ServerReturns)
}
