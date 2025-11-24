
#' ds.CreateSurvObject
#'
#' Create a \code{survival::Surv()} object on servers
#'
#' Linked to server-side \code{ASSIGN} function \code{CreateSurvObjectDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the features of concern
#' @param TimeFeature \code{string} - Name of time feature
#' @param EventFeature \code{string} - Name of event feature
#' @param MinFollowUpTime \code{integer} - Optional minimum of observed follow up time
#' @param OutputName \code{string} - Assigned symbol name on servers
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.CreateSurvObject <- function(TableName,
                                TimeFeature,
                                EventFeature,
                                MinFollowUpTime = 1,
                                OutputName = "SurvObject",
                                DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName <- "ADS_Patients"
  # TimeFeature <- "TimeFollowUp"
  # EventFeature <- "IsDocumentedDeceased"
  # MinFollowUpTime <- 20
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(TimeFeature),
              is.string(EventFeature),
              is.count(MinFollowUpTime),
              is.string(OutputName))

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

  ServerReturns <- DSI::datashield.assign(conns = DSConnections,
                                          symbol = OutputName,
                                          value = call("CreateSurvObjectDS",
                                                       TableName.S = TableName,
                                                       TimeFeature.S = TimeFeature,
                                                       EventFeature.S = EventFeature,
                                                       MinFollowUpTime.S = MinFollowUpTime))

#-------------------------------------------------------------------------------
  return(NULL)
}
