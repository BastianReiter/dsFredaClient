
#' ds.GetRDSValidationReport
#'
#' Triggers validation of Raw Data Set on server and requests a report.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetRDSValidationReportDS()}
#'
#' @param RawDataSetName \code{string} - Name of 'RawDataSet' object on server - Default: 'RawDataSet'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of \code{tibbles} containing output of validation
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetRDSValidationReport <- function(RawDataSetName = "RawDataSet",
                                      DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ServerCall <- call("GetRDSValidationReportDS",
                     RawDataSetName.S = RawDataSetName)

  ValidationReport <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = ServerCall)

  return(ValidationReport)
}
