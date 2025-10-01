
#' ds.GetReportingObject
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Receives a reporting object from servers. Its name must be on a list of permitted object names to ensure data privacy.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetReportingObjectDS()}
#'
#' @param ObjectName \code{string} - Name of reporting object on server. Must be on a list of permitted object names.
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of reporting objects
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetReportingObject <- function(ObjectName,
                                  DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)

  # --- Argument Assertions ---
  assert_that(is.string(ObjectName))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  ReportingObjects <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetReportingObjectDS",
                                                            ObjectName.S = ObjectName))

#-------------------------------------------------------------------------------
  return(ReportingObjects)
}
