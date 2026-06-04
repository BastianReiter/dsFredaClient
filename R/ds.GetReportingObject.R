
#' ds.GetReportingObject
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Receives a reporting object from servers. Its name must be on a list of permitted object names to ensure data privacy.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetReportingObjectDS()}
#'
#' @param ObjectName \code{string} - Name of reporting object on server. Must be on a list of permitted object names.
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of reporting objects
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetReportingObject <- function(ObjectName,
                                  DSConnections = NULL,
                                  DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(ObjectName),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  ReportingObjects <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetReportingObjectDS",
                                                            ObjectName.S = ObjectName),
                                                async = DS.async)

#-------------------------------------------------------------------------------
  return(ReportingObjects)
}
