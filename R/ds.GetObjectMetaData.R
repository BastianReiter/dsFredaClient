
#' ds.GetObjectMetaData
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Gathers meta data about an R object.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetObjectMetaDataDS()}
#'
#' @param ObjectName \code{string} - Name of object on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of server returns
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetObjectMetaData <- function(ObjectName,
                                 DSConnections = NULL,
                                 DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ObjectName <- "RDS_Diagnosis"
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(ObjectName),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get object meta data from every server
  ObjectMetaData <- DSI::datashield.aggregate(conns = DSConnections,
                                              expr = call("GetObjectMetaDataDS",
                                                          ObjectName.S = ObjectName),
                                              async = DS.async)

  # Get logical vector indicating existence of object on servers
  ObjectExistence <- ObjectMetaData %>%
                          map_lgl(\(metadatalist) metadatalist$ObjectExists)

  # Get names of all servers that host the object (so everywhere it exists)
  EligibleServers <- names(DSConnections)[ObjectExistence]

  # Add to output list: Meta data from any (first eligible) server that hosts the object in question
  ObjectMetaData$FirstEligible <- if(!is.null(EligibleServers)) { ObjectMetaData[[first(EligibleServers)]] } else { NULL }

#-------------------------------------------------------------------------------
  return(ObjectMetaData)
}
