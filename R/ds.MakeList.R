
#' ds.MakeList
#'
#' Bundle objects in a list on servers.
#'
#' Linked to server-side \code{ASSIGN} function \code{MakeListDS()}
#'
#' @param ObjectNames \code{character} - Names of objects on server to be bundled in a list. Can be a \code{named vector} if names in list should differ from symbol names (with names in named vector being the names in list).
#' @param OutputName \code{string} - Name of resulting \code{list} on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.MakeList <- function(ObjectNames,
                        OutputName,
                        DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.character(ObjectNames),
              is.string(OutputName))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side ASSIGN function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("MakeListDS",
                                      ObjectNames.S = ObjectNames))

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(OutputName,
                                       DSConnections = DSConnections)

#-------------------------------------------------------------------------------
  return(AssignmentInfo)
}
