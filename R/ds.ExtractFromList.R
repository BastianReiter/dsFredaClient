
#' ds.ExtractFromList
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Auxiliary function to trigger assignment of object extracted from a list on server
#'
#' Linked to server-side ASSIGN method ExtractFromListDS()
#'
#' @param ListName \code{string} - Name of a \code{list} object on server
#' @param ObjectName \code{string} - Name of object inside \code{list}
#' @param NewObjectName \code{string} - Optionally assigned name of object after extraction to server session - Default: \code{NULL}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.ExtractFromList <- function(ListName,
                               ObjectName,
                               AssignedObjectName = NULL,
                               DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(ListName),
              is.string(ObjectName))
  if (!is.null(NewObjectName)) { assert_that(is.string(NewObjectName)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side ASSIGN function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = ifelse(!is.null(NewObjectName),      # Per default, assign same name to object as it was in the list, but optionally assign new name
                                         NewObjectName,
                                         ObjectName),
                         value = call("ExtractFromListDS",
                                      ListName.S = ListName,
                                      ObjectName.S = ObjectName))

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(ObjectName,
                                       DSConnections = DSConnections)

#-------------------------------------------------------------------------------
  return(AssignmentInfo)
}
