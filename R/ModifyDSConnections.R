
#' ModifyDSConnections
#'
#' Modifies given \code{list} of \code{DSConnection} objects created by DataSHIELD functionality.
#'
#' @param ServersToBeRemoved \code{character vector} of server names to be removed from \code{DSConnections}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A (modified) \code{list} of \code{DSConnection} objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ModifyConnections <- function(ServersToBeRemoved = NULL,
                              DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Remove DSConnection list elements according to 'ServersToBeRemoved' vector
  ModifiedDSConnections <- DSConnections[names(DSConnections) %in% ServersToBeRemoved == FALSE]

  # Return DSConnection objects
  return(ModifiedDSConnections)
}
