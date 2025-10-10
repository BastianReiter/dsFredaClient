
#' ds.GetObjectStatus
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Checks if an object exists on every server in a valid form and returns appropriate messages.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetObjectStatusDS()}
#'
#' @param ObjectName \code{string} - Name of object on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return \code{list} of messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetObjectStatus <- function(ObjectName,
                               DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ObjectName <- "RDS_GeneralCondition"
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(ObjectName))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Call GetObjectStatusDS() on every server
#-------------------------------------------------------------------------------

  ObjectStatus <- DSI::datashield.aggregate(conns = DSConnections,
                                            expr = call("GetObjectStatusDS",
                                                        ObjectName.S = ObjectName))


#-------------------------------------------------------------------------------
# Inspect object status info and return message
#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  Messages$ObjectExistence <- character()
  Messages$ObjectValidity <- character()


  CountSources <- length(ObjectStatus)

  ObjectExistsEverywhere <- TRUE
  ObjectNotNullEverywhere <- TRUE

  for (i in 1:CountSources)
  {
  	  if (!ObjectStatus[[i]]$ObjectExists)
  	  {
  		    ObjectExistsEverywhere <- FALSE
  		}
  	  if (is.null(ObjectStatus[[i]]$ObjectClass) || ("ABSENT" %in% ObjectStatus[[i]]$ObjectClass))
  	  {
  		    ObjectNotNullEverywhere <- FALSE
  		}
  }

  # Return message in case non-null object has been created on all servers
  MessageExistence <- MakeFunctionMessage(Text = paste0("The object '", ObjectName, "' has been created on all specified servers."),
                                          IsClassSuccess = TRUE)

  # ...and in case object creation did not succeed on all servers
  if (!(ObjectExistsEverywhere && ObjectNotNullEverywhere))
  {
      MessageExistence <- MakeFunctionMessage(Text = paste0("Error: A valid data object '", ObjectName, "' does NOT exist on ALL specified servers. ",
                                                            "It is either ABSENT and/or has no valid content/class, see return.info above. ",
                                                            "Please use ds.ls() to identify servers where the object is missing."),
                                              IsClassWarning = TRUE)
  }

  # Add message to list
  Messages$ObjectExistence <- MessageExistence


#-------------------------------------------------------------------------------
# Look for messages linked to object on server to check for possible errors
#-------------------------------------------------------------------------------

  # Call dsBase::messageDS() to get possible server-side message about object
  ServerCall <- call("messageDS", ObjectName)

  ServerMessage <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = ServerCall)

  NoErrors <- TRUE

  for (i in 1:CountSources)
  {
      if (ServerMessage[[i]] != "ALL OK: there are no studysideMessage(s) on this datasource")      # This is a string defined in dsBase::messageDS()
      {
  		    NoErrors <- FALSE
  		}
  }

  if (NoErrors == TRUE)
  {
      ValidityMessage <- MakeFunctionMessage(Text = paste0("'", ObjectName, "' appears valid on all servers."),
                                             IsClassSuccess = TRUE)

      Messages$ObjectValidity <- ValidityMessage

      return(Messages)
  }

  if (NoErrors == FALSE)
  {
  	  ValidityMessage <- MakeFunctionMessage(Text = paste0("'", ObjectName, "' seems to be invalid in at least one source."),
  	                                         IsClassWarning = TRUE)

  	  Messages$ObjectValidity <- ValidityMessage

  	  Messages$ServerMessage <- ServerMessage

  	  return(Messages)
  }
}
