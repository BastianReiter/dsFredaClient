
#' ds.UnpackAugmentedDataSet
#'
#' Make tables within Augmented Data Set (\code{list} object) directly addressable in R server sessions.
#'
#' Linked to server-side \code{ASSIGN} method \code{ExtractFromListDS()}
#'
#' @param AugmentedDataSetName \code{string} - Name of Augmented Data Set object (\code{list}) on server - Default: 'AugmentedDataSet'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.UnpackAugmentedDataSet <- function(AugmentedDataSetName = "AugmentedDataSet",
                                      DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # AugmentedDataSetName <- "AugmentedDataSet"
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initiate output messaging objects
  Messages <- list()
  Messages$Assignment <- list()

  # Define ADS table names
  CCPTableNames_ADS <- c("Patient", "Diagnosis", "Therapy", "DiseaseCourse", "Events")

  for(i in 1:length(CCPTableNames_ADS))
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0("ADS_", CCPTableNames_ADS[i]),      # E.g. 'ADS_Events'
                             value = call("ExtractFromListDS",
                                          ListName.S = AugmentedDataSetName,
                                          ObjectName.S = CCPTableNames_ADS[i]))

      # Call helper function to check if object assignment succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = paste0("ADS_", CCPTableNames_ADS[i]),
                                                  DSConnections = DSConnections))
  }

  # Turn list into (named) vector
  Messages$Assignment <- purrr::list_c(Messages$Assignment)

  # Add topic element to start of vector
  Messages$Assignment <- c(Topic = "Object assignment on servers",
                           Messages$Assignment)


  # Print messages and return Messages object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  PrintMessages(Messages)

  # Return Messages invisibly
  invisible(Messages)
}
