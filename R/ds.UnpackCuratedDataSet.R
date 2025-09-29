
#' ds.UnpackCuratedDataSet
#'
#' Make tables within Curated Data Set (\code{list} object) directly addressable in R server sessions
#'
#' Linked to server-side \code{ASSIGN} function \code{ExtractFromListDS()}
#'
#' @param CuratedDataSetName \code{string} - Name of Curated Data Set object (list) on server - Default: 'CuratedDataSet'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.UnpackCuratedDataSet <- function(CuratedDataSetName = "CuratedDataSet",
                                    DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  Messages$Assignment <- list()

  # Get curated CCP table names
  CCPTableNames_CDS <- dsCCPhosClient::Meta_Tables$TableName_Curated

  for(i in 1:length(CCPTableNames_CDS))
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0("CDS_", CCPTableNames_CDS[i]),      # E.g. 'CDS_Metastasis'
                             value = call("ExtractFromListDS",
                                          ListName.S = CuratedDataSetName,
                                          ObjectName.S = CCPTableNames_CDS[i]))

      # Call helper function to check if object assignment succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = paste0("CDS_", CCPTableNames_CDS[i]),
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
