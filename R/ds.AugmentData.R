
#' ds.AugmentData
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Transforms curated CCP core data set (CDM) into augmented data set (ADM)
#'
#' Linked to server-side ASSIGN method \code{AugmentDataDS()}
#'
#' @param CuratedDataSetName \code{string} - Name of the Curated Data Set object on server - Default: 'CuratedDataSet'
#' @param OutputName \code{string} - Name of output object to be assigned on server - Default: 'AugmentationOutput'
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param UnpackAugmentedDataSet \code{logical} indicating whether the Augmented Data Set \code{list} should be unpacked so that tables \code{data.frames} are directly accessible - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.AugmentData <- function(CuratedDataSetName = "CuratedDataSet",
                           OutputName = "AugmentationOutput",
                           RunAssignmentChecks = TRUE,
                           UnpackAugmentedDataSet = TRUE,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)

  #--- For testing purposes ---
  # CuratedDataSetName <- "CuratedDataSet"
  # OutputName <- "AugmentationOutput"
  # RunAssignmentChecks <- TRUE
  # UnpackAugmentedDataSet <- TRUE
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- list() }
  Messages$AugmentationCompletion <- list()


  # 1) Trigger dsCCPhos::AugmentDataDS()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Execute the server-side function call
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("AugmentDataDS",
                                      CuratedDataSetName.S = CuratedDataSetName))

  if (RunAssignmentChecks == TRUE)
  {
      # Call helper function to check if assignment of AugmentationOutput succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = OutputName,
                                                  DSConnections = DSConnections))
  }



  # 2) Extract objects from list returned by AugmentDataDS() and assign them to R server sessions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  AugmentationOutputObjects <- c("AugmentedDataSet",
                                 "AugmentationReport",
                                 "AugmentationMessages")

  for(i in 1:length(AugmentationOutputObjects))
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = AugmentationOutputObjects[i],
                             value = call("ExtractFromListDS",
                                          ListName.S = OutputName,
                                          ObjectName.S = AugmentationOutputObjects[i]))

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = AugmentationOutputObjects[i],
                                                      DSConnections = DSConnections))
      }
  }

  # Optionally unpack (unlist) AugmentedDataSet
  if (UnpackAugmentedDataSet == TRUE)
  {
      # Define ADS table names
      CCPTableNames_ADS <- c("Patient", "Diagnosis", "Therapy", "DiseaseCourse", "Events")

      for(i in 1:length(CCPTableNames_ADS))
      {
          # Execute server-side assign function
          DSI::datashield.assign(conns = DSConnections,
                                 symbol = paste0("ADS_", CCPTableNames_ADS[i]),      # E.g. 'ADS_Events'
                                 value = call("ExtractFromListDS",
                                              ListName.S = "AugmentedDataSet",
                                              ObjectName.S = CCPTableNames_ADS[i]))

          if (RunAssignmentChecks == TRUE)
          {
              # Call helper function to check if object assignment succeeded
              Messages$Assignment <- c(Messages$Assignment,
                                       ds.GetObjectStatus(ObjectName = paste0("ADS_", CCPTableNames_ADS[i]),
                                                          DSConnections = DSConnections))
          }
      }
  }

  if (RunAssignmentChecks == TRUE)
  {
      # Turn list into (named) vector
      Messages$Assignment <- purrr::list_c(Messages$Assignment)

      # Add topic element to start of vector
      Messages$Assignment <- c(Topic = "Object assignment on servers",
                               Messages$Assignment)
  }


  # 3) Get AugmentationMessages objects from servers (as a list of lists) and create completion check object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  AugmentationMessages <- DSI::datashield.aggregate(conns = DSConnections,
                                                    expr = call("GetReportingObjectDS",
                                                                ObjectName.S = "AugmentationMessages"))

  # Create table object for output
  AugmentationCompletionCheck <- AugmentationMessages %>%
                                      map(\(ServerMessages) tibble(CheckAugmentationCompletion = ServerMessages$CheckAugmentationCompletion) ) %>%
                                      list_rbind(names_to = "ServerName")

  # Create vector of messages informing about Augmentation completion
  Messages$AugmentationCompletion <- AugmentationMessages %>%
                                          imap(function(ServerMessages, servername)
                                               {
                                                  case_when(ServerMessages$CheckAugmentationCompletion == "green" ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' performed successfully!"),
                                                                                                                                 IsClassSuccess = TRUE),
                                                            ServerMessages$CheckAugmentationCompletion == "yellow" ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' performed with warnings!"),
                                                                                                                                  IsClassWarning = TRUE),
                                                            ServerMessages$CheckAugmentationCompletion == "red" ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' could not be performed!"),
                                                                                                                               IsClassFailure = TRUE),
                                                            TRUE ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' could not be assessed."),
                                                                                       IsClassFailure = TRUE))
                                               }) %>%
                                          list_c()

  # Add topic element to start of vector
  Messages$AugmentationCompletion <- c(Topic = "Augmentation process completion",
                                       Messages$AugmentationCompletion)



  # Print messages and return Messages object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  PrintMessages(Messages)

  # Invisibly return Messages and Augmentation completion check object
  invisible(list(Messages = Messages,
                 AugmentationCompletionCheck = AugmentationCompletionCheck))
}
