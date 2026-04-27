
#' ds.CurateData
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Triggers transformation of Raw Data Set (RDS) into Curated Data Set (CDS) on servers.
#'
#' Linked to server-side ASSIGN methods \code{CurateDataDS()} and \code{ExtractFromListDS()}
#'
#' @param RawDataSetName \code{character} - Name of Raw Data Set object (list) on server - Default: 'CCP.RawDataSet'
#' @param Module \code{string} - Identifying a registered FREDA module (Examples: 'CCP' / 'P21')
#' @param OutputName \code{character} - Name of output object to be assigned on server - Default: '<Module>.CurationOutput'
#' @param Profile.CurationProcess \code{string} - "Default"
#' @param Profile.DataRemediation \code{string} - "Default"
#' @param Profile.TransformativeExpressions \code{string} - "Default"
#' @param Profile.Dictionary \code{string} - "Default"
#' @param Profile.FuzzyStringMatching \code{string} - "Default"
#' @param Profile.FeatureRequirements \code{string} - "Default"
#' @param Profile.FeatureTracking \code{string} - "Default"
#' @param Profile.PrimaryTableCleaning \code{string} - "Default"
#' @param Profile.RecordSubsumption \code{string} - "Default"
#' @param Profile.SecondaryTableCleaning \code{string} - "Default"
#' @param Profile.TableNormalization \code{string} - "Default"
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param UnpackCuratedDataSet \code{logical} indicating whether the Curated Data Set \code{list} should be unpacked so that tables \code{data.frames} are directly accessible - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{flag} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return Messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.CurateData <- function(RawDataSetName,
                          Module,
                          OutputName = paste0(Module, ".CurationOutput"),
                          Profile.CurationProcess = "Default",
                          Profile.DataRemediation = "Default",
                          Profile.Dictionary = "Default",
                          Profile.FeatureRequirements = "Default",
                          Profile.FeatureTracking = "Default",
                          Profile.FuzzyStringMatching = "Default",
                          Profile.PrimaryTableCleaning = "Default",
                          Profile.RecordSubsumption = "Default",
                          Profile.SecondaryTableCleaning = "Default",
                          Profile.TableNormalization = "Default",
                          Profile.TransformativeExpressions = "Default",
                          #--- Secondary Arguments ---
                          RunAssignmentChecks = TRUE,
                          UnpackCuratedDataSet = TRUE,
                          DSConnections = NULL,
                          DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  #--- For Testing Purposes ---
  # RawDataSetName <- "CCP.RawDataSet"
  # Module <- "CCP"
  # OutputName <- "CCP.CurationOutput"
  # Profile.CurationProcess <- "Default"
  # Profile.DataRemediation <- "Default"
  # Profile.Dictionary <- "Default"
  # Profile.FeatureRequirements <- "Default"
  # Profile.FeatureTracking <- "Default"
  # Profile.FuzzyStringMatching <- "Default"
  # Profile.PrimaryTableCleaning <- "Default"
  # Profile.RecordSubsumption <- "Default"
  # Profile.SecondaryTableCleaning <- "Default"
  # Profile.TableNormalization <- "Default"
  # Profile.TransformativeExpressions <- "Default"
  # RunAssignmentChecks <- TRUE
  # UnpackCuratedDataSet <- TRUE
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName),
              is.string(Module),
              is.string(OutputName),
              is.string(Profile.CurationProcess),
              is.string(Profile.DataRemediation),
              is.string(Profile.Dictionary),
              is.string(Profile.FeatureRequirements),
              is.string(Profile.FeatureTracking),
              is.string(Profile.FuzzyStringMatching),
              is.string(Profile.PrimaryTableCleaning),
              is.string(Profile.RecordSubsumption),
              is.string(Profile.SecondaryTableCleaning),
              is.string(Profile.TableNormalization),
              is.string(Profile.TransformativeExpressions),
              is.flag(RunAssignmentChecks),
              is.flag(UnpackCuratedDataSet),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- list() }
  Messages$Curation.Completion <- list()


# Trigger dsFreda::CurateDataDS()
#-------------------------------------------------------------------------------
  # Execute the server-side function call
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("CurateDataDS",
                                      RawDataSetName.S = RawDataSetName,
                                      Module.S = Module,
                                      Profile.CurationProcess.S = Profile.CurationProcess,
                                      Profile.DataRemediation.S = Profile.DataRemediation,
                                      Profile.Dictionary.S = Profile.Dictionary,
                                      Profile.FeatureRequirements.S = Profile.FeatureRequirements,
                                      Profile.FeatureTracking.S = Profile.FeatureTracking,
                                      Profile.FuzzyStringMatching.S = Profile.FuzzyStringMatching,
                                      Profile.PrimaryTableCleaning.S = Profile.PrimaryTableCleaning,
                                      Profile.RecordSubsumption.S = Profile.RecordSubsumption,
                                      Profile.SecondaryTableCleaning.S = Profile.SecondaryTableCleaning,
                                      Profile.TableNormalization.S = Profile.TableNormalization,
                                      Profile.TransformativeExpressions.S = Profile.TransformativeExpressions),
                         async = DS.async)

  if (RunAssignmentChecks == TRUE)
  {
      # Call helper function to check if assignment of CurationOutput succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = OutputName,
                                                  DSConnections = DSConnections))
  }


# Extract objects from list returned by CurateDataDS() and assign them to R server sessions
#-------------------------------------------------------------------------------

  # Named vector determining how objects inside CurationOutput list created on servers should be extracted
  ObjectNames <- setNames(c(paste0(Module, c(".CuratedDataSet",
                                             ".NonconformingRecords",
                                             ".AffectedRootSubjects",
                                             ".CurationReport")),
                            "Messages"),
                          nm = c("DataSet",
                                 "NonconformingRecords",
                                 "AffectedRootSubjects",
                                 "Report",
                                 "Messages"))

  # Extract objects from CurationOutput list
  for (i in 1:length(ObjectNames))
  {
      # Execute server-side list extraction
      DSI::datashield.assign(conns = DSConnections,
                             symbol = unname(ObjectNames[i]),
                             value = call("ExtractFromListDS",
                                           ListName.S = OutputName,
                                           ObjectName.S = names(ObjectNames)[i]),
                             async = DS.async)

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = unname(ObjectNames[i]),
                                                      DSConnections = DSConnections,
                                                      DS.async = DS.async))
      }
  }


# Optionally unpack (unlist) CuratedDataSet
#-------------------------------------------------------------------------------

  if (UnpackCuratedDataSet == TRUE)
  {
      # Get correct package name from 'Meta.Modules'
      ModulePackageName <- dsFredaClient::Meta.Modules[[Module]]

      # Get curated table names
      CuratedTableNames <- eval(parse(text = paste0(ModulePackageName, "::Meta.Tables$TableName.Curated")))

      for(i in 1:length(CuratedTableNames))
      {
          # Execute server-side assign function
          DSI::datashield.assign(conns = DSConnections,
                                 symbol = paste0(Module, ".CDS.", CuratedTableNames[i]),      # E.g. 'CCP.CDS.Metastasis'
                                 value = call("ExtractFromListDS",
                                              ListName.S = paste0(Module, ".CuratedDataSet"),
                                              ObjectName.S = CuratedTableNames[i]),
                                 async = DS.async)

          if (RunAssignmentChecks == TRUE)
          {
              # Call helper function to check if object assignment succeeded
              Messages$Assignment <- c(Messages$Assignment,
                                       ds.GetObjectStatus(ObjectName = paste0(Module, ".CDS.", CuratedTableNames[i]),
                                                          DSConnections = DSConnections,
                                                          DS.async = DS.async))
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



# Get Messages object from servers (as a list of lists) and create completion check object
#-------------------------------------------------------------------------------

  CurationMessages <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetReportingObjectDS",
                                                            ObjectName.S = "Messages"),
                                                async = DS.async)

  # Create table object for output
  Curation.Completion <- CurationMessages %>%
                              map(\(ServerMessages) tibble(Curation.CompletionCheck = ServerMessages$Curation.CompletionCheck,
                                                           Curation.Duration = ServerMessages$Curation.Duration )) %>%
                              list_rbind(names_to = "ServerName")

  # Create vector of messages informing about curation completion
  Messages$Curation.Completion <- CurationMessages %>%
                                      imap(function(ServerMessages, servername)
                                           {
                                              tibble(CompletionCheck = ServerMessages$Curation.CompletionCheck,
                                                     Duration = ServerMessages$Curation.Duration) %>%
                                                  mutate(Message = case_when(CompletionCheck == "green" ~ paste0("Curation on server '", servername, "' performed successfully! (Duration: ", Duration, ")"),
                                                                             CompletionCheck == "yellow" ~ paste0("Curation on server '", servername, "' performed with warnings! \n", ServerMessages$FinalMessage),
                                                                             CompletionCheck == "red" ~ paste0("Curation on server '", servername, "' could not be performed! \n", ServerMessages$FinalMessage),
                                                                             .default = paste0("Curation on server '", servername, "' could not be assessed. \n", ServerMessages$FinalMessage)),
                                                         MessageClass = case_when(CompletionCheck == "green" ~ "Success",
                                                                                  CompletionCheck == "yellow" ~ "Warning",
                                                                                  CompletionCheck == "red" ~ "Failure",
                                                                                  .default = "Info")) %>%
                                                  select(MessageClass,
                                                         Message) %>%
                                                  tibble::deframe()
                                           }) %>%
                                      list_c()

  # Add topic element to start of vector
  Messages$Curation.Completion <- c(Topic = "Curation process completion",
                                    Messages$Curation.Completion)


# Print messages and return output
#-------------------------------------------------------------------------------

  # Print messages on console
  PrintMessages(Messages)

  # Invisibly return Messages and Curation completion object
  invisible(list(Messages = Messages,
                 Curation.Completion = Curation.Completion))
}
