
#' ds.RefineData
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Triggers transformation of Source Data Set (SDS) into Refined Data Set (RDS) on servers.
#'
#' Linked to server-side ASSIGN methods \code{RefineDataDS()} and \code{ExtractFromListDS()}
#'
#' @param SourceDataSetName \code{character} - Name of Source Data Set object (list) on server - Default: '<Module>.SourceDataSet'
#' @param Module \code{string} - Identifying a registered FREDA module (Examples: 'CCP' / 'P21')
#' @param OutputName \code{character} - Name of output object to be assigned on server - Default: '<Module>.RefinementOutput'
#' @param Profile.RefinementProcess \code{string} - "Default"
#' @param Profile.DataRemediation \code{string} - "Default"
#' @param Profile.Dictionary \code{string} - "Default"
#' @param Profile.FeatureRequirements \code{string} - "Default"
#' @param Profile.FeatureTracking \code{string} - "Default"
#' @param Profile.FuzzyStringMatching \code{string} - "Default"
#' @param Profile.PrimaryTableCleaning \code{string} - "Default"
#' @param Profile.RecordSubsumption \code{string} - "Default"
#' @param Profile.SecondaryTableCleaning \code{string} - "Default"
#' @param Profile.TableNormalization \code{string} - "Default"
#' @param Profile.TransformativeExpressions \code{string} - "Default"
#' @param RunAssignmentChecks \code{logical} - Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param UnpackRefinedDataSet \code{logical} - Indicating whether the Refined Data Set \code{list} should be unpacked so that tables \code{data.frames} are directly accessible - Default: \code{TRUE}
#' @param RunSeparately \code{logical} - Indicating whether \code{RefineDataDS()} should be run separately for each server. This can be done for testing purposes or if timeout issues arise. - Default: \code{FALSE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{flag} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return Messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.RefineData <- function(SourceDataSetName = paste0(Module, ".SourceDataSet"),
                          Module,
                          OutputName = paste0(Module, ".RefinementOutput"),
                          Profile.RefinementProcess = "Default",
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
                          UnpackRefinedDataSet = TRUE,
                          RunSeparately = FALSE,
                          DSConnections = NULL,
                          DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  #--- For Testing Purposes ---
  # SourceDataSetName <- "CCP.SourceDataSet"
  # Module <- "CCP"
  # OutputName <- "CCP.RefinementOutput"
  # Profile.RefinementProcess <- "Default"
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
  # UnpackRefinedDataSet <- TRUE
  # RunSeparately <- FALSE
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(SourceDataSetName),
              is.string(Module),
              is.string(OutputName),
              is.string(Profile.RefinementProcess),
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
              is.flag(UnpackRefinedDataSet),
              is.flag(RunSeparately),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- list() }
  Messages$Refinement.Completion <- list()


# Trigger dsFreda::RefineDataDS()
#-------------------------------------------------------------------------------

  # Auxiliary function to wrap triggering of server-side function call
  TriggerRefineDataDS <- function(SelectedConnections)
  {
      DSI::datashield.assign(conns = SelectedConnections,
                             symbol = OutputName,
                             value = call("RefineDataDS",
                                          SourceDataSetName.S = SourceDataSetName,
                                          Module.S = Module,
                                          Profile.RefinementProcess.S = Profile.RefinementProcess,
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
  }

  # Trigger server-side function call separately...
  if (RunSeparately == TRUE)
  {
      for (i in 1:length(DSConnections))
      {
          # First, use dsBaseClient::ds.exists("PING") to ping ALL servers and effectively keep them idle before triggering heavy work at ONE server. This may reduce risk of running into a server timeout issue.
          .Ping <- dsBaseClient::ds.exists("PING")
          # Then trigger heavy work at ONE server
          TriggerRefineDataDS(DSConnections[i])
      }

  # ... or trigger normal DSI call with complete list of connections (default)
  } else {

      TriggerRefineDataDS(DSConnections)
  }


  if (RunAssignmentChecks == TRUE)
  {
      # Call helper function to check if assignment of RefinementOutput succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = OutputName,
                                                  DSConnections = DSConnections))
  }


# Extract objects from list returned by RefineDataDS() and assign them to R server sessions
#-------------------------------------------------------------------------------

  # Named vector determining how objects inside RefinementOutput list created on servers should be extracted
  ObjectNames <- setNames(c(paste0(Module, c(".RefinedDataSet",
                                             ".NonconformingRecords",
                                             ".AffectedRootSubjects",
                                             ".RefinementReport")),
                            "Messages"),
                          nm = c("DataSet",
                                 "NonconformingRecords",
                                 "AffectedRootSubjects",
                                 "Report",
                                 "Messages"))

  # Extract objects from RefinementOutput list
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


# Optionally unpack (unlist) RefinedDataSet
#-------------------------------------------------------------------------------

  if (UnpackRefinedDataSet == TRUE)
  {
      # Get correct package name from 'Meta.Modules'
      ModulePackageName <- dsFredaClient::Meta.Modules[[Module]]

      # Get Refined table names
      RefinedTableNames <- eval(parse(text = paste0(ModulePackageName, "::Meta.Tables$TableName.Refined")))

      for(i in 1:length(RefinedTableNames))
      {
          # Execute server-side assign function
          DSI::datashield.assign(conns = DSConnections,
                                 symbol = paste0(Module, ".RDS.", RefinedTableNames[i]),      # E.g. 'CCP.RDS.Metastasis'
                                 value = call("ExtractFromListDS",
                                              ListName.S = paste0(Module, ".RefinedDataSet"),
                                              ObjectName.S = RefinedTableNames[i]),
                                 async = DS.async)

          if (RunAssignmentChecks == TRUE)
          {
              # Call helper function to check if object assignment succeeded
              Messages$Assignment <- c(Messages$Assignment,
                                       ds.GetObjectStatus(ObjectName = paste0(Module, ".RDS.", RefinedTableNames[i]),
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

  RefinementMessages <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetReportingObjectDS",
                                                            ObjectName.S = "Messages"),
                                                async = DS.async)

  # Create table object for output
  Refinement.Completion <- RefinementMessages %>%
                              map(\(ServerMessages) tibble(Process.CompletionCheck = ServerMessages$Process.CompletionCheck,
                                                           Process.Duration = ServerMessages$Process.Duration )) %>%
                              list_rbind(names_to = "ServerName")

  # Create vector of messages informing about Refinement completion
  Messages$Refinement.Completion <- RefinementMessages %>%
                                      imap(function(ServerMessages, servername)
                                           {
                                              tibble(CompletionCheck = ServerMessages$Process.CompletionCheck,
                                                     Duration = ServerMessages$Process.Duration) %>%
                                                  mutate(Message = case_when(CompletionCheck == "green" ~ paste0("Refinement on server '", servername, "' performed successfully! (Duration: ", Duration, ")"),
                                                                             CompletionCheck == "yellow" ~ paste0("Refinement on server '", servername, "' performed with warnings! \n", ServerMessages$FinalMessage),
                                                                             CompletionCheck == "red" ~ paste0("Refinement on server '", servername, "' could not be performed! \n", ServerMessages$FinalMessage),
                                                                             .default = paste0("Refinement on server '", servername, "' could not be assessed. \n", ServerMessages$FinalMessage)),
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
  Messages$Refinement.Completion <- c(Topic = "Refinement process completion",
                                    Messages$Refinement.Completion)


# Print messages and return output
#-------------------------------------------------------------------------------

  # Print messages on console
  PrintMessages(Messages)

  # Invisibly return Messages and Refinement completion object
  invisible(list(Messages = Messages,
                 Refinement.Completion = Refinement.Completion))
}
