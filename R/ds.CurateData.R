
#' ds.CurateData
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Triggers transformation of Raw Data Set (RDS) into Curated Data Set (CDS) on servers.
#'
#' Linked to server-side ASSIGN methods \code{CurateDataDS()} and \code{ExtractFromListDS()}
#'
#' @param RawDataSetName \code{character} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param OutputName \code{character} - Name of output object to be assigned on server - Default: 'CurationOutput'
#' @param Settings \code{list} - Settings passed to function
#'                   \itemize{  \item \emph{DataHarmonization} - \code{list}
#'                                  \itemize{ \item Run \code{logical} - Whether or not to perform data harmonization - Default: \code{TRUE}
#'                                            \item Methods \code{data.frame} - Default: \code{dsCCPhos::Meta_DataHarmonizationMethods}
#'                                            \item TransformativeExpressions \code{data.frame} - Default: \code{dsCCPhos::Meta_TransformativeExpressions}
#'                                            \item TransformativeExpressions.Profile \code{string} - Profile used in \emph{TransformativeExpressions} - Default: 'Default'
#'                                            \item Dictionary \code{data.frame} - Default: \code{dsCCPhos::Meta_Dictionary}
#'                                            \item Dictionary.Profile \code{string} - Profile used in \emph{Dictionary} - Default: 'Default'
#'                                            \item FuzzyStringMatching \code{data.frame} - Default: \code{dsCCPhos::Meta_FuzzyStringMatching}
#'                                            \item FuzzyStringMatching.Profile \code{string} - Profile used in \emph{FuzzyStringMatching} - Default: 'Default'}
#'                              \item \emph{FeatureObligations} - \code{list}
#'                                  \itemize{ \item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureObligations}
#'                                            \item RuleSet.Profile \code{string} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations$RuleSet} - Default: 'Default'}
#'                              \item \emph{FeatureTracking} - \code{list}
#'                                  \itemize{ \item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureTracking}
#'                                            \item RuleSet.Profile \code{string} - Profile name defining which features should be tracked/monitored during curation process. Profile name must be stated in \code{FeatureTracking$RuleSet} - Default: 'Default'}
#'                              \item \emph{TableCleaning} - \code{list}
#'                                  \itemize{ \item Run \code{logical} - Whether or not to perform table cleaning (removal of redundant and ineligible entries) - Default: \code{TRUE}}
#'                              \item \emph{TableNormalization} - \code{list}
#'                                  \itemize{ \item Run \code{logical} - Whether or not to perform table normalization - Default: \code{TRUE}
#'                                            \item RuleSet \code{data.frame} - Deault: \code{dsCCPhos::Meta_TableNormalization}
#'                                            \item RuleSet.Profile \code{string} - Profile name defining rule set to be used for table normalization. Profile name must be stated in \code{TableNormalization$RuleSet} - Default: 'Default'}}
#'
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param UnpackCuratedDataSet \code{logical} indicating whether the Curated Data Set \code{list} should be unpacked so that tables \code{data.frames} are directly accessible - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return \code{list} of following objects:
#'         \itemize{\item 'Messages' - Info messages concerning completion of \code{CurateDataDS()} and assignment of the following objects on server:
#'                        \itemize{\item CurationOutput (\code{list})
#'                                    \itemize{ \item CuratedDataSet \code{list}
#'                                                \itemize{ \item BioSampling
#'                                                          \item Diagnosis
#'                                                          \item DiseaseStatus
#'                                                          \item GeneralCondition
#'                                                          \item Histology
#'                                                          \item Metastasis
#'                                                          \item MolecularDiagnostics
#'                                                          \item OtherClassification
#'                                                          \item Patient
#'                                                          \item RadiationTherapy
#'                                                          \item Staging
#'                                                          \item Surgery
#'                                                          \item SystemicTherapy
#'                                                          \item TherapyRecommendation}
#'                                              \item CurationReport \code{list}
#'                                                \itemize{\item EntryCounts \code{tibble}
#'                                                          \itemize{ \item Table
#'                                                                    \item InitialCount
#'                                                                    \item ExcludedPrimary
#'                                                                    \item AfterPrimaryExclusion
#'                                                                    \item ExcludedSecondary
#'                                                                    \item AfterSecondaryExclusion
#'                                                                    \item ExcludedSecondaryRedundancy
#'                                                                    \item AfterSecondaryRedundancyExclusion}
#'                                                          \item Transformation (list of lists)
#'                                                            \itemize{ \item Monitors
#'                                                                      \item EligibilityOverviews
#'                                                                      \item ValueSetOverviews}}
#'                                              \item CurationMessages \code{list}}}
#'                  \item 'CurationCompletionCheck'}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.CurateData <- function(RawDataSetName = "RawDataSet",
                          OutputName = "CurationOutput",
                          Settings = NULL,
                          # Settings = list(DataHarmonization = list(Run = TRUE,
                          #                                          TransformativeExpressions.Profile = "Default",
                          #                                          Dictionary.Profile = "Default",
                          #                                          FuzzyStringMatching.Profile = "Default"),
                          #                 FeatureObligations = list(RuleSet.Profile = "Default"),
                          #                 FeatureTracking = list(RuleSet.Profile = "Default")),
                          #--- Secondary Arguments ---
                          RunAssignmentChecks = TRUE,
                          UnpackCuratedDataSet = TRUE,
                          DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)

  #--- For Testing Purposes ---
  # RawDataSetName <- "RawDataSet"
  # OutputName <- "CurationOutput"
  # RunAssignmentChecks <- FALSE
  # UnpackCuratedDataSet <- TRUE
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- list() }
  Messages$CurationCompletion <- list()


  # 1) Trigger dsCCPhos::CurateDataDS()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Execute the server-side function call
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("CurateDataDS",
                                      RawDataSetName.S = RawDataSetName,
                                      Settings.S = Settings))

  if (RunAssignmentChecks == TRUE)
  {
      # Call helper function to check if assignment of CurationOutput succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = OutputName,
                                                  DSConnections = DSConnections))
  }


  # 2) Extract objects from list returned by CurateDataDS() and assign them to R server sessions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  CurationOutputObjects <- c("CuratedDataSet",
                             "CurationReport",
                             "CurationMessages")

  for(i in 1:length(CurationOutputObjects))
  {
      # Execute server-side list extraction
      DSI::datashield.assign(conns = DSConnections,
                             symbol = CurationOutputObjects[i],
                             value = call("ExtractFromListDS",
                                           ListName.S = OutputName,
                                           ObjectName.S = CurationOutputObjects[i]))

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = CurationOutputObjects[i],
                                                      DSConnections = DSConnections))
      }
  }

  # Optionally unpack (unlist) CuratedDataSet
  if (UnpackCuratedDataSet == TRUE)
  {
      # Get curated table names
      CCPTableNames_CDS <- dsCCPhosClient::Meta_Tables$TableName_Curated

      for(i in 1:length(CCPTableNames_CDS))
      {
          # Execute server-side assign function
          DSI::datashield.assign(conns = DSConnections,
                                 symbol = paste0("CDS_", CCPTableNames_CDS[i]),      # E.g. 'CDS_Metastasis'
                                 value = call("ExtractFromListDS",
                                              ListName.S = "CuratedDataSet",
                                              ObjectName.S = CCPTableNames_CDS[i]))

          if (RunAssignmentChecks == TRUE)
          {
              # Call helper function to check if object assignment succeeded
              Messages$Assignment <- c(Messages$Assignment,
                                       ds.GetObjectStatus(ObjectName = paste0("CDS_", CCPTableNames_CDS[i]),
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



  # 3) Get CurationMessages objects from servers (as a list of lists) and create completion check object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  CurationMessages <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetReportingObjectDS",
                                                            ObjectName.S = "CurationMessages"))

  # Create table object for output
  CurationCompletionCheck <- CurationMessages %>%
                                map(\(ServerMessages) tibble(CheckCurationCompletion = ServerMessages$CheckCurationCompletion) ) %>%
                                list_rbind(names_to = "ServerName")

  # Create vector of messages informing about curation completion
  Messages$CurationCompletion <- CurationMessages %>%
                                    imap(function(ServerMessages, servername)
                                         {
                                            case_when(ServerMessages$CheckCurationCompletion == "green" ~ MakeFunctionMessage(Text = paste0("Curation on server '", servername, "' performed successfully!"),
                                                                                                                            IsClassSuccess = TRUE),
                                                      ServerMessages$CheckCurationCompletion == "yellow" ~ MakeFunctionMessage(Text = paste0("Curation on server '", servername, "' performed with warnings! \n",
                                                                                                                                           ServerMessages$FinalMessage),
                                                                                                                             IsClassWarning = TRUE),
                                                      ServerMessages$CheckCurationCompletion == "red" ~ MakeFunctionMessage(Text = paste0("Curation on server '", servername, "' could not be performed! \n",
                                                                                                                                        ServerMessages$FinalMessage),
                                                                                                                          IsClassFailure = TRUE),
                                                      TRUE ~ MakeFunctionMessage(Text = paste0("Curation on server '", servername, "' could not be assessed. \n",
                                                                                               ServerMessages$FinalMessage),
                                                                                 IsClassFailure = TRUE))
                                         }) %>%
                                    list_c()

  # Add topic element to start of vector
  Messages$CurationCompletion <- c(Topic = "Curation process completion",
                                   Messages$CurationCompletion)


  # Print messages and return output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  PrintMessages(Messages)

  # Invisibly return Messages and Curation completion check object
  invisible(list(Messages = Messages,
                 CurationCompletionCheck = CurationCompletionCheck))
}
