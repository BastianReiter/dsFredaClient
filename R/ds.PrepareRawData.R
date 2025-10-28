
#' ds.PrepareRawData
#'
#' Trigger basic preparatory transformations on RawDataSet prior to Curation:
#' \itemize{  \item Optionally convert all columns to character type
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Try to harmonize feature names employing Fuzzy String Matching and Dictionary look-up }
#'
#' Linked to server-side ASSIGN method \code{PrepareRawDataDS()}
#'
#' @param RawDataSetName \code{string} - Name of Raw Data Set object (list) on server
#' @param Module \code{string} identifying a defined data set and the corresponding meta data needed for feature name harmonization (Examples: 'CCP' / 'P21')
#' @param RDSTableNames \code{character vector} - Names of RDS tables
#' @param FeatureNameDictionary Optional \code{list} containing dictionary data for feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#' @param RunFuzzyStringMatching \code{logical} - Whether to use fuzzy string matching to harmonize raw feature names
#' @param FSMSettings \code{list} of parameters for Fuzzy String Matching ('PreferredMethod', 'Tolerance')
#' @param AddIDFeature \code{list} containing parameters about adding an ID feature to tables:
#'                            \itemize{ \item Do (\code{logical}) - Whether to add an ID feature (running number)
#'                                      \item IDFeatureName (\code{string})
#'                                      \item OverwriteExistingIDFeature (\code{logical}) - Whether to overwrite an existing feature with the same name }
#' @param CompleteCharacterConversion \code{logical} - Indicating whether to convert all features in data set tables to character type
#' @param CurateFeatureNames \code{logical} - Indicating whether (after primary harmonization) feature names should be recoded from 'raw' to 'curated' feature names according to Module-specific meta data
#' @param OutputName \code{character scalar} - Name of output object to be assigned on server - Default: 'RDSPreparationOutput'
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{FALSE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.PrepareRawData <- function(RawDataSetName,
                              Module,
                              RDSTableNames,
                              FeatureNameDictionary = list(),
                              RunFuzzyStringMatching = FALSE,
                              FSMSettings = list(PreferredMethod = "jw",
                                                 Tolerance = 0.2),
                              AddIDFeature = list(Do = FALSE,
                                                  IDFeatureName = "ID",
                                                  OverwriteExistingIDFeature = FALSE),
                              CompleteCharacterConversion = FALSE,
                              CurateFeatureNames = FALSE,
                              OutputName = "RDSPreparationOutput",
                              RunAssignmentChecks = FALSE,
                              DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # RawDataSetName = "P21.RawDataSet"
  # FeatureNameDictionary <- list(Department = c(FAB = "Fachabteilung"))
  # AddIDFeature <- list(Do = TRUE,
  #                     IDFeatureName = "ID",
  #                     OverwriteExistingIDFeature = FALSE)
  # CompleteCharacterConversion <- TRUE
  # OutputName <- "RDSPreparationOutput"
  # RunAssignmentChecks <- FALSE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName),
              is.string(Module),
              is.character(RDSTableNames),
              is.list(FeatureNameDictionary),
              is.flag(RunFuzzyStringMatching),
              is.list(FSMSettings),
              is.list(AddIDFeature),
              is.flag(AddIDFeature$Do),
              is.flag(CompleteCharacterConversion),
              is.flag(CurateFeatureNames),
              is.string(OutputName),
              is.flag(RunAssignmentChecks))
  if (!is.null(AddIDFeature$IDFeatureName)) { assert_that(is.string(AddIDFeature$IDFeatureName)) }
  if (!is.null(AddIDFeature$OverwriteExistingIDFeature)) { assert_that(is.flag(AddIDFeature$OverwriteExistingIDFeature)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side assign function: This creates a list on servers with name assigned with 'OutputName'
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("PrepareRawDataDS",
                                      RawDataSetName.S = RawDataSetName,
                                      Module.S = Module,
                                      FeatureNameDictionary.S = FeatureNameDictionary,
                                      RunFuzzyStringMatching.S = RunFuzzyStringMatching,
                                      FSMSettings.S = FSMSettings,
                                      AddIDFeature.S = AddIDFeature,
                                      CompleteCharacterConversion.S = CompleteCharacterConversion,
                                      CurateFeatureNames.S = CurateFeatureNames))


# Extract objects from list returned by PrepareRawDataDS() and assign them to R server sessions
#-------------------------------------------------------------------------------

  # Named vector determining how objects inside RDSPreparationOutput list created on servers should be extracted
  ObjectNames <- setNames(c(RawDataSetName,
                            paste0(Module, ".OriginalRawDataSet"),
                            "Messages"),
                          nm = c("RawDataSet",
                                 "OriginalRawDataSet",
                                 "Messages"))

  # Extract objects from RDSPreparationOutput list
  for (i in 1:length(ObjectNames))
  {
      # Execute server-side list extraction
      DSI::datashield.assign(conns = DSConnections,
                             symbol = unname(ObjectNames[i]),
                             value = call("ExtractFromListDS",
                                           ListName.S = OutputName,
                                           ObjectName.S = names(ObjectNames)[i]))

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = unname(ObjectNames[i]),
                                                      DSConnections = DSConnections))
      }
  }


# Re-unpack RDS list, so that PREPARED RDS tables overwrite 'old' ones on servers
#-------------------------------------------------------------------------------

  for(tablename in RDSTableNames)
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0(Module, ".RDS.", tablename),      # E.g. 'P21.RDS.Case'
                             value = call("ExtractFromListDS",
                                          ListName.S = RawDataSetName,
                                          ObjectName.S = tablename))

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = paste0(Module, ".RDS.", RDSTableNames[i]),
                                                      DSConnections = DSConnections))
      }
  }


#--- Get 'Messages' object from servers (as a list of lists) -------------------
  Messages <- DSI::datashield.aggregate(conns = DSConnections,
                                        expr = call("GetReportingObjectDS",
                                                    ObjectName.S = "Messages"))

#--- Print and invisibly return Messages ---------------------------------------
  PrintMessages(Messages)

  invisible(Messages)
}
