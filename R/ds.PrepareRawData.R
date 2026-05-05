
#' ds.PrepareRawData
#'
#' Trigger basic preparatory transformations on RawDataSet prior to Curation:
#' \itemize{  \item Feature name harmonization
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Type- or format-conversion of features }
#'
#' Linked to server-side ASSIGN method \code{PrepareRawDataDS()}
#'
#' @param RawDataSetName \code{string} - Name of Raw Data Set object (list) on server
#' @param Module \code{string} identifying a defined data set and the corresponding meta data needed for feature name harmonization (Examples: 'CCP' / 'P21')
#' @param RDSTableNames \code{character vector} - Names of RDS tables
#' @param FeatureNames.Dictionary Optional \code{list} containing dictionary data for feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#' @param FeatureNames.FuzzyStringMatching.Run \code{logical} - Whether to use fuzzy string matching to harmonize raw feature names
#' @param FeatureNames.FuzzyStringMatching.PreferredMethod \code{logical} - Selects the method \code{stringdist()} uses (see \code{stringdist} documentation) - Default: 'jw'
#' @param FeatureNames.FuzzyStringMatching.Tolerance \code{logical} - Number between 0 and 1 relating to normalized distance between to strings (1 meaning furthest distance, 0 meaning no distance). If a string in 'Vector' is not similar enough to any of the 'EligibleStrings' and its minimal harmonized distance exceeds this number, it is set \code{NA}. - Default: 0.2
#' @param AddIDFeature.Do \code{logical} - Whether to add an ID feature (running number) to tables
#' @param AddIDFeature.IDFeatureName \code{string} - The name of the new ID feature
#' @param AddIDFeature.OverwriteExistingIDFeature \code{logical} - Whether to overwrite an existing feature with the same name
#' @param Conversion.IntoCharacter \code{string} - Controls conversion of certain features in data set tables into character type. One of 'None' / 'All' / 'Date'. - Default: 'None'
#' @param Conversion.DateIntoPOSIXct \code{list} - Containing character vectors representing date formats used in argument 'tryFormats' in function \code{base::as.POSIXct}. List element should be either '.All' for all date features or specific date feature names. - Default: \code{NULL}
#' @param CurateFeatureNames \code{logical} - Indicating whether (after primary harmonization) feature names should be recoded from 'raw' to 'curated' feature names according to Module-specific meta data
#' @param OutputName \code{character scalar} - Name of output object to be assigned on server - Default: 'RDSPreparationOutput'
#' @param RunAssignmentChecks \code{logical} - Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{FALSE}
#' @param PrintMessages \code{logical} - Indicating whether to print messages
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{flag} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
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
                              FeatureNames.Dictionary = list(),
                              FeatureNames.FuzzyStringMatching.Run = FALSE,
                              FeatureNames.FuzzyStringMatching.PreferredMethod = "jw",
                              FeatureNames.FuzzyStringMatching.Tolerance = 0.2,
                              AddIDFeature.Do = FALSE,
                              AddIDFeature.IDFeatureName = "ID",
                              AddIDFeature.OverwriteExistingIDFeature = FALSE,
                              Conversion.IntoCharacter = "None",
                              Conversion.DateIntoPOSIXct = NULL,
                              CurateFeatureNames = FALSE,
                              OutputName = "RDSPreparationOutput",
                              RunAssignmentChecks = FALSE,
                              PrintMessages = TRUE,
                              DSConnections = NULL,
                              DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # RawDataSetName = "P21.RawDataSet"
  # FeatureNames.Dictionary <- list(Department = c(FAB = "Fachabteilung"))
  # AddIDFeature <- list(Do = TRUE,
  #                     IDFeatureName = "ID",
  #                     OverwriteExistingIDFeature = FALSE)
  # TotalCharacterConversion <- TRUE
  # OutputName <- "RDSPreparationOutput"
  # RunAssignmentChecks <- FALSE
  # PrintMessages <- TRUE
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName),
              is.string(Module),
              is.character(RDSTableNames),
              is.list(FeatureNames.Dictionary),
              is.flag(FeatureNames.FuzzyStringMatching.Run),
              is.string(FeatureNames.FuzzyStringMatching.PreferredMethod),
              is.number(FeatureNames.FuzzyStringMatching.Tolerance),
              is.flag(AddIDFeature.Do),
              is.string(Conversion.IntoCharacter),
              is.flag(CurateFeatureNames),
              is.string(OutputName),
              is.flag(PrintMessages),
              is.flag(RunAssignmentChecks),
              is.flag(DS.async))
  if (FeatureNames.FuzzyStringMatching.Tolerance < 0 | FeatureNames.FuzzyStringMatching.Tolerance > 1) { stop("ERROR: Value of argument 'FeatureNames.FuzzyStringMatching.Tolerance' must be between 0 and 1.") }
  if (!is.null(AddIDFeature.IDFeatureName)) { assert_that(is.string(AddIDFeature.IDFeatureName)) }
  if (!is.null(AddIDFeature.OverwriteExistingIDFeature)) { assert_that(is.flag(AddIDFeature.OverwriteExistingIDFeature)) }
  if (!(Conversion.IntoCharacter %in% c("None", "All", "Date"))) { stop("ERROR: Value of argument 'Conversion.IntoCharacter' must be one of 'None' / 'All' / 'Date'.") }
  if (!is.null(Conversion.DateIntoPOSIXct)) { assert_that(is.list(Conversion.DateIntoPOSIXct)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side assign function: This creates a list on servers with name assigned with 'OutputName'
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("PrepareRawDataDS",
                                      RawDataSetName.S = RawDataSetName,
                                      Module.S = Module,
                                      FeatureNames.Dictionary.S = FeatureNames.Dictionary,
                                      FeatureNames.FuzzyStringMatching.Run.S = FeatureNames.FuzzyStringMatching.Run,
                                      FeatureNames.FuzzyStringMatching.PreferredMethod.S = FeatureNames.FuzzyStringMatching.PreferredMethod,
                                      FeatureNames.FuzzyStringMatching.Tolerance.S = FeatureNames.FuzzyStringMatching.Tolerance,
                                      AddIDFeature.Do.S = AddIDFeature.Do,
                                      AddIDFeature.IDFeatureName.S = AddIDFeature.IDFeatureName,
                                      AddIDFeature.OverwriteExistingIDFeature.S = AddIDFeature.OverwriteExistingIDFeature,
                                      Conversion.IntoCharacter.S = Conversion.IntoCharacter,
                                      Conversion.DateIntoPOSIXct.S = Conversion.DateIntoPOSIXct,
                                      CurateFeatureNames.S = CurateFeatureNames),
                         async = DS.async)


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


# Re-unpack RDS list, so that PREPARED RDS tables overwrite 'old' ones on servers
#-------------------------------------------------------------------------------

  for (tablename in RDSTableNames)
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0(Module, ".RDS.", tablename),      # E.g. 'CCP.RDS.Case'
                             value = call("ExtractFromListDS",
                                          ListName.S = RawDataSetName,
                                          ObjectName.S = tablename),
                             async = DS.async)

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = paste0(Module, ".RDS.", RDSTableNames[i]),
                                                      DSConnections = DSConnections,
                                                      DS.async = DS.async))
      }
  }


#--- Get 'Messages' object from servers (as a list of lists) -------------------
  Messages <- DSI::datashield.aggregate(conns = DSConnections,
                                        expr = call("GetReportingObjectDS",
                                                    ObjectName.S = "Messages"),
                                        async = DS.async)

#--- Print and invisibly return Messages ---------------------------------------
  if (PrintMessages == TRUE) { PrintMessages(Messages, .ListNamesAsSubtopic = TRUE) }

  invisible(Messages)
}
