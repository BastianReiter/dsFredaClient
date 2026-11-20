
#' ds.PrepareSourceData
#'
#' Trigger basic preparatory transformations on SourceDataSet prior to Refinement:
#' \itemize{  \item Feature name harmonization
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Type- or format-conversion of features }
#'
#' Linked to server-side ASSIGN method \code{PrepareSourceDataDS()}
#'
#' @param SourceDataSetName \code{string} - Name of Source Data Set object (list) on server
#' @param Module \code{string} identifying a defined data set and the corresponding meta data needed for feature name harmonization (Examples: 'CCP' / 'P21')
#' @param SDSTableNames \code{character vector} - Names of SDS tables
#' @param FeatureNames.Dictionary Optional \code{list} containing dictionary data for feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#' @param FeatureNames.FuzzyStringMatching.Run \code{logical} - Whether to use fuzzy string matching to harmonize Source feature names
#' @param FeatureNames.FuzzyStringMatching.PreferredMethod \code{logical} - Selects the method \code{stringdist()} uses (see \code{stringdist} documentation) - Default: 'jw'
#' @param FeatureNames.FuzzyStringMatching.Tolerance \code{logical} - Number between 0 and 1 relating to normalized distance between to strings (1 meaning furthest distance, 0 meaning no distance). If a string in 'Vector' is not similar enough to any of the 'EligibleStrings' and its minimal harmonized distance exceeds this number, it is set \code{NA}. - Default: 0.2
#' @param AddIDFeature.Do \code{logical} - Whether to add an ID feature (running number) to tables
#' @param AddIDFeature.IDFeatureName \code{string} - The name of the new ID feature
#' @param AddIDFeature.OverwriteExistingIDFeature \code{logical} - Whether to overwrite an existing feature with the same name
#' @param Conversion.IntoCharacter \code{string} - Controls conversion of certain features in data set tables into character type. One of 'None' / 'All' / 'Date'. - Default: 'None'
#' @param Conversion.DateIntoPOSIXct \code{list} - Containing character vectors representing date formats used in argument 'tryFormats' in function \code{base::as.POSIXct}. List element should be either '.All' for all date features or specific date feature names. - Default: \code{NULL}
#' @param RecodeFeatureNames \code{logical} - Indicating whether (after primary harmonization) feature names should be recoded from 'Source' to 'Refined' feature names according to Module-specific meta data
#' @param OutputName \code{character scalar} - Name of output object to be assigned on server - Default: 'SDSPreparationOutput'
#' @param RunAssignmentChecks \code{logical} - Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{FALSE}
#' @param PrintMessages \code{logical} - Indicating whether to print messages
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{flag} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.PrepareSourceData <- function(SourceDataSetName,
                              Module,
                              SDSTableNames,
                              FeatureNames.Dictionary = list(),
                              FeatureNames.FuzzyStringMatching.Run = FALSE,
                              FeatureNames.FuzzyStringMatching.PreferredMethod = "jw",
                              FeatureNames.FuzzyStringMatching.Tolerance = 0.2,
                              AddIDFeature.Do = FALSE,
                              AddIDFeature.IDFeatureName = "ID",
                              AddIDFeature.OverwriteExistingIDFeature = FALSE,
                              Conversion.IntoCharacter = "None",
                              Conversion.DateIntoPOSIXct = NULL,
                              RecodeFeatureNames = FALSE,
                              OutputName = "SDSPreparationOutput",
                              RunAssignmentChecks = FALSE,
                              PrintMessages = TRUE,
                              DSConnections = NULL,
                              DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # SourceDataSetName <- "P21.SourceDataSet"
  # Module <- "P21"
  # SDSTableNames <- dsFredaP21Client::Meta.Tables$TableName.Refined
  # FeatureNames.Dictionary <- list(Case = c("Aufnahmegrund" = NA,      # Fix termini that could otherwise be falsely classified by Fuzzy String Matching
  #                                          "Geburtsmonat" = NA),
  #                                 DiagnosisICD = c("icd_lokalisation" = "Lokalisation",
  #                                                  "diagnosensicherheit" = NA),
  #                                 Department = c(FAB = "Fachabteilung"))
  # FeatureNames.FuzzyStringMatching.Run <- TRUE
  # FeatureNames.FuzzyStringMatching.PreferredMethod <- "jw"
  # FeatureNames.FuzzyStringMatching.Tolerance <- 0.2
  # AddIDFeature.Do <- TRUE
  # AddIDFeature.IDFeatureName <- "ID"
  # AddIDFeature.OverwriteExistingIDFeature <- FALSE
  # Conversion.IntoCharacter <- "All"
  # Conversion.DateIntoPOSIXct <- NULL
  # Conversion.DateIntoPOSIXct <- list(.All = c("%Y%m%d%H%M", "%Y%m%d", "%Y-%m-%d"))
  # RecodeFeatureNames <- TRUE
  # OutputName <- "SDSPreparationOutput"
  # RunAssignmentChecks <- FALSE
  # PrintMessages <- TRUE
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(SourceDataSetName),
              is.string(Module),
              is.character(SDSTableNames),
              is.list(FeatureNames.Dictionary),
              is.flag(FeatureNames.FuzzyStringMatching.Run),
              is.string(FeatureNames.FuzzyStringMatching.PreferredMethod),
              is.number(FeatureNames.FuzzyStringMatching.Tolerance),
              is.flag(AddIDFeature.Do),
              is.string(Conversion.IntoCharacter),
              is.flag(RecodeFeatureNames),
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

  # Turn list 'Conversion.DateIntoPOSIXct' into one big encoded string to make it passable through DSI
  if (length(Conversion.DateIntoPOSIXct) > 0)
  {
      # Turn character vectors into strings
      Conversion.DateIntoPOSIXct <- Conversion.DateIntoPOSIXct %>%
                                        imap_chr(\(X, selector) paste0("'", selector, "' = ", paste0("c(", paste0("'", X, "'", collapse = ", "), ")")))

      # Turn complete list into one string and encode
      Conversion.DateIntoPOSIXct <- .encode_tidy_eval(paste0("list(", paste0(Conversion.DateIntoPOSIXct, collapse = ", "), ")"), .get_encode_dictionary())
  }

  # Execute server-side assign function: This creates a list on servers with name assigned with 'OutputName'
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("PrepareSourceDataDS",
                                      SourceDataSetName.S = SourceDataSetName,
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
                                      RecodeFeatureNames.S = RecodeFeatureNames),
                         async = DS.async)


# Extract objects from list returned by PrepareSourceDataDS() and assign them to R server sessions
#-------------------------------------------------------------------------------

  # Named vector determining how objects inside SDSPreparationOutput list created on servers should be extracted
  ObjectNames <- setNames(c(SourceDataSetName,
                            paste0(Module, ".OriginalSourceDataSet"),
                            "Messages"),
                          nm = c("SourceDataSet",
                                 "OriginalSourceDataSet",
                                 "Messages"))

  # Extract objects from SDSPreparationOutput list
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


# Re-unpack SDS list, so that PREPARED SDS tables overwrite 'old' ones on servers
#-------------------------------------------------------------------------------

  for (tablename in SDSTableNames)
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0(Module, ".SDS.", tablename),      # E.g. 'CCP.SDS.Case'
                             value = call("ExtractFromListDS",
                                          ListName.S = SourceDataSetName,
                                          ObjectName.S = tablename),
                             async = DS.async)

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = paste0(Module, ".SDS.", SDSTableNames[i]),
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
