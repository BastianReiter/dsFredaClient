
#' ds.GetCrossTab
#'
#' Perform cross tabulation for an arbitrary number of features in a table on the servers.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetCrossTabDS()}
#'
#' @param TableName \code{string} - Name of \code{data.frame} on server
#' @param FeatureNames \code{character} - Vector of feature names
#' @param RemoveNA \code{logical} - Indicating whether missing values should be removed prior to cross tabulation - Default: \code{FALSE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list}
#'            \itemize{ \item CrossTab (\code{list}),
#'                      \item ChiSquaredTest (\code{list}) }
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetCrossTab <- function(TableName,
                           FeatureNames,
                           RemoveNA = FALSE,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(DSI)
  require(purrr)
  require(stringr)
  require(tidyr)

  # --- For Testing Purposes ---
  # TableName <- "ADS_Patient"
  # FeatureNames <- c("Sex", "CountDiagnoses")
  # RemoveNA = FALSE
  # DSConnections <- CCPConnections

  # --- Argument Assertions ---
  assert_that(is.string(TableName),
              is.character(FeatureNames),
              is.logical(RemoveNA))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get (maximum) number of unique values for selected features and multiply them to calculate the projected number of value combinations
  ProjectedCombinations <- sapply(FeatureNames, function(featurename)
                                                { ds.GetFeatureInfo(TableName = TableName,
                                                                    FeatureName = featurename) %>%
                                                      pull(CountUniqueValues) %>%
                                                      max(na.rm = TRUE)
                                                }) %>%
                              prod(na.rm = TRUE)

  # Prompt user input when projected number of value combinations is high
  if (ProjectedCombinations > 20)
  {
      UserResponse <- readline(prompt = paste0("The features you selected will result in a high number of different value combinations (",
                                               ProjectedCombinations,
                                               "). This can result in low absolute frequencies for certain combinations, which could increase disclosure potential.\n",
                                               "Do you want to continue? (y/n) "))

      if (!(UserResponse %in% c("Y", "y"))) { stop(call. = FALSE) }
  }


  # Paste elements of 'FeatureNames' together and encode the resulting string to make it passable through DSI
  FeatureNamesString <- paste0(FeatureNames, collapse = ", ") %>%
                            .encode_tidy_eval(.get_encode_dictionary())

  # Call server-side function
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetCrossTabDS",
                                                         TableName.S = TableName,
                                                         FeatureNames.S = FeatureNamesString,
                                                         RemoveNA.S = RemoveNA))

  #-----------------------------------------------------------------------------
  # Processing of server-specific cross tabs and calculation of cumulated cross tab
  #-----------------------------------------------------------------------------

  # Create coherent data.frame from ServerReturns
  CrossTab.Separate <- ServerReturns %>%
                            list_transpose(simplify = FALSE) %>%
                            pluck("CrossTab") %>%
                            list_rbind(names_to = "Server")

  # Process CrossTab.Separate
  CrossTab.Separate <- CrossTab.Separate %>%
                            expand(!!!syms(c("Server", FeatureNames))) %>%     # This will harmonize the extent of value combinations to facilitate further function logic
                            left_join(CrossTab.Separate, by = join_by(!!!syms(c("Server", FeatureNames)))) %>%
                            mutate(across(all_of(FeatureNames),      # Turn NAs into character for syntax reasons (later group_by not working properly otherwise)
                                          ~ if_else(is.na(.x), "<NA>", .x))) %>%
                            mutate(JointCount = if_else(is.na(JointCount) & (is.na(IsMasked.JointCount) | IsMasked.JointCount == FALSE),      # Turn NAs in 'JointCount' into 0, if they do not stand for a value that has been masked by servers
                                                        0,
                                                        JointCount))

  # Go through 'FeatureNames' for consecutive grouping and fill operation to complete marginal counts where they are missing
  for (featurename in FeatureNames)
  {
      MargCountColumn <- sym(paste0("MargCount.", featurename))
      IsMaskedColumn <- sym(paste0("IsMasked.MargCount.", featurename))

      CrossTab.Separate <- CrossTab.Separate %>%
                                group_by(across(c(Server, all_of(featurename)))) %>%
                                    fill(!!MargCountColumn, .direction = "downup") %>%
                                ungroup() %>%
                                mutate(!!MargCountColumn := if_else(is.na(!!MargCountColumn) & (is.na(!!IsMaskedColumn) | !!IsMaskedColumn == FALSE),      # Turn NAs in MarginalCounts into 0, if they do not stand for a value that has been masked by servers
                                                                    0,
                                                                    !!MargCountColumn))
  }

  # Create cumulated CrossTab
  CrossTab.Cumulated <- CrossTab.Separate %>%
                            group_by(across(all_of(FeatureNames))) %>%
                                summarize(Server = "All",
                                          across(c("JointCount", starts_with("MargCount.")),
                                                 ~ sum(.x, na.rm = TRUE)),
                                          across(starts_with("IsMasked."),
                                                 ~ sum(.x == TRUE, na.rm = TRUE),
                                                 .names = "ServersWithMaskedCounts.{.col}")) %>%
                                rename_with(.cols = starts_with("ServersWithMaskedCounts.IsMasked."),
                                            .fn = ~ str_remove(.x, "IsMasked.")) %>%
                            ungroup() %>%
                            as.data.frame()

  # Bind server-specific and cumulated CrossTabs together
  CrossTab <- CrossTab.Cumulated %>%
                    bind_rows(CrossTab.Separate) %>%
                    relocate(Server, .before = 1) %>%
                    group_by(Server) %>%
                        arrange(across(all_of(FeatureNames),   # For cosmetic reasons put rows with '<NA>' values in the feature columns at the bottom
                                       ~ .x == "<NA>"),
                                .by_group = TRUE) %>%
                    ungroup()


  #-----------------------------------------------------------------------------
  # Calculate Relative Frequencies (Joint and Marginal) for CrossTabs
  #-----------------------------------------------------------------------------

  # First calculate the Joint Relative Frequencies for
  CrossTab <- CrossTab %>%
                  group_by(Server) %>%
                      mutate(JointRelFreq = JointCount / sum(JointCount, na.rm = TRUE), .after = JointCount) %>%
                  ungroup()

  # Then calculate Marginal Relative Frequencies
  for (featurename in FeatureNames)
  {
      MargCountColumn <- sym(paste0("MargCount.", featurename))
      MargRelFreqColumn <- sym(paste0("MargRelFreq.", featurename))

      CrossTab <- CrossTab %>%
                      group_by(Server) %>%
                          mutate(!!MargRelFreqColumn := !!MargCountColumn / sum(JointCount, na.rm = TRUE),
                                 .after = !!MargCountColumn) %>%
                      ungroup()
  }

  # Split coherent data.frame into list of data.frames
  CrossTab <- CrossTab %>%
                    split(., .$Server)


  #-----------------------------------------------------------------------------
  # Perform cumulated tests of significance
  #   - In case there are two crossed features, perform Chi-Squared-Test
  #   - In case there are more than two crossed features, perform Log-Linear Modeling
  #-----------------------------------------------------------------------------

  ChiSquaredTest <- NULL

  if (length(FeatureNames) == 2)
  {
      # Get server-specific ChiSq.PValues from ServerReturns
      ChiSquaredTest <- ServerReturns %>%
                            list_transpose(simplify = FALSE) %>%
                            pluck("ChiSq.PValue")

      # Select relevant columns from CrossTab.Cumulated as preparation for conversion into object of class 'table'
      PrepareTable <- CrossTab.Cumulated %>%
                          select(c(all_of(FeatureNames), "JointCount")) %>%
                          filter(if_all(all_of(FeatureNames), ~ !is.na(.)))

      # Get object of class 'table' (chisq.test() needs a 'table' as argument)
      TableObject <- xtabs(formula = reformulate(termlabels = FeatureNames,
                                                 response = "JointCount"),
                           data = PrepareTable)

      # Perform Chi-Squared-Test
      ChiSq.Cumulated <- chisq.test(x = TableObject)

      # Bind cumulated Chi Squared test result with list of server-specific p-values
      ChiSquaredTest <- c(list(All = ChiSq.Cumulated,
                          ChiSquaredTest))
  }

  # loglin()
  # if (length(FeatureNames) > 2)
  # {
  #
  # }


  #-----------------------------------------------------------------------------
  return(list(CrossTab = CrossTab,
              ChiSquaredTest = ChiSquaredTest))
}
