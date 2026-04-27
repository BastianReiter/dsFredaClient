
#' ds.GetCurationReport
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Receive Curation Reports from servers and create cumulated reporting objects.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetReportingObjectDS()}
#'
#' @param Module Optional \code{string} identifying a defined data set and the corresponding meta data (Examples: 'CCP' / 'P21')
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return A \code{list} of the following structure:
#'         \itemize{  \item Log
#'                    \item Counter
#'                        \itemize{ \item DataSetLevel
#'                                  \item TableLevel
#'                                  \item Details }
#'                    \item DataHarmonization
#'                        \itemize{ \item Reports
#'                                      \itemize{ \item DataSetLevel
#'                                                \item TableLevel
#'                                                \item FeatureLevel
#'                                                \item ValueLevel
#'                                                \item ValueSets }
#'                                  \item Monitors
#'                                      \itemize{ \item TableLevel
#'                                                \item FeatureLevel
#'                                                \item ValueLevel }}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetCurationReport <- function(Module = "CCP",
                                 DSConnections = NULL,
                                 DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Module <- "CCP"
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  if (!is.null(Module)) { assert_that(is.string(Module)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#===============================================================================
# - OVERVIEW -
#===============================================================================
#   A)  Get CurationReports from all servers
#   B)  Rearrangement of LOG report objects
#   C)  Compilation of COUNTER data
#   D)  Compilation of DATA HARMONIZATION Reports and Monitors
#-------------------------------------------------------------------------------

#===============================================================================
# A) Get CurationReports from all servers
#===============================================================================

  # Get CurationReport objects from servers
  CurationReports <- DSI::datashield.aggregate(conns = DSConnections,
                                               expr = call("GetReportingObjectDS",
                                                           ObjectName.S = paste0(Module, ".CurationReport")),
                                               async = DS.async)


#===============================================================================
# B) Rearrangement of LOG report objects
#===============================================================================

  Log <- CurationReports %>%
              list_transpose() %>%
              pluck("Log")


#===============================================================================
# C) Compilation of COUNTER data
#===============================================================================
#   1) Cumulate stage-level Counter data as basis for summaries
#   2) Create Summary View from stage-level data and split into table-level and data-set-level summary
#   3) Extensive Counter data on sub-stage-level (details), no cumulation
#-------------------------------------------------------------------------------

# C 1) COUNTER SUMMARY on DATA SET level
#-------------------------------------------------------------------------------


  Counter.StageLevel.Servers <- CurationReports %>%
                                    map(\(ServerCurationReport) ServerCurationReport %>%
                                                                    pluck("Counter", "Extensive") %>%
                                                                    map(\(TableCounter) TableCounter %>% pluck("StageLevel")) %>%
                                                                    list_rbind(names_to = "Table")) %>%
                                    list_rbind(names_to = "Server")

  Counter.StageLevel.Cumulated <- Counter.StageLevel.Servers %>%
                                      group_by(ProcessingStage, Table) %>%
                                          summarize(Server = "All",
                                                    across(starts_with("Count") & !CountLevel & !contains("Proportion"),
                                                           ~ sum(.x, na.rm = TRUE))) %>%
                                          mutate(CountRecords.Change.Proportion = case_when(CountRecords.Prior > 0 ~ CountRecords.Change / CountRecords.Prior,
                                                                                            .default = NA_real_),
                                                 CountRootSubjects.Change.Proportion = case_when(CountRootSubjects.Prior > 0 ~ CountRootSubjects.Change / CountRootSubjects.Prior,
                                                                                                 .default = NA_real_),
                                                 CountSeedSubjects.Change.Proportion = case_when(CountSeedSubjects.Prior > 0 ~ CountSeedSubjects.Change / CountSeedSubjects.Prior,
                                                                                                 .default = NA_real_))

  Counter.StageLevel <- Counter.StageLevel.Cumulated %>%
                            bind_rows(Counter.StageLevel.Servers)


# C 2) Create COUNTER SUMMARY views from Stage-level data
#-------------------------------------------------------------------------------

  Counter.Summary <- Counter.StageLevel %>%
                          select(Server,
                                 ProcessingStage,
                                 Table,
                                 CountRecords.Change,
                                 CountRecords.Change.Proportion,
                                 CountRecords.Post,
                                 CountRootSubjects.Change,
                                 CountRootSubjects.Change.Proportion,
                                 CountRootSubjects.Post,
                                 CountSeedSubjects.Change,
                                 CountSeedSubjects.Change.Proportion,
                                 CountSeedSubjects.Post) %>%
                          rename(CountRecords = "CountRecords.Post",
                                 CountRootSubjects = CountRootSubjects.Post,
                                 CountSeedSubjects = CountSeedSubjects.Post) %>%
                          mutate(ProcessingStage = str_remove_all(ProcessingStage, " ")) %>%
                          pivot_wider(names_from = ProcessingStage,
                                      names_glue = "{ProcessingStage}.{.value}",
                                      values_from = !c(Server, ProcessingStage, Table)) %>%
                          select(Server,
                                 Table,
                                 Initial.CountRecords,
                                 Initial.CountRootSubjects,
                                 Initial.CountSeedSubjects,
                                 starts_with("PrimaryTableCleaning"),
                                 starts_with("TableNormalization"),
                                 starts_with("SecondaryTableCleaning"),
                                 starts_with("RecordSubsumption")) %>%
                          mutate(Final.CountRecords = RecordSubsumption.CountRecords,
                                 Final.CountRecords.Change = Final.CountRecords - Initial.CountRecords,
                                 Final.CountRecords.Change.Proportion = case_when(Initial.CountRecords > 0 ~ Final.CountRecords.Change / Initial.CountRecords,
                                                                                  .default = NA_real_),
                                 Final.CountRootSubjects = RecordSubsumption.CountRootSubjects,
                                 Final.CountRootSubjects.Change = Final.CountRootSubjects - Initial.CountRootSubjects,
                                 Final.CountRootSubjects.Change.Proportion = case_when(Initial.CountRootSubjects > 0 ~ Final.CountRootSubjects.Change / Initial.CountRootSubjects,
                                                                                       .default = NA_real_),
                                 Final.CountSeedSubjects = RecordSubsumption.CountSeedSubjects,
                                 Final.CountSeedSubjects.Change = Final.CountSeedSubjects - Initial.CountSeedSubjects,
                                 Final.CountSeedSubjects.Change.Proportion = case_when(Initial.CountSeedSubjects > 0 ~ Final.CountSeedSubjects.Change / Initial.CountSeedSubjects,
                                                                                       .default = NA_real_))


  # TABLE-LEVEL Counter summaries
  #------------------------------
  Counter.TableLevel <- Counter.Summary %>%
                            filter(Table != ".All") %>%
                            split(.$Table) %>%
                                map(\(X) X %>% select(-Table))


  # DATA-SET-LEVEL Counter summaries
  #---------------------------------

  # First, get server-specific data set table counts from CurationReports
  TableCount <- CurationReports %>%
                    map(\(ServerCurationReport) ServerCurationReport %>%
                                                          pluck("Counter", "Summary", "DataSetLevel") %>%
                                                          select(CountTables)) %>%
                    list_rbind(names_to = "Server") %>%
                    AddCumulativeRow()

  # Add table counts to Counter summary
  Counter.DataSetLevel <- Counter.Summary %>%
                              filter(Table == ".All") %>%
                              left_join(TableCount, by = join_by(Server)) %>%
                              select(-Table) %>%
                              relocate(CountTables, .before = 2)


# C 3) Extensive COUNTER data on sub-stage-level (details), no cumulation necessary
#-------------------------------------------------------------------------------

  # Compile relevant COUNTER DETAILS data in cumulated data.frame (all servers, all tables) and then split into table-specific data.frames
  Counter.Details <- CurationReports %>%
                          map(\(ServerCurationReport) ServerCurationReport %>%
                                                          pluck("Counter", "Extensive") %>%
                                                          map(\(TableCounter) TableCounter %>% pluck("Details")) %>%
                                                          list_rbind(names_to = "Table")) %>%
                          list_rbind(names_to = "Server") %>%
                          select(Table,
                                 Server,
                                 Timestamp,
                                 ProcessingStage,
                                 ProcessTopic,
                                 ProcessTopic.Subgroup,
                                 MessageClass,
                                 Message,
                                 CountRecords.Removed,
                                 CountRecords.Added,
                                 CountRootSubjects.Affected,
                                 CountSeedSubjects.Affected) %>%
                          filter(CountRecords.Removed > 0 | CountRecords.Added > 0) %>%
                          arrange(Timestamp) %>%
                          split(.$Server) %>%
                              map(\(X) X %>%
                                        select(-Server) %>%
                                        split(.$Table) %>%
                                            map(\(X) X %>% select(-Table)))


#===============================================================================
# D) Compilation of DATA HARMONIZATION Reports and Monitors
#===============================================================================
#   1) Data Harmonization Reports on data-set-level
#   2) Data Harmonization Reports on table-level
#   3) Data Harmonization Reports on feature-level
#   4) Data Harmonization Reports on value-level
#   5) Data Harmonization Stage-specific value sets
#   6) Data Harmonization Monitors on table-level
#   7) Data Harmonization Monitors on feature-level
#   8) Data Harmonization Monitors on value-level
#
#-------------------------------------------------------------------------------

# D 1) Data Harmonization REPORTS on DATA-SET-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific data-set-level data harmonization reports and row-bind them
  DataHarmonization.Report.DataSetLevel.Servers <- CurationReports %>%
                                                        map(\(X) X %>% pluck("DataHarmonization", "Reports", "DataSetLevel")) %>%
                                                        list_rbind(names_to = "Server")

  DataHarmonization.Report.DataSetLevel.Cumulated <- DataHarmonization.Report.DataSetLevel.Servers %>%
                                                          summarize(Server = "All",
                                                                    CountAffectedTables = max(CountAffectedTables),
                                                                    CountTrackedFeatures = max(CountTrackedFeatures),
                                                                    across(starts_with("CountValues"), ~ sum(.x, na.rm = FALSE)),
                                                                    ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Raw) | CountValues.Ineligible.Raw == 0,
                                                                                                         NA,
                                                                                                         CountValues.Harmonized / CountValues.Ineligible.Raw))

  DataHarmonization.Report.DataSetLevel <- DataHarmonization.Report.DataSetLevel.Cumulated %>%
                                                bind_rows(DataHarmonization.Report.DataSetLevel.Servers)


# D 2) Data Harmonization REPORTS on TABLE-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific table-level data harmonization reports and row-bind them
  DataHarmonization.Report.TableLevel.Servers <- CurationReports %>%
                                                      map(\(X) X %>% pluck("DataHarmonization", "Reports", "TableLevel")) %>%
                                                      list_rbind(names_to = "Server")

  DataHarmonization.Report.TableLevel.Cumulated <- DataHarmonization.Report.TableLevel.Servers %>%
                                                        group_by(Table) %>%
                                                        summarize(Server = "All",
                                                                  CountTrackedFeatures = max(CountTrackedFeatures),
                                                                  across(starts_with("CountValues"), ~ sum(.x, na.rm = FALSE)),
                                                                  ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Raw) | CountValues.Ineligible.Raw == 0,
                                                                                                       NA,
                                                                                                       CountValues.Harmonized / CountValues.Ineligible.Raw))

  DataHarmonization.Report.TableLevel <- DataHarmonization.Report.TableLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Report.TableLevel.Servers) %>%
                                              split(.$Table) %>%
                                                  map(\(X) X %>% select(-Table))


# D 3) Data Harmonization REPORTS on FEATURE-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific feature-level data harmonization reports and row-bind them
  DataHarmonization.Report.FeatureLevel.Servers <- CurationReports %>%
                                                        map(\(ServerCurationReport) ServerCurationReport %>%
                                                                                        pluck("DataHarmonization", "Reports", "FeatureLevel") %>%
                                                                                        list_rbind(names_to = "Table")) %>%
                                                        list_rbind(names_to = "Server")

  DataHarmonization.Report.FeatureLevel.Cumulated <- DataHarmonization.Report.FeatureLevel.Servers %>%
                                                          group_by(Table, Feature) %>%
                                                              summarize(Server = "All",
                                                                        across(starts_with("Count"), ~ sum(.x, na.rm = TRUE))) %>%
                                                          ungroup() %>%
                                                          mutate(ProportionValues.Harmonized = case_when(is.na(CountValues.Ineligible.Raw) | CountValues.Ineligible.Raw == 0 ~ NA,
                                                                                                         .default = CountValues.Harmonized / CountValues.Ineligible.Raw)) %>%
                                                          relocate(Server, .before = 1)

  DataHarmonization.Report.FeatureLevel <- DataHarmonization.Report.FeatureLevel.Cumulated %>%
                                                bind_rows(DataHarmonization.Report.FeatureLevel.Servers) %>%
                                                split(.$Table) %>%
                                                    map(\(X) X %>%
                                                              select(-Table) %>%
                                                              split(.$Feature) %>%
                                                                  map(\(X) X %>% select(-Feature)))


# D 4) Data Harmonization REPORTS on VALUE-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific value-level data harmonization reports and row-bind them
  DataHarmonization.Report.ValueLevel.Servers <- CurationReports %>%
                                                      map(\(ServerCurationReport) ServerCurationReport %>%
                                                                                      pluck("DataHarmonization", "Reports", "ValueLevel") %>%
                                                                                      list_rbind(names_to = "Table")) %>%
                                                      list_rbind(names_to = "Server")

  DataHarmonization.Report.ValueLevel.Cumulated <- DataHarmonization.Report.ValueLevel.Servers %>%
                                                        group_by(Table, Feature, Value.Raw) %>%
                                                            arrange(desc(IsOccurring), .by_group = TRUE) %>%      # Make sure that a server-specific row can be on top, where 'IsOccurring' is TRUE
                                                            summarize(Server = "All",
                                                                      Count = sum(Count, na.rm = TRUE),
                                                                      across(!Count, ~ .x[1])) %>%      # Take first row in group for all columns other than 'Count'
                                                                                                        # Note: This assumes that different harmonization results for different Servers are not possible. Possibly needs to be adjusted if non-deterministic data remediation methods are applied in the future.
                                                        ungroup() %>%
                                                        relocate(Server, .before = 1)

  DataHarmonization.Report.ValueLevel <- DataHarmonization.Report.ValueLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Report.ValueLevel.Servers) %>%
                                              split(.$Table) %>%
                                                    map(\(X) X %>%
                                                              select(-Table) %>%
                                                              split(.$Feature) %>%
                                                                  map(\(X) X %>%
                                                                            select(-Feature) %>%
                                                                            split(.$Value.Raw) %>%
                                                                                map(\(X) X %>% select(-Value.Raw))))


# D 5) Full Value Sets for all harmonization stages (Raw, Remediated, Recoded, Final)
#-------------------------------------------------------------------------------

  DataHarmonization.ValueSets.Servers <- CurationReports %>%
                                              map(\(ServerCurationReport) ServerCurationReport %>%
                                                                              pluck("DataHarmonization", "Reports", "ValueSets") %>%
                                                                              map(\(TableValueSets) TableValueSets %>%
                                                                                                        map(\(FeatureValueSet) FeatureValueSet %>%
                                                                                                                                    list_rbind(names_to = "HarmonizationStage")) %>%
                                                                                                        list_rbind(names_to = "Feature")) %>%
                                                                              list_rbind(names_to = "Table")) %>%
                                              list_rbind(names_to = "Server")

  DataHarmonization.ValueSets.Cumulated <- DataHarmonization.ValueSets.Servers %>%
                                                group_by(Table, Feature, HarmonizationStage, Value) %>%
                                                    arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                    summarize(Server = "All",
                                                              across(c(IsOccurring, IsEligible), ~ .x[1]),
                                                              Count = sum(Count, na.rm = TRUE)) %>%
                                                group_by(Table, Feature, HarmonizationStage) %>%
                                                    mutate(Proportion = { if (sum(Count, na.rm = TRUE) == 0) { NA } else { Count / sum(Count, na.rm = TRUE) } }) %>%
                                                ungroup() %>%
                                                relocate(Server, .before = 1)

  DataHarmonization.ValueSets <- DataHarmonization.ValueSets.Cumulated %>%
                                      bind_rows(DataHarmonization.ValueSets.Servers) %>%
                                      split(.$Server) %>%
                                                    map(\(X) X %>%
                                                              select(-Server) %>%
                                                              split(.$Table) %>%
                                                                  map(\(X) X %>%
                                                                            select(-Table) %>%
                                                                            split(.$Feature) %>%
                                                                                map(\(X) X %>%
                                                                                          select(-Feature) %>%
                                                                                          split(.$HarmonizationStage) %>%
                                                                                              map(\(X) X %>% select(-HarmonizationStage)))))


# D 6) Data Harmonization MONITORS on TABLE-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific table-level data harmonization monitors and row-bind them
  DataHarmonization.Monitor.TableLevel.Servers <- CurationReports %>%
                                                      map(\(ServerCurationReport) ServerCurationReport %>%
                                                                                      pluck("DataHarmonization", "Monitors", "TableLevel") %>%
                                                                                      list_rbind(names_to = "Table")) %>%
                                                      list_rbind(names_to = "Server")

  DataHarmonization.Monitor.TableLevel.Cumulated <- DataHarmonization.Monitor.TableLevel.Servers %>%
                                                        group_by(Table, Eligibility) %>%
                                                            summarize(Server = "All",
                                                                      across(starts_with("Count"), ~ sum(.x))) %>%
                                                            mutate(across(starts_with("Count."), ~ { if (sum(.x) == 0) NA else .x / sum(.x) },
                                                                          .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                            rename_with(~ str_replace(.x, ".Count.", "."), starts_with("Proportion.")) %>%
                                                        ungroup() %>%
                                                        relocate(Server, .before = 1)

  DataHarmonization.Monitor.TableLevel <- DataHarmonization.Monitor.TableLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Monitor.TableLevel.Servers) %>%
                                              split(.$Server) %>%
                                                  map(\(X) X %>%
                                                            select(-Server) %>%
                                                            split(.$Table) %>%
                                                                map(\(X) X %>% select(-Table)))


# D 7) Data Harmonization MONITORS on FEATURE-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific feature-level data harmonization monitors and row-bind them
  DataHarmonization.Monitor.FeatureLevel.Servers <- CurationReports %>%
                                                        map(\(ServerCurationReport) ServerCurationReport %>%
                                                                                        pluck("DataHarmonization", "Monitors", "FeatureLevel") %>%
                                                                                        list_rbind(names_to = "Table")) %>%
                                                        list_rbind(names_to = "Server")

  DataHarmonization.Monitor.FeatureLevel.Cumulated <- DataHarmonization.Monitor.FeatureLevel.Servers %>%
                                                          group_by(Table, Feature, Eligibility) %>%
                                                              summarize(Server = "All",
                                                                        across(starts_with("Count"), ~ sum(.x))) %>%
                                                              mutate(across(starts_with("Count."), ~ { if (sum(.x) == 0) NA else .x / sum(.x) },
                                                                            .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                              rename_with(~ str_replace(.x, ".Count.", "."), starts_with("Proportion.")) %>%
                                                          ungroup() %>%
                                                          relocate(Server, .before = 1)

  DataHarmonization.Monitor.FeatureLevel <- DataHarmonization.Monitor.FeatureLevel.Cumulated %>%
                                                bind_rows(DataHarmonization.Monitor.FeatureLevel.Servers) %>%
                                                    split(.$Server) %>%
                                                    map(\(X) X %>%
                                                              select(-Server) %>%
                                                              split(.$Table) %>%
                                                                  map(\(X) X %>%
                                                                            select(-Table) %>%
                                                                            split(.$Feature) %>%
                                                                                map(\(X) X %>% select(-Feature))))


# D 8) Data Harmonization MONITORS on VALUE-LEVEL
#-------------------------------------------------------------------------------

  # Get all server-specific value-level data harmonization monitors and row-bind them
  DataHarmonization.Monitor.ValueLevel.Servers <- CurationReports %>%
                                                      map(\(ServerCurationReport) ServerCurationReport %>%
                                                                                      pluck("DataHarmonization", "Monitors", "ValueLevel") %>%
                                                                                      list_rbind(names_to = "Table")) %>%
                                                      list_rbind(names_to = "Server")

  DataHarmonization.Monitor.ValueLevel.Cumulated <- DataHarmonization.Monitor.ValueLevel.Servers %>%
                                                        group_by(Table, Feature, Value.Raw) %>%
                                                            arrange(desc(IsOccurring), .by_group = TRUE) %>%      # Make sure that a server-specific row can be on top where 'IsOccurring' is TRUE
                                                            summarize(Server = "All",
                                                                      across(!starts_with("Count"), ~ .x[1]),      # Take first row in group for all columns other than the ones starting with 'Count'
                                                                                                                   # Note: This assumes that different harmonization paths for different Servers are not possible. Possibly needs to be adjusted if non-deterministic data remediation methods are applied in the future.
                                                                      across(starts_with("Count"), ~ sum(.x, na.rm = TRUE))) %>%
                                                        ungroup() %>%
                                                        relocate(Server, .before = 1)

  DataHarmonization.Monitor.ValueLevel <- DataHarmonization.Monitor.ValueLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Monitor.ValueLevel.Servers) %>%
                                              group_by(Server, Table) %>%
                                                  arrange(Feature,
                                                          desc(IsOccurring),
                                                          desc(IsEligible.Raw),
                                                          desc(IsEligible.Remediated),
                                                          Value.Raw,
                                                          .by_group = TRUE) %>%
                                              ungroup() %>%
                                              split(.$Server) %>%
                                                  map(\(X) X %>%
                                                            select(-Server) %>%
                                                            split(.$Table) %>%
                                                                map(\(X) X %>% select(-Table)))


#-------------------------------------------------------------------------------
  return(list(Log = Log,
              Counter = list(DataSetLevel = Counter.DataSetLevel,
                             TableLevel = Counter.TableLevel,
                             StageLevel = Counter.StageLevel,
                             Details = Counter.Details),
              DataHarmonization = list(Reports = list(DataSetLevel = DataHarmonization.Report.DataSetLevel,
                                                      TableLevel = DataHarmonization.Report.TableLevel,
                                                      FeatureLevel = DataHarmonization.Report.FeatureLevel,
                                                      ValueLevel = DataHarmonization.Report.ValueLevel,
                                                      ValueSets = DataHarmonization.ValueSets),
                                       Monitors = list(TableLevel = DataHarmonization.Monitor.TableLevel,
                                                       FeatureLevel = DataHarmonization.Monitor.FeatureLevel,
                                                       ValueLevel = DataHarmonization.Monitor.ValueLevel))))
}
