
#' ds.GetCurationReport
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Receive Curation Reports from servers and create cumulated reporting objects.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetReportingObjectDS()}
#'
#' @param Module Optional \code{string} identifying a defined data set and the corresponding meta data (Examples: 'CCP' / 'P21')
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of Curation Reports
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetCurationReport <- function(Module = "CCP",
                                 DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  if (!is.null(Module)) { assert_that(is.string(Module)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#===============================================================================
# A) Get CurationReport objects from servers (as a list of lists)
#===============================================================================

  CurationReports <- DSI::datashield.aggregate(conns = DSConnections,
                                               expr = call("GetReportingObjectDS",
                                                           ObjectName.S = paste0(Module, ".CurationReport")))

  # Turn returned list 'inside-out' using purrr::list_transpose() for easier processing
  CurationReports <- CurationReports %>% list_transpose()



#===============================================================================
# B) Compilation of Tracker data
#===============================================================================
#   1) Processing stage level
#   2) Sub-stage (details) level
#-------------------------------------------------------------------------------

# B 1) Tracker data on processing stage level
#-------------------------------------------------------------------------------

  # Compile tracker data in cumulated data.frame (all servers, all data set tables)
  Tracker.Stages.Cumulated <- CurationReports %>%
                                  map(\(ServerCurationReport) ServerCurationReport %>%
                                                                  pluck("Tracker") %>%
                                                                  map(\(TableTracker) TableTracker %>% pluck("Stages")) %>%
                                                                  list_rbind(names_to = "Table")) %>%
                                  list_rbind(names_to = "Server") %>%
                                  select(Server,
                                         ProcessingStage,
                                         Table,
                                         CountRecords.Post,
                                         Change.CountRecords,
                                         CountRootSubjects.Post) %>%
                                  rename(CountRecords = "CountRecords.Post",
                                         Change = "Change.CountRecords",
                                         CountRootSubjects = CountRootSubjects.Post) %>%
                                  mutate(ProcessingStage = str_remove_all(ProcessingStage, " ")) %>%
                                  pivot_wider(names_from = ProcessingStage,
                                              names_glue = "{ProcessingStage}.{.value}",
                                              values_from = !c(Server, ProcessingStage, Table)) %>%
                                  select(Server,
                                         Table,
                                         Initial.CountRecords,
                                         Initial.CountRootSubjects,
                                         starts_with("PrimaryTableCleaning"),
                                         starts_with("TableNormalization"),
                                         starts_with("SecondaryTableCleaning"),
                                         starts_with("RecordSubsumption")) %>%
                                  mutate(Final.CountRecords = RecordSubsumption.CountRecords,
                                         Final.ProportionRecords = if_else(!is.na(Initial.CountRecords) & Initial.CountRecords > 0,
                                                                           Final.CountRecords / Initial.CountRecords,
                                                                           NA),
                                         Final.CountRootSubjects = RecordSubsumption.CountRootSubjects)

  # Split cumulated tracker data.frame into list of table-specific tracker data.frames
  Tracker.Stages <- Tracker.Stages.Cumulated %>%
                        split(.$Table) %>%
                        map(\(TableTrackerData) TableTrackerData %>% select(-Table))


# B 2) Tracker data on sub-stage (details) level
#-------------------------------------------------------------------------------

  # Compile tracker data in cumulated data.frame (all servers, all data set tables)
  Tracker.Details.Cumulated <- CurationReports %>%
                                    map(\(ServerCurationReport) ServerCurationReport %>%
                                                                    pluck("Tracker") %>%
                                                                    map(\(TableTracker) TableTracker %>% pluck("Details")) %>%
                                                                    list_rbind(names_to = "Table")) %>%
                                    list_rbind(names_to = "Server") %>%
                                    select(Server,
                                           ProcessingStage,
                                           Table,
                                           Timestamp,
                                           MessageClass,
                                           Message,
                                           CountRecords.Removed,
                                           CountRecords.Added,
                                           CountRootSubjects.Affected) %>%
                                    filter(CountRecords.Removed > 0 | CountRecords.Added > 0)

  # Split cumulated tracker data.frame into list of table-specific tracker data.frames
  Tracker.Details <- Tracker.Details.Cumulated %>%
                          split(.$Table) %>%
                          map(\(TableTrackerData) TableTrackerData %>% select(-Table) %>% arrange(Timestamp))


#===============================================================================
# C) Transformation objects
#===============================================================================
#   i) Detailed monitors
#   ii) Eligibility overviews
#   iii) Value set overviews
#---------------------------------------------------------------------------

  TransformationMonitorsCumulated <- list()
  EligibilityOverviewsCumulated <- list()
  ValueSetOverviewsCumulated <- list()

  for (i in 1:length(CurationReports$Transformation[[1]]$Monitors))      # Loop through all transformation monitor tables (Diagnosis, Histology, etc.)
  {
      AllServersMonitor <- data.frame()

      AllServersEligibilityOverview <- data.frame()

      AllServersValueSetOverview.Raw <- data.frame()
      AllServersValueSetOverview.Harmonized <- data.frame()
      AllServersValueSetOverview.Recoded <- data.frame()
      AllServersValueSetOverview.Final <- data.frame()


      # 1) Row-bind data frames from different Servers
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (j in 1:length(DSConnections))      # Loop through all Servers
      {
          ServerMonitor <- CurationReports$Transformation[[j]]$Monitors[[i]]
          ServerEligibilityOverview <- CurationReports$Transformation[[j]]$EligibilityOverviews[[i]]

          ServerValueSetOverview.Raw <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Raw
          ServerValueSetOverview.Harmonized <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Harmonized
          ServerValueSetOverview.Recoded <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Recoded
          ServerValueSetOverview.Final <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Final

          # Monitor table
          if (!is.null(ServerMonitor))
          {
              ServerMonitor <- ServerMonitor %>%
                                  mutate(TemporaryServerID = j)      # Create temporary Server ID for processing

              # Row-bind all Server-specific transformation monitor tables
              AllServersMonitor <- AllServersMonitor %>%
                                      bind_rows(ServerMonitor)
          }


          # Eligibility overview
          AllServersEligibilityOverview <- AllServersEligibilityOverview %>%
                                              bind_rows(ServerEligibilityOverview)


          # Value set overview (separate for different transformation stages)
          AllServersValueSetOverview.Raw <- AllServersValueSetOverview.Raw %>%
                                                bind_rows(ServerValueSetOverview.Raw)

          AllServersValueSetOverview.Harmonized <- AllServersValueSetOverview.Harmonized %>%
                                                        bind_rows(ServerValueSetOverview.Harmonized)

          AllServersValueSetOverview.Recoded <- AllServersValueSetOverview.Recoded %>%
                                                    bind_rows(ServerValueSetOverview.Recoded)

          AllServersValueSetOverview.Final <- AllServersValueSetOverview.Final %>%
                                                  bind_rows(ServerValueSetOverview.Final)
      }


      # 2) i) Consolidate cumulated detailed monitor tables
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (nrow(AllServersMonitor) > 0)      # In case 'AllServersMonitor' is not empty
      {
          # Get summarized counts of raw values
          SummaryRawValues <- AllServersMonitor %>%
                                  filter(IsOccurring == TRUE) %>%
                                  distinct(pick(TemporaryServerID,      # This makes sure that per Server exactly one (and only one) 'instance' of a particular value is counted
                                                Feature,
                                                Value.Raw,
                                                Count.Raw)) %>%
                                  group_by(Feature,
                                           Value.Raw) %>%
                                  summarize(Count.Raw = sum(Count.Raw, na.rm = TRUE))

          # Get summarized counts of harmonized values
          SummaryHarmonizedValues <- AllServersMonitor %>%
                                          distinct(pick(TemporaryServerID,
                                                        Feature,
                                                        Value.Harmonized,
                                                        Count.Harmonized)) %>%
                                          group_by(Feature,
                                                   Value.Harmonized) %>%
                                          summarize(Count.Harmonized = sum(Count.Harmonized, na.rm = TRUE))

          # Get summarized counts of recoded values
          SummaryRecodedValues <- AllServersMonitor %>%
                                      distinct(pick(TemporaryServerID,
                                                    Feature,
                                                    Value.Recoded,
                                                    Count.Recoded)) %>%
                                      group_by(Feature,
                                               Value.Recoded) %>%
                                      summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE))

          # Get summarized counts of final values
          SummaryFinalValues <- AllServersMonitor %>%
                                    distinct(pick(TemporaryServerID,
                                                  Feature,
                                                  Value.Final,
                                                  Count.Final)) %>%
                                    group_by(Feature,
                                             Value.Final) %>%
                                    summarize(Count.Final = sum(Count.Final, na.rm = TRUE))


          AllServersMonitor <- AllServersMonitor %>%
                                  select(-TemporaryServerID,
                                         -Count.Raw,
                                         -Count.Harmonized,
                                         -Count.Recoded,
                                         -Count.Final) %>%
                                  distinct() %>%
                                  #--- Delete remnant values marked as non-occurring that actually occur on some Server ---
                                  group_by(Feature, Value.Raw) %>%
                                      arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                      slice_head() %>%
                                  ungroup() %>%
                                  #--- Add cumulated value counts of different transformation stages ---
                                  left_join(SummaryRawValues, by = join_by(Feature, Value.Raw)) %>%
                                  left_join(SummaryHarmonizedValues, by = join_by(Feature, Value.Harmonized)) %>%
                                  left_join(SummaryRecodedValues, by = join_by(Feature, Value.Recoded)) %>%
                                  left_join(SummaryFinalValues, by = join_by(Feature, Value.Final)) %>%
                                  arrange(Feature,
                                          desc(IsOccurring),
                                          desc(IsEligible.Raw),
                                          desc(IsEligible.Harmonized),
                                          Value.Raw)
      }

      TransformationMonitorsCumulated <- c(TransformationMonitorsCumulated,
                                           list(AllServersMonitor))


      # 2) ii) Consolidate cumulated eligibility overview tables
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (nrow(AllServersEligibilityOverview) > 0)
      {
          AllServersEligibilityOverview <- AllServersEligibilityOverview %>%
                                              select(Feature,
                                                     Eligibility,
                                                     Raw,
                                                     Harmonized,
                                                     Recoded,
                                                     Final) %>%
                                              group_by(Feature, Eligibility) %>%
                                                  summarize(across(c(Raw, Harmonized, Recoded, Final), ~ sum(.x, na.rm = TRUE))) %>%
                                              group_by(Feature) %>%
                                                  mutate(across(c(Raw, Harmonized, Recoded, Final), ~ .x / sum(.x, na.rm = TRUE), .names = "{.col}.Proportional")) %>%
                                              ungroup()
      }

      EligibilityOverviewsCumulated <- c(EligibilityOverviewsCumulated,
                                         list(AllServersEligibilityOverview))


      # 2) iii) Consolidate cumulated value set overview tables
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (nrow(AllServersValueSetOverview.Raw) > 0)
      {
          AllServersValueSetOverview.Raw <- AllServersValueSetOverview.Raw %>%
                                                group_by(Feature, Value.Raw, IsOccurring, IsEligible.Raw) %>%
                                                    summarize(Count.Raw = sum(Count.Raw, na.rm = TRUE)) %>%
                                                    arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                    slice_head() %>%      # Remove / Replace values marked as non-occurring
                                                group_by(Feature) %>%
                                                    mutate(Proportion.Raw = Count.Raw / sum(Count.Raw, na.rm = TRUE)) %>%
                                                ungroup()
      }

      if (nrow(AllServersValueSetOverview.Harmonized) > 0)
      {
          AllServersValueSetOverview.Harmonized <- AllServersValueSetOverview.Harmonized %>%
                                                        group_by(Feature, Value.Harmonized, IsEligible.Harmonized) %>%
                                                            summarize(Count.Harmonized = sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                        group_by(Feature) %>%
                                                            mutate(Proportion.Harmonized = Count.Harmonized / sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                        ungroup()
      }

      if (nrow(AllServersValueSetOverview.Recoded) > 0)
      {
          AllServersValueSetOverview.Recoded <- AllServersValueSetOverview.Recoded %>%
                                                    group_by(Feature, Value.Recoded, IsEligible.Recoded) %>%
                                                        summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                    group_by(Feature) %>%
                                                        mutate(Proportion.Recoded = Count.Recoded / sum(Count.Recoded, na.rm = TRUE)) %>%
                                                    ungroup()
      }

      if (nrow(AllServersValueSetOverview.Final) > 0)
      {
          AllServersValueSetOverview.Final <- AllServersValueSetOverview.Final %>%
                                                  group_by(Feature, Value.Final, IsEligible.Final) %>%
                                                      summarize(Count.Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                  group_by(Feature) %>%
                                                      mutate(Proportion.Final = Count.Final / sum(Count.Final, na.rm = TRUE)) %>%
                                                  ungroup()
      }

      ValueSetOverviewsCumulated <- c(ValueSetOverviewsCumulated,
                                      list(Raw = AllServersValueSetOverview.Raw,
                                           Harmonized = AllServersValueSetOverview.Harmonized,
                                           Recoded = AllServersValueSetOverview.Recoded,
                                           Final = AllServersValueSetOverview.Final))
  }

  names(TransformationMonitorsCumulated) <- names(CurationReports$Transformation[[1]]$Monitors)
  names(EligibilityOverviewsCumulated) <- names(CurationReports$Transformation[[1]]$EligibilityOverviews)
  names(TransformationMonitorsCumulated) <- names(CurationReports$Transformation[[1]]$ValueSetOverviews)


#-------------------------------------------------------------------------------
  return(list(RecordCounts = RecordCounts,
              Transformation = c(list(All = list(Monitors = TransformationMonitorsCumulated,
                                                 EligibilityOverviews = EligibilityOverviewsCumulated,
                                                 ValueSetOverviews = ValueSetOverviewsCumulated)),
                                 CurationReports$Transformation)))
}
