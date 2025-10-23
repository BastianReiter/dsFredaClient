
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

#-------------------------------------------------------------------------------
# 1) Get CurationReport objects from servers (as a list of lists)
#-------------------------------------------------------------------------------

  CurationReports <- DSI::datashield.aggregate(conns = DSConnections,
                                               expr = call("GetReportingObjectDS",
                                                           ObjectName.S = paste0(Module, ".CurationReport")))

  # Turn returned list 'inside-out' using purrr::list_transpose() for easier processing
  CurationReports <- CurationReports %>% list_transpose()



#-------------------------------------------------------------------------------
# 2) Cumulation of server-specific reports
#-------------------------------------------------------------------------------
#   a) Entry Counts
#   b) Transformation Monitor objects
#         i) Detailed monitors
#         ii) Eligibility overviews
#         iii) Value set overviews
#   c) Diagnosis classification
#-------------------------------------------------------------------------------


# 2 a) Entry Counts
#-------------------------------------------------------------------------------

  AllServersEntryCounts <- data.frame()

  # Row-bind data frames from different servers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (j in 1:length(DSConnections))      # Loop through all servers
  {
      # Get Server-specific entry count table and attach column with server name
      ServerEntryCounts <- CurationReports$EntryCounts[[j]] %>%
                              mutate(Server = names(DSConnections)[j])

      # Row-bind all Server-specific entry count tables
      AllServersEntryCounts <- AllServersEntryCounts %>%
                                  bind_rows(ServerEntryCounts)
  }

  # Create table of cumulated entry counts
  if (nrow(AllServersEntryCounts) > 0)
  {
      EntryCountsCumulated <- AllServersEntryCounts %>%
                                  group_by(Table) %>%
                                      summarize(across(c(everything(), -Server), sum)) %>%
                                  ungroup() %>%
                                  mutate(Server = "All")

  } else { EntryCountsCumulated <- data.frame() }

  # Row-binding Server-specific and cumulated entry counts to get one coherent data.frame
  AllEntryCounts <- EntryCountsCumulated %>%
                        bind_rows(AllServersEntryCounts) %>%
                        mutate(ExcludedPrimary.Proportion = ExcludedPrimary / InitialCount,
                               AfterPrimaryExclusion.Proportion = AfterPrimaryExclusion / InitialCount,
                               ExcludedSecondary.Proportion = ExcludedSecondary / InitialCount,
                               AfterSecondaryExclusion.Proportion = AfterSecondaryExclusion / InitialCount,
                               ExcludedSecondaryRedundancy.Proportion = ExcludedSecondaryRedundancy / InitialCount,
                               AfterSecondaryRedundancyExclusion.Proportion = AfterSecondaryRedundancyExclusion / InitialCount) %>%
                        select(Table,
                               Server,
                               InitialCount,
                               ExcludedPrimary,
                               ExcludedPrimary.Proportion,
                               AfterPrimaryExclusion,
                               AfterPrimaryExclusion.Proportion,
                               ExcludedSecondary,
                               ExcludedSecondary.Proportion,
                               AfterSecondaryExclusion,
                               AfterSecondaryExclusion.Proportion,
                               ExcludedSecondaryRedundancy,
                               ExcludedSecondaryRedundancy.Proportion,
                               AfterSecondaryRedundancyExclusion,
                               AfterSecondaryRedundancyExclusion.Proportion)

  # Create list of data.frames (one per RDS table) containing data on entry counts, comparing all Servers
  EntryCounts <- split(AllEntryCounts, AllEntryCounts$Table) %>%
                      imap(function(Table, tablename)
                           {
                              Table %>% select(-Table)
                           })


  # 2 b) Transformation objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  return(list(EntryCounts = EntryCounts,
              Transformation = c(list(All = list(Monitors = TransformationMonitorsCumulated,
                                                 EligibilityOverviews = EligibilityOverviewsCumulated,
                                                 ValueSetOverviews = ValueSetOverviewsCumulated)),
                                 CurationReports$Transformation)))
}
