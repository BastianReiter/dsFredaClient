
#' ds.GetCurationReport
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Receive Curation Reports from servers and create cumulated reporting objects.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetReportingObjectDS()}
#'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of Curation Reports
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetCurationReport <- function(DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)

  # --- For Testing Purposes ---
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 1) Get CurationReport objects from servers (as a list of lists)
#-------------------------------------------------------------------------------

  CurationReports <- DSI::datashield.aggregate(conns = DSConnections,
                                               expr = call("GetReportingObjectDS",
                                                           ObjectName.S = "CurationReport"))

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
                        mutate(ExcludedPrimary_Proportion = ExcludedPrimary / InitialCount,
                               AfterPrimaryExclusion_Proportion = AfterPrimaryExclusion / InitialCount,
                               ExcludedSecondary_Proportion = ExcludedSecondary / InitialCount,
                               AfterSecondaryExclusion_Proportion = AfterSecondaryExclusion / InitialCount,
                               ExcludedSecondaryRedundancy_Proportion = ExcludedSecondaryRedundancy / InitialCount,
                               AfterSecondaryRedundancyExclusion_Proportion = AfterSecondaryRedundancyExclusion / InitialCount) %>%
                        select(Table,
                               Server,
                               InitialCount,
                               ExcludedPrimary,
                               ExcludedPrimary_Proportion,
                               AfterPrimaryExclusion,
                               AfterPrimaryExclusion_Proportion,
                               ExcludedSecondary,
                               ExcludedSecondary_Proportion,
                               AfterSecondaryExclusion,
                               AfterSecondaryExclusion_Proportion,
                               ExcludedSecondaryRedundancy,
                               ExcludedSecondaryRedundancy_Proportion,
                               AfterSecondaryRedundancyExclusion,
                               AfterSecondaryRedundancyExclusion_Proportion)

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

      AllServersValueSetOverview_Raw <- data.frame()
      AllServersValueSetOverview_Harmonized <- data.frame()
      AllServersValueSetOverview_Recoded <- data.frame()
      AllServersValueSetOverview_Final <- data.frame()


      # 1) Row-bind data frames from different Servers
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (j in 1:length(DSConnections))      # Loop through all Servers
      {
          ServerMonitor <- CurationReports$Transformation[[j]]$Monitors[[i]]
          ServerEligibilityOverview <- CurationReports$Transformation[[j]]$EligibilityOverviews[[i]]

          ServerValueSetOverview_Raw <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Raw
          ServerValueSetOverview_Harmonized <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Harmonized
          ServerValueSetOverview_Recoded <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Recoded
          ServerValueSetOverview_Final <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Final

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
          AllServersValueSetOverview_Raw <- AllServersValueSetOverview_Raw %>%
                                              bind_rows(ServerValueSetOverview_Raw)

          AllServersValueSetOverview_Harmonized <- AllServersValueSetOverview_Harmonized %>%
                                                      bind_rows(ServerValueSetOverview_Harmonized)

          AllServersValueSetOverview_Recoded <- AllServersValueSetOverview_Recoded %>%
                                                  bind_rows(ServerValueSetOverview_Recoded)

          AllServersValueSetOverview_Final <- AllServersValueSetOverview_Final %>%
                                                bind_rows(ServerValueSetOverview_Final)
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
                                                Value_Raw,
                                                Count_Raw)) %>%
                                  group_by(Feature,
                                           Value_Raw) %>%
                                  summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE))

          # Get summarized counts of harmonized values
          SummaryHarmonizedValues <- AllServersMonitor %>%
                                          distinct(pick(TemporaryServerID,
                                                        Feature,
                                                        Value_Harmonized,
                                                        Count_Harmonized)) %>%
                                          group_by(Feature,
                                                   Value_Harmonized) %>%
                                          summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE))

          # Get summarized counts of recoded values
          SummaryRecodedValues <- AllServersMonitor %>%
                                      distinct(pick(TemporaryServerID,
                                                    Feature,
                                                    Value_Recoded,
                                                    Count_Recoded)) %>%
                                      group_by(Feature,
                                               Value_Recoded) %>%
                                      summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE))

          # Get summarized counts of final values
          SummaryFinalValues <- AllServersMonitor %>%
                                    distinct(pick(TemporaryServerID,
                                                  Feature,
                                                  Value_Final,
                                                  Count_Final)) %>%
                                    group_by(Feature,
                                             Value_Final) %>%
                                    summarize(Count_Final = sum(Count_Final, na.rm = TRUE))


          AllServersMonitor <- AllServersMonitor %>%
                                  select(-TemporaryServerID,
                                         -Count_Raw,
                                         -Count_Harmonized,
                                         -Count_Recoded,
                                         -Count_Final) %>%
                                  distinct() %>%
                                  #--- Delete remnant values marked as non-occurring that actually occur on some Server ---
                                  group_by(Feature, Value_Raw) %>%
                                      arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                      slice_head() %>%
                                  ungroup() %>%
                                  #--- Add cumulated value counts of different transformation stages ---
                                  left_join(SummaryRawValues, by = join_by(Feature, Value_Raw)) %>%
                                  left_join(SummaryHarmonizedValues, by = join_by(Feature, Value_Harmonized)) %>%
                                  left_join(SummaryRecodedValues, by = join_by(Feature, Value_Recoded)) %>%
                                  left_join(SummaryFinalValues, by = join_by(Feature, Value_Final)) %>%
                                  arrange(Feature,
                                          desc(IsOccurring),
                                          desc(IsEligible_Raw),
                                          desc(IsEligible_Harmonized),
                                          Value_Raw)
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
                                                  mutate(across(c(Raw, Harmonized, Recoded, Final), ~ .x / sum(.x, na.rm = TRUE), .names = "{.col}_Proportional")) %>%
                                              ungroup()
      }

      EligibilityOverviewsCumulated <- c(EligibilityOverviewsCumulated,
                                         list(AllServersEligibilityOverview))


      # 2) iii) Consolidate cumulated value set overview tables
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (nrow(AllServersValueSetOverview_Raw) > 0)
      {
          AllServersValueSetOverview_Raw <- AllServersValueSetOverview_Raw %>%
                                                group_by(Feature, Value_Raw, IsOccurring, IsEligible_Raw) %>%
                                                    summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE)) %>%
                                                    arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                    slice_head() %>%      # Remove / Replace values marked as non-occurring
                                                group_by(Feature) %>%
                                                    mutate(Proportion_Raw = Count_Raw / sum(Count_Raw, na.rm = TRUE)) %>%
                                                ungroup()
      }

      if (nrow(AllServersValueSetOverview_Harmonized) > 0)
      {
          AllServersValueSetOverview_Harmonized <- AllServersValueSetOverview_Harmonized %>%
                                                        group_by(Feature, Value_Harmonized, IsEligible_Harmonized) %>%
                                                            summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                        group_by(Feature) %>%
                                                            mutate(Proportion_Harmonized = Count_Harmonized / sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                        ungroup()
      }

      if (nrow(AllServersValueSetOverview_Recoded) > 0)
      {
          AllServersValueSetOverview_Recoded <- AllServersValueSetOverview_Recoded %>%
                                                    group_by(Feature, Value_Recoded, IsEligible_Recoded) %>%
                                                        summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE)) %>%
                                                    group_by(Feature) %>%
                                                        mutate(Proportion_Recoded = Count_Recoded / sum(Count_Recoded, na.rm = TRUE)) %>%
                                                    ungroup()
      }

      if (nrow(AllServersValueSetOverview_Final) > 0)
      {
          AllServersValueSetOverview_Final <- AllServersValueSetOverview_Final %>%
                                                  group_by(Feature, Value_Final, IsEligible_Final) %>%
                                                      summarize(Count_Final = sum(Count_Final, na.rm = TRUE)) %>%
                                                  group_by(Feature) %>%
                                                      mutate(Proportion_Final = Count_Final / sum(Count_Final, na.rm = TRUE)) %>%
                                                  ungroup()
      }

      ValueSetOverviewsCumulated <- c(ValueSetOverviewsCumulated,
                                      list(Raw = AllServersValueSetOverview_Raw,
                                           Harmonized = AllServersValueSetOverview_Harmonized,
                                           Recoded = AllServersValueSetOverview_Recoded,
                                           Final = AllServersValueSetOverview_Final))
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
