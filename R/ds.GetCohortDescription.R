
#' ds.GetCohortDescription
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Obtain Server-specific and aggregated descriptive characteristics about the patient cohort to be analyzed.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetCohortDescriptionDS()}.
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet" - Default: "AugmentedDataSet"
#' @param CCPDataSetType \code{string} - Indicating the type of CCP data set that should be described, one of "RDS" / "CDS" / "ADS" - Default: "ADS"
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing descriptive statistics characterizing patient cohort:
#'         \itemize{\item CohortSize
#'                  \item CohortSizeOverTime
#'                  \item AgeDistribution
#'                  \item SexDistribution}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetCohortDescription <- function(DataSetName = "AugmentedDataSet",
                                    CCPDataSetType = "ADS",
                                    DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)

  #--- For Testing Purposes ---
  # DataSetName <- "AugmentedDataSet"
  # CCPDataSetType <- "ADS"
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Server returns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ServerReturns: Obtain descriptive data for each server calling dsCCPhos::GetCohortDescriptionDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                           expr = call("GetCohortDescriptionDS",
                                                       DataSetName.S = DataSetName,
                                                       CCPDataSetType.S = CCPDataSetType))


  # Transpose list (turning 'inside-out') for easier processing
  ServerReturns <- ServerReturns %>% list_transpose(simplify = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cohort Size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cohort Size Summary (Cumulated Patient and Diagnosis Count)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  CohortSize_Servers <- ServerReturns$CohortSize %>%
                            list_rbind(names_to = "Server")

  CohortSize_All <- CohortSize_Servers %>%
                        summarize(PatientCount = sum(PatientCount),
                                  DiagnosisCount = sum(DiagnosisCount)) %>%
                        mutate(Server = "All",
                               DiagnosesPerPatient = DiagnosisCount / PatientCount)

  CohortSize <- CohortSize_Servers %>%
                    bind_rows(CohortSize_All)


# Cohort Size Time Series
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Create coherent data.frame with Server-specific data
  CohortSize_OverTime_Servers <- ServerReturns$CohortSize_OverTime %>%
                                    list_rbind(names_to = "Server")

  # Get cumulated values
  CohortSize_OverTime_All <- CohortSize_OverTime_Servers %>%
                                  group_by(DiagnosisYear) %>%
                                      summarize(across(c(PatientCount, DiagnosisCount), ~ sum(.x, na.rm = TRUE))) %>%
                                  ungroup() %>%
                                  mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
                                         Server = "All")

  # # Get time-point-specific median values across Servers
  # CohortSize_OverTime_Mean <- CohortSize_OverTime_Servers %>%
  #                                   group_by(DiagnosisYear) %>%
  #                                       summarize(across(c(PatientCount, DiagnosisCount), ~ mean(.x, na.rm = TRUE))) %>%
  #                                   ungroup() %>%
  #                                   mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
  #                                          Server = "Mean")
  #
  # # Get time-point-specific median values across Servers
  # CohortSize_OverTime_Median <- CohortSize_OverTime_Servers %>%
  #                                   group_by(DiagnosisYear) %>%
  #                                       summarize(across(c(PatientCount, DiagnosisCount), ~ round(median(.x, na.rm = TRUE)))) %>%
  #                                   ungroup() %>%
  #                                   mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
  #                                          Server = "Median")

  # Row-bind Server-specific and cumulated data
  CohortSize_OverTime <- CohortSize_OverTime_Servers %>%
                              bind_rows(CohortSize_OverTime_All)
                              # bind_rows(CohortSize_OverTime_Mean) %>%
                              # bind_rows(CohortSize_OverTime_Median)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  AgeDistribution_Servers <- ServerReturns$Age %>%
                                list_rbind(names_to = "Server")

  AgeDistribution_All <- AgeDistribution_Servers %>%
                              group_by(AgeGroup) %>%
                                  summarize(N = sum(N)) %>%
                              ungroup() %>%
                              mutate(Server = "All",
                                     Proportion = N / sum(N))

  AgeDistribution <- AgeDistribution_Servers %>%
                          bind_rows(AgeDistribution_All)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SexDistribution_Servers <- ServerReturns$Sex %>%
                                  list_rbind(names_to = "Server")

  SexDistribution_All <- SexDistribution_Servers %>%
                              group_by(Sex) %>%
                                  summarize(N = sum(N)) %>%
                              ungroup() %>%
                              mutate(Server = "All",
                                     Proportion = N / sum(N))

  SexDistribution <- SexDistribution_Servers %>%
                          bind_rows(SexDistribution_All)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(CohortSize = CohortSize,
              CohortSize_OverTime = CohortSize_OverTime,
              AgeDistribution = AgeDistribution,
              SexDistribution = SexDistribution))
}
