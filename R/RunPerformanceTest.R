
#' RunPerformanceTest
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Perform multiple data preprocessing runs of FREDA functionality while tracking performance measures.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param Module \code{string} - Identifying a registered FREDA module (Examples: 'CCP' / 'P21')
#' @param ScenarioA.Runs \code{integer} - Number of runs through performance test scenario A
#' @param ScenarioA.SampleSizes \code{integer}
#' @param ScenarioA.SampleSizes.Shuffle \code{logical} - Whether to shuffle sample size numbers between runs in order to mitigate possible sequence position effects - Default: \code{TRUE}
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of report objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RunPerformanceTest <- function(ServerSpecifications,
                               Module = "CCP",
                               ScenarioA.Runs = 1,
                               ScenarioA.ServerCount = nrow(ServerSpecifications),
                               ScenarioA.SampleSizes = c(125, 250, 500, 1000, 2000),
                               ScenarioA.SampleSizes.Shuffle = TRUE,
                               ScenarioB.Runs = 1,
                               ScenarioB.SequenceServerCount = 1:nrow(ServerSpecifications),
                               ScenarioB.SampleSize = 500,
                               CreateLogFiles = FALSE,
                               DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ScenarioA.Runs <- 2
  # ScenarioA.SampleSizes = c(125, 250)   #, 500, 1000, 2000)
  # ScenarioA.SampleSizes.Shuffle <- TRUE
  # ScenarioB.Runs <- 2
  # ScenarioB.SequenceServerCount <- 1:3
  # ScenarioB.SampleSize <- 100
  # CreateLogFiles <- TRUE
  # DS.async <- FALSE
  # #---
  # ScenarioA.CCPConnections <- CCPConnections
  # rm(CCPConnections)

  # --- Argument Validation ---
  assert_that(is.flag(DS.async))
  if (!is.null(ServerSpecifications)) { is.data.frame(ServerSpecifications) }

#-------------------------------------------------------------------------------

  # Set up log file name
  if (CreateLogFiles == TRUE)
  {
      Timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      LogFile.ScenarioA <- paste0("PerformanceTest_ScenarioA_", Timestamp, ".txt")
      LogFile.ScenarioA.ServerSpecific <- paste0("PerformanceTest_ScenarioA_ServerSpecific_", Timestamp, ".txt")
      LogFile.ScenarioB <- paste0("PerformanceTest_ScenarioB_", Timestamp, ".txt")
      LogFile.ScenarioB.ServerSpecific <- paste0("PerformanceTest_ScenarioB_ServerSpecific_", Timestamp, ".txt")

      #if (file.exists(LogFile)) { file.remove(LogFile) }
  }


  # Set up function that carries out a single processing run, with times being tracked along the way
  SingleRun <- function(SingleRun.SampleSize,
                        SingleRun.DSConnections)
  {
      Tracker <- tibble(SampleSize = SingleRun.SampleSize)

      #---  Draw SDS Samples  --------------------------------------------------
      ds.CCP.DrawSample(SourceDataSetName = "CCP.SourceDataSet",
                        SampleSize = SingleRun.SampleSize,
                        SampleName = "SDSSample",
                        DSConnections = SingleRun.DSConnections)

      Tracker$Time.AfterSampleDrawing <- Sys.time()


      #---  Data Refinement  -----------------------------------------------------
      # ds.RefineData(SourceDataSetName = "SDSSample",
      #               Module = "CCP",
      #               OutputName = "CCP.RefinementOutput",
      #               RunAssignmentChecks = FALSE,
      #               UnpackRefinedDataSet = FALSE,
      #               RunSeparately = FALSE,
      #               DSConnections = SingleRun.DSConnections)

      # Trigger Refinement on servers
      DSI::datashield.assign(conns = SingleRun.DSConnections,
                             symbol = "CCP.RefinementOutput",
                             value = call("RefineDataDS",
                                          SourceDataSetName.S = "SDSSample",
                                          Module.S = "CCP"),
                             async = DS.async)

      Tracker$Time.AfterRefinement <- Sys.time()

      # Get Refinement messages (more lightweight then full Refinement Report)
      # First, extract Refinement messages from Refinement output on servers
      DSI::datashield.assign(conns = SingleRun.DSConnections,
                             symbol = "Messages",
                             value = call("ExtractFromListDS",
                                           ListName.S = "CCP.RefinementOutput",
                                           ObjectName.S = "Messages"),
                             async = DS.async)

      ServerProcessInfo <- DSI::datashield.aggregate(conns = SingleRun.DSConnections,
                                                    expr = call("GetReportingObjectDS",
                                                                ObjectName.S = "Messages"),
                                                    async = DS.async) %>%
                                tibble::enframe(name = "Server",
                                                value = "RefinementMessages") %>%
                                tidyr::unnest_wider(RefinementMessages) %>%
                                select(Server, Process.Start, Process.End) %>%
                                arrange(Process.Start) %>%
                                mutate(Process.Duration = as.numeric(difftime(Process.End, Process.Start, units = "secs")),
                                       PreviousProcess.End = lag(Process.End),
                                       Interval = as.numeric(difftime(Process.Start, PreviousProcess.End, units = "secs")))


      ServerSystemSpecs <- DSI::datashield.aggregate(conns = SingleRun.DSConnections,
                                                     expr = call("GetSystemSpecsDS"),
                                                     async = DS.async) %>%
                                tibble::enframe(name = "Server",
                                                value = "SystemSpecs") %>%
                                tidyr::unnest_wider(SystemSpecs)

      Tracker.ServerSpecific <- ServerProcessInfo %>%
                                    left_join(ServerSystemSpecs, by = join_by(Server)) %>%
                                    mutate(SampleSize = SingleRun.SampleSize)

      ProcessSummary <- Tracker.ServerSpecific %>%
                            mutate(ProcessGapTime = case_when(Interval > 0 ~ Interval,
                                                              .default = 0)) %>%
                            summarize(Duration.Refinement.TotalGapTime = sum(ProcessGapTime),
                                      Duration.Refinement.Total = as.numeric(difftime(max(Process.End, na.rm = TRUE), min(Process.Start, na.rm = TRUE), units = "secs")),
                                      Duration.Refinement.Total.WithoutGapTime = Duration.Refinement.Total - Duration.Refinement.TotalGapTime,
                                      CPUCount.Median = median(CPU.Count, na.rm = TRUE),
                                      RAM.Total.Median = median(RAM.Total, na.rm = TRUE),
                                      RAM.Available.Median = median(RAM.Available, na.rm = TRUE))

      Tracker <- Tracker %>%
                      bind_cols(ProcessSummary)

      return(list(Summary = Tracker,
                  ServerSpecific = Tracker.ServerSpecific))
  }


#===============================================================================
# Test scenario A)
#   - All servers
#   - Varying sample sizes
#===============================================================================

  Results.A <- tibble()
  Results.A.ServerSpecific <- tibble()

  if (ScenarioA.Runs > 0)
  {
      for (i in 1:ScenarioA.Runs)
      {
          PrintSoloMessage(c(Special = paste0("Scenario A: Started Run ", i, " / ", ScenarioA.Runs)))

          Results.A.CurrentRun <- tibble(Run = i,
                                         Time.StartRun = Sys.time())

          # Connect to CCP servers
          ScenarioA.CCPConnections <- dsCCPhosClient::ConnectToCCP(ServerSpecifications = ServerSpecifications)

          Results.A.CurrentRun$NumberOfServers <- length(ScenarioA.CCPConnections)
          Results.A.CurrentRun$Time.AfterLogin <- Sys.time()

          #---  Load Source Data Set  ---------------------------------------------
          CCP.LoadSourceDataSet(ServerSpecifications = ServerSpecifications,
                             DSConnections = ScenarioA.CCPConnections)

          Results.A.CurrentRun$Time.AfterLoading <- Sys.time()

          # Shuffle sample size vector
          if (ScenarioA.SampleSizes.Shuffle == TRUE) { ScenarioA.SampleSizes.Shuffle <- sample(ScenarioA.SampleSizes.Shuffle) }

          Tracker.ScenarioA <- tibble()
          Tracker.ScenarioA.ServerSpecific <- tibble()

          #---  Run single processing run with specific sample size ------------
          for (CurrentSampleSize in ScenarioA.SampleSizes)
          {
              PrintSoloMessage(c(Special = paste0("Scenario A, Run ", i, " / ", ScenarioA.Runs, ": Started processing with ", nrow(ServerSpecifications), " servers and sample size ", CurrentSampleSize, ".")))

              SingleRunResults <- SingleRun(SingleRun.SampleSize = CurrentSampleSize,
                                            SingleRun.DSConnections = ScenarioA.CCPConnections)

              Tracker.ScenarioA <- Tracker.ScenarioA %>%
                                        bind_rows(SingleRunResults$Summary)

              Tracker.ScenarioA.ServerSpecific <- Tracker.ScenarioA.ServerSpecific %>%
                                                      bind_rows(SingleRunResults$ServerSpecific)
          }

          # Column-binding of server-specific tracker results
          Results.A.ServerSpecific.CurrentRun <- Results.A.CurrentRun %>%
                                                    bind_cols(Tracker.ScenarioA.ServerSpecific)

          # Column-binding of tracker results and calculation of durations
          Results.A.CurrentRun <- Results.A.CurrentRun %>%
                                      bind_cols(Tracker.ScenarioA) %>%
                                      mutate(Duration.Login = as.numeric(difftime(Time.AfterLogin, Time.StartRun, units = "secs")),
                                             Duration.Loading = as.numeric(difftime(Time.AfterLoading, Time.AfterLogin, units = "secs")),
                                             Duration.SampleDrawing = as.numeric(difftime(Time.AfterSampleDrawing, Time.AfterLoading, units = "secs")),
                                             Duration.Refinement.WithTI = as.numeric(difftime(Time.AfterRefinement, Time.AfterSampleDrawing, units = "secs")),
                                             Duration.Refinement.WithoutTI = Duration.Refinement.Total.WithoutGapTime,
                                             Duration.Refinement.OnlyTI = Duration.Refinement.WithTI - Duration.Refinement.WithoutTI)

          # Row-bind current results to main result data.frames
          Results.A <- Results.A %>%
                            bind_rows(Results.A.CurrentRun)

          Results.A.ServerSpecific <- Results.A.ServerSpecific %>%
                                          bind_rows(Results.A.ServerSpecific.CurrentRun)

          if (CreateLogFiles == TRUE)
          {
              write.table(Results.A.CurrentRun, file = LogFile.ScenarioA, sep = ",", row.names = FALSE, append = file.exists(LogFile.ScenarioA))
              write.table(Results.A.ServerSpecific.CurrentRun, file = LogFile.ScenarioA.ServerSpecific, sep = ",", row.names = FALSE, append = file.exists(LogFile.ScenarioA.ServerSpecific))
          }

          DSI::datashield.logout(conns = ScenarioA.CCPConnections)

      }
  }


#===============================================================================
# Test scenario B)
#   - Varying number of servers, these are randomly selected
#   - Fixed sample size
#===============================================================================

  Results.B <- tibble()
  Results.B.ServerSpecific <- tibble()

  if (ScenarioB.Runs > 0)
  {
      for (i in 1:ScenarioB.Runs)
      {
          PrintSoloMessage(c(Special = paste0("Scenario B: Started Run ", i, " / ", ScenarioB.Runs)))

          for (k in ScenarioB.SequenceServerCount)
          {
              PrintSoloMessage(c(Special = paste0("Scenario B, Run ", i, " / ", ScenarioB.Runs, ": Started processing with ", k, " from ", nrow(ServerSpecifications), " servers and fixed sample size ", ScenarioB.SampleSize, ".")))

              # Randomly select k servers from all servers
              CurrentServerSpecifications <- ServerSpecifications[sample(1:nrow(ServerSpecifications), k), ]

              Results.B.CurrentRun <- tibble(Run = i,
                                             Time.StartRun = Sys.time())

              # Connect to CCP servers
              ScenarioB.CCPConnections <- dsCCPhosClient::ConnectToCCP(ServerSpecifications = CurrentServerSpecifications)

              Results.B.CurrentRun$NumberOfServers <- k
              Results.B.CurrentRun$Time.AfterLogin <- Sys.time()

              #---  Load Source Data Set  ---------------------------------------------
              CCP.LoadSourceDataSet(ServerSpecifications = CurrentServerSpecifications,
                                 DSConnections = ScenarioB.CCPConnections)

              Results.B.CurrentRun$Time.AfterLoading <- Sys.time()

              #---  Run single processing run with specific sample size ------------
              SingleRunResults <- SingleRun(SingleRun.SampleSize = ScenarioB.SampleSize,
                                            SingleRun.DSConnections = ScenarioB.CCPConnections)

              Results.B.ServerSpecific.CurrentRun <- Results.B.CurrentRun %>%
                                                          bind_cols(SingleRunResults$ServerSpecific)

              Results.B.CurrentRun <- Results.B.CurrentRun %>%
                                          bind_cols(SingleRunResults$Summary) %>%
                                          mutate(Duration.Login = as.numeric(difftime(Time.AfterLogin, Time.StartRun, units = "secs")),
                                                 Duration.Loading = as.numeric(difftime(Time.AfterLoading, Time.AfterLogin, units = "secs")),
                                                 Duration.SampleDrawing = as.numeric(difftime(Time.AfterSampleDrawing, Time.AfterLoading, units = "secs")),
                                                 Duration.Refinement.WithTI = as.numeric(difftime(Time.AfterRefinement, Time.AfterSampleDrawing, units = "secs")),
                                                 Duration.Refinement.WithoutTI = Duration.Refinement.Total.WithoutGapTime,
                                                 Duration.Refinement.OnlyTI = Duration.Refinement.WithTI - Duration.Refinement.WithoutTI)

              # Row-bind current results to main result data.frames
              Results.B <- Results.B %>%
                                bind_rows(Results.B.CurrentRun)

              Results.B.ServerSpecific <- Results.B.ServerSpecific %>%
                                              bind_rows(Results.B.ServerSpecific.CurrentRun)

              if (CreateLogFiles == TRUE)
              {
                  write.table(Results.B.CurrentRun, file = LogFile.ScenarioB, sep = ",", row.names = FALSE, append = file.exists(LogFile.ScenarioB))
                  write.table(Results.B.ServerSpecific.CurrentRun, file = LogFile.ScenarioB.ServerSpecific, sep = ",", row.names = FALSE, append = file.exists(LogFile.ScenarioB.ServerSpecific))
              }

              DSI::datashield.logout(conns = ScenarioB.CCPConnections)
          }
      }
  }

#-------------------------------------------------------------------------------
  return(list(Results.A = Results.A,
              Results.A.ServerSpecific = Results.A.ServerSpecific,
              Results.B = Results.B,
              Results.B.ServerSpecific = Results.B.ServerSpecific))
}
