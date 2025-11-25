
#' ds.GetTTEModel
#'
#' Get Time-to-Event model
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetTTEModelDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the features of concern
#' @param TimeFeature \code{string} - Name of time feature
#' @param EventFeature \code{string} - Name of event feature
#' @param ModelType \code{string} - Function name of different TTE models implemented in \code{survival} package:
#'                                  \itemize{\item 'survfit'
#'                                           \item 'survdiff'
#'                                           \item 'coxph'}
#' @param CovariateA \code{string} - Name of optional CovariateA
#' @param CovariateB \code{string} - Name of optional CovariateB
#' @param CovariateC \code{string} - Name of optional CovariateC
#' @param MinFollowUpTime \code{integer} - Optional minimum of observed follow up time
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of Time-to-Event models
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetTTEModel <- function(TableName,
                           TimeFeature,
                           EventFeature,
                           ModelType = "survfit",
                           CovariateA = NULL,
                           CovariateB = NULL,
                           CovariateC = NULL,
                           MinFollowUpTime = 1,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName <- "AnalysisDataSet"
  # TimeFeature <- "TimeFollowUp"
  # EventFeature <- "IsDocumentedDeceased"
  # ModelType <- "survfit"
  # CovariateA <- "UICCStageCategory"
  # CovariateB <- NULL
  # CovariateC <- NULL
  # MinFollowUpTime <- 20
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(TimeFeature),
              is.string(EventFeature),
              is.string(ModelType),
              is.count(MinFollowUpTime))
  if (!is.null(CovariateA)) { assert_that(is.string(CovariateA)) }
  if (!is.null(CovariateB)) { assert_that(is.string(CovariateB)) }
  if (!is.null(CovariateC)) { assert_that(is.string(CovariateC)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check if addressed objects (Table and Feature) are eligible
#-------------------------------------------------------------------------------

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  if (is.null(TableMetaData$FirstEligible$Class)) { stop("Error: The referred table object does not seem to exist on any server.", call. = FALSE)}
  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


#-------------------------------------------------------------------------------
# Server returns
#-------------------------------------------------------------------------------

  # ServerReturns: Obtain survival model for each server calling dsCCPhos::GetSurvModelDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetTTEModelDS",
                                                         TableName.S = TableName,
                                                         TimeFeature.S = TimeFeature,
                                                         EventFeature.S = EventFeature,
                                                         ModelType.S = ModelType,
                                                         CovariateA.S = CovariateA,
                                                         CovariateB.S = CovariateB,
                                                         CovariateC.S = CovariateC,
                                                         MinFollowUpTime.S = MinFollowUpTime))

#-------------------------------------------------------------------------------
# Create separate life tables from server returns
#-------------------------------------------------------------------------------

  LifeTables.Separate <- ServerReturns %>%
                            imap(function(Model, servername)
                                 {
                                      summary(object = Model,
                                              times = seq(from = floor(min(Model$time)),
                                                          to = ceiling(max(Model$time)),
                                                          by = 1),
                                              data.frame = TRUE)
                                 })

#-------------------------------------------------------------------------------
# Create cumulated life table from server-specific life tables
#-------------------------------------------------------------------------------

  LifeTable.Cumulated <- LifeTables.Separate %>%
                              list_rbind(names_to = "Server") %>%
                              group_by(time, strata) %>%
                                  summarize(Server = "All",
                                            n.risk = sum(n.risk),
                                            n.event = sum(n.event),
                                            n.censor = sum(n.censor))
                              group_by(strata) %>%
                                  filter(time >= time[which.max(n.risk)]) %>%
                                  mutate(surv = dsFredaClient::ComputeSurvHaz(n.risk, n.event)$SurvEstimate,
                                         cumhaz = dsFredaClient::ComputeSurvHaz(n.risk, n.event, CumHazMethod = "NelsonAalen")$CumHazEstimate,
                                         std.err = dsFredaClient::ComputeSurvHaz(n.risk, n.event)$SurvStdErr,
                                         std.chaz = dsFredaClient::ComputeSurvHaz(n.risk, n.event, CumHazMethod = "NelsonAalen")$CumHazStdErr,
                                         lower = dsFredaClient::ComputeSurvCI(SurvEstimate = surv, SurvStdErr = std.err, Method = "Cloglog", ConfLevel = 0.95)$Lower,
                                         upper = dsFredaClient::ComputeSurvCI(SurvEstimate = surv, SurvStdErr = std.err, Method = "Cloglog", ConfLevel = 0.95)$Upper) %>%
                                  arrange(time, .by_group = TRUE)

#-------------------------------------------------------------------------------
  return(c(LifeTables.Separate,
           list(All = LifeTable.Cumulated)))
}
