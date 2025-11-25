
#' ds.GetSampleStatistics
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Obtain common parametric and nonparametric statistics about a metric feature.
#'
#' Linked to server-side \code{AGGREGATE} functions \code{GetSampleStatisticsDS()} and \code{GetEcdfDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param GroupingFeatureName \code{string} - Name of optional grouping feature from the same table
#' @param RemoveNA \code{logical} - Indicating whether missing values should be removed prior to frequency calculation - Default: \code{FALSE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetSampleStatistics <- function(TableName,
                                   FeatureName,
                                   GroupingFeatureName = NULL,
                                   RemoveNA = TRUE,
                                   DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName <- "CCP.ADS.Diagnosis"
  # FeatureName <- "PatientAgeAtDiagnosis"
  # GroupingFeatureName <- "TNM.M"
  # RemoveNA <- TRUE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(FeatureName),
              is.flag(RemoveNA))
  if (!is.null(GroupingFeatureName)) { assert_that(is.string(GroupingFeatureName))
                                       assert_that(GroupingFeatureName != FeatureName,
                                                   msg = "Values for 'GroupingFeatureName' and 'FeatureName' can not be identical.") }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check if addressed objects (Table and Feature) are eligible
#-------------------------------------------------------------------------------

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  # Stop execution if referred table object is not a data.frame
  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

  # Get data type of feature in question
  FeatureType <- TableMetaData$FirstEligible$DataTypes[FeatureName]

  # Stop function if referred feature is not of class 'numeric' or similar
  if (!(FeatureType %in% c("double", "integer", "numeric"))) { stop(paste0("Error: The referred feature '", FeatureName, "' is of class '", FeatureType, "' and therefore not suitable."), call. = FALSE) }


#-------------------------------------------------------------------------------
# Server returns
#-------------------------------------------------------------------------------

  # ServerReturns: Obtain sample statistics for each server calling dsFreda::GetSampleStatisticsDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetSampleStatisticsDS",
                                                         TableName.S = TableName,
                                                         FeatureName.S = FeatureName,
                                                         GroupingFeatureName.S = GroupingFeatureName,
                                                         RemoveNA.S = RemoveNA,
                                                         ReturnECDF.S = TRUE))

  # Convert Server returns into tibble containing separate statistics
  Statistics.Separate <- ServerReturns %>%
                            list_transpose() %>%
                            pluck("Statistics") %>%
                            list_rbind(names_to = "Server")
                            #arrange(factor(!!sym(GroupingFeatureName), levels = c(""))

  # If 'ServerReturns' are empty return NULL
  if (length(Statistics.Separate) == 0 || nrow(Statistics.Separate) == 0) { return(NULL) }


#-------------------------------------------------------------------------------
# Cumulation
#-------------------------------------------------------------------------------

  # TEMPORARY - Non-parametric statistics calculated with weighted means
  Statistics.Cumulated <- Statistics.Separate %>%
                              { if (!is.null(GroupingFeatureName)) { group_by(., !!sym(GroupingFeatureName)) } else {.} } %>%
                              summarize(Server = "All",
                                        q5_Cum = sum(q5 * N) / sum(N),
                                        Q1_Cum = sum(Q1 * N) / sum(N),
                                        Median_Cum = sum(Median * N) / sum(N),
                                        Q3_Cum = sum(Q3 * N) / sum(N),
                                        q95_Cum = sum(q95 * N) / sum(N),
                                        MAD_Cum = sum(MAD * N) / sum(N),
                                        N_Cum = sum(N),
                                        Mean_Cum = sum(Mean * N) / sum(N),
                                        SD_Cum = sqrt((sum((N - 1) * SD^2 + N_Cum * (Mean - Mean_Cum)^2)) / (N_Cum - 1)),
                                        SEM_Cum = SD_Cum / sqrt(N_Cum)) %>%
                              rename_with(~ sub("_Cum$", "", .x),
                                          ends_with("_Cum"))




  # ECDF.Separate <- ServerReturns %>%
  #                       list_transpose() %>%
  #                       pluck("ECDF") %>%
  #                       list_rbind(names_to = "Server")
  #
  # for (groupvalue in unique(ECDF.Separate[[GroupingFeatureName]]))
  # {
  #     ECDFData <- ECDF.Separate %>%
  #                     filter(!!sym(GroupingFeatureName) == groupvalue) %>%
  #                     mutate(N.Cum = sum(N),
  #                            W = N / sum(N))
  #
  #     ECDF.Cum <- function(x)
  #     {
  #
  #
  #         sapply(x, function(xx)
  #                   {
  #                       sum(ECDFData$W * vapply(ECDFData$ECDF, function(Fi) Fi(xx), numeric(1)))
  #                   })
  #     }
  #
  #     ECDF.Cum(80)
  # }


  # group_by(., !!sym(GroupingFeatureName)) %>%
  #                           summarize(ECDF = list(function(x)
  #                                                 {
  #                                                     sapply(x, function(xx)
  #                                                               {
  #                                                                   sum((N / sum(N)) * vapply(ECDF, function(Fi) Fi(xx), numeric(1)))
  #                                                               })
  #                                                 }))


  # Using dsBaseClient::ds.ranksSecure not possible if servers run in 'non-permissive' mode
  # Test <- dsBaseClient::ds.ranksSecure(input.var.name = "CCP.ADS.Diagnosis$PatientAgeAtDiagnosis",
  #                                      datasources = DSConnections)


  # Making use of dsBaseClient::ds.meadSdGp() to obtain CUMULATED parametric statistics
  # CumulatedStatistics_Parametric <- dsBaseClient::ds.meanSdGp(x = paste0(TableName, "$", FeatureName),      # Fails for irregular feature names like 'xyz-abc'. Using TableName[['FeatureName']] not possible due to DataSHIELD R parser
  #                                                             y = "1",
  #                                                             datasources = DSConnections)

  # Making use of dsBaseClient::ds.quantileMean() to obtain CUMULATED non-parametric statistics
  # vc_CumulatedStatistics_Nonparametric <- dsBaseClient::ds.quantileMean(x = paste0(TableName, "$", FeatureName),
  #                                                                       type = "combine",
  #                                                                       datasources = DSConnections)

  # Compiling cumulated statistics
  # CumulatedStatistics <- tibble(Server = "All",
  #                               N = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$Nvalid_gp_study[1, "COMBINE"], NA),
  #                               q5 = vc_CumulatedStatistics_Nonparametric["5%"],
  #                               Q1 = vc_CumulatedStatistics_Nonparametric["25%"],
  #                               Median = vc_CumulatedStatistics_Nonparametric["50%"],
  #                               Q3 = vc_CumulatedStatistics_Nonparametric["75%"],
  #                               q95 = vc_CumulatedStatistics_Nonparametric["95%"],
  #                               MAD = NA,
  #                               Mean = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$Mean_gp_study[1, "COMBINE"], NA),
  #                               SD = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$StDev_gp_study[1, "COMBINE"], NA),
  #                               SEM = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$SEM_gp_study[1, "COMBINE"], NA))


#-------------------------------------------------------------------------------
  return(bind_rows(Statistics.Cumulated,
                   Statistics.Separate))
}
