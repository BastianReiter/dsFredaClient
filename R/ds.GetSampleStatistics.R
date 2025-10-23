
#' ds.GetSampleStatistics
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Obtain common parametric and nonparametric statistics about a metric feature.
#' Making use of \code{dsBaseClient::ds.meanSdGp()} and \code{dsBaseClient::ds.quantileMean()}.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetSampleStatisticsDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param MetricFeatureName \code{string} - Name of feature
#' @param GroupingFeatureName \code{string} - Name of optional grouping feature from the same table
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetSampleStatistics <- function(TableName,
                                   MetricFeatureName,
                                   GroupingFeatureName = NULL,
                                   RemoveMissings = TRUE,
                                   DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName <- "CCP.RDS.Diagnosis"
  # MetricFeatureName <- "ICDOTopographyVersion"
  # GroupingFeatureName <- NULL
  # RemoveMissings <- TRUE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(MetricFeatureName),
              is.flag(RemoveMissings))
  if (!is.null(GroupingFeatureName)) { assert_that(is.string(GroupingFeatureName)) }

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
  FeatureType <- TableMetaData$FirstEligible$DataTypes[MetricFeatureName]

  # Stop function if referred feature is not of class 'numeric' or similar
  if (!(FeatureType %in% c("double", "integer", "numeric"))) { stop(paste0("Error: The referred feature '", MetricFeatureName, "' is of class '", FeatureType, "' and therefore not suitable."), call. = FALSE) }


#-------------------------------------------------------------------------------
# Server returns
#-------------------------------------------------------------------------------

  # ServerReturns: Obtain sample statistics for each server calling dsFreda::GetSampleStatisticsDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetSampleStatisticsDS",
                                                         TableName.S = TableName,
                                                         MetricFeatureName.S = MetricFeatureName,
                                                         GroupingFeatureName.S = GroupingFeatureName,
                                                         RemoveMissings.S = RemoveMissings))

  # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


  # Convert Server returns into tibble containing separate statistics
  SeparateStatistics <- ServerReturns %>%
                            list_rbind(names_to = "Server")

  # If 'ServerReturns' are empty return NULL
  if (length(SeparateStatistics) == 0 || nrow(SeparateStatistics) == 0) { return(NULL) }


#-------------------------------------------------------------------------------
# Cumulation
#-------------------------------------------------------------------------------

  # Making use of dsBaseClient::ds.meadSdGp() to obtain CUMULATED parametric statistics
  CumulatedStatistics_Parametric <- dsBaseClient::ds.meanSdGp(x = paste0(TableName, "$", MetricFeatureName),      # Fails for irregular feature names like 'xyz-abc'. Using TableName[['FeatureName']] not possible due to DataSHIELD R parser
                                                              y = "1",
                                                              datasources = DSConnections)

  # Making use of dsBaseClient::ds.quantileMean() to obtain CUMULATED non-parametric statistics
  vc_CumulatedStatistics_Nonparametric <- dsBaseClient::ds.quantileMean(x = paste0(TableName, "$", MetricFeatureName),
                                                                        type = "combine",
                                                                        datasources = DSConnections)

  # Compiling cumulated statistics
  CumulatedStatistics <- tibble(Server = "All",
                                N = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$Nvalid_gp_study[1, "COMBINE"], NA),
                                q5 = vc_CumulatedStatistics_Nonparametric["5%"],
                                Q1 = vc_CumulatedStatistics_Nonparametric["25%"],
                                Median = vc_CumulatedStatistics_Nonparametric["50%"],
                                Q3 = vc_CumulatedStatistics_Nonparametric["75%"],
                                q95 = vc_CumulatedStatistics_Nonparametric["95%"],
                                MAD = NA,
                                Mean = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$Mean_gp_study[1, "COMBINE"], NA),
                                SD = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$StDev_gp_study[1, "COMBINE"], NA),
                                SEM = ifelse(is.list(CumulatedStatistics_Parametric), CumulatedStatistics_Parametric$SEM_gp_study[1, "COMBINE"], NA))


#-------------------------------------------------------------------------------
  return(bind_rows(CumulatedStatistics,
                   SeparateStatistics))
}
