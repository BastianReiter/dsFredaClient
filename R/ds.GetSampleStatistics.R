
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
  # TableName <- "ADS_Patients"
  # MetricFeatureName <- "TNM_T"
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
  ls_ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetSampleStatisticsDS",
                                                            TableName.S = TableName,
                                                            MetricFeatureName.S = MetricFeatureName,
                                                            GroupingFeatureName.S = GroupingFeatureName,
                                                            RemoveMissings.S = RemoveMissings))

  # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


  # Convert Server returns into tibble containing separate statistics
  df_SeparateStatistics <- ls_ServerReturns %>%
                                list_rbind(names_to = "Server")


#-------------------------------------------------------------------------------
# Cumulation
#-------------------------------------------------------------------------------

  # Making use of dsBaseClient::ds.meadSdGp() to obtain CUMULATED parametric statistics
  ls_CumulatedStatistics_Parametric <- dsBaseClient::ds.meanSdGp(x = paste0(TableName, "$", MetricFeatureName),
                                                                 y = "1",
                                                                 datasources = DSConnections)

  # Making use of dsBaseClient::ds.quantileMean() to obtain CUMULATED non-parametric statistics
  vc_CumulatedStatistics_Nonparametric <- dsBaseClient::ds.quantileMean(x = paste0(TableName, "$", MetricFeatureName),
                                                                        type = "combine",
                                                                        datasources = DSConnections)

  # Compiling cumulated statistics
  df_CumulatedStatistics <- tibble(Server = "All",
                                   N = ls_CumulatedStatistics_Parametric$Nvalid_gp_study[1, "COMBINE"],
                                   q5 = vc_CumulatedStatistics_Nonparametric["5%"],
                                   Q1 = vc_CumulatedStatistics_Nonparametric["25%"],
                                   Median = vc_CumulatedStatistics_Nonparametric["50%"],
                                   Q3 = vc_CumulatedStatistics_Nonparametric["75%"],
                                   q95 = vc_CumulatedStatistics_Nonparametric["95%"],
                                   MAD = NA,
                                   Mean = ls_CumulatedStatistics_Parametric$Mean_gp_study[1, "COMBINE"],
                                   SD = ls_CumulatedStatistics_Parametric$StDev_gp_study[1, "COMBINE"],
                                   SEM = ls_CumulatedStatistics_Parametric$SEM_gp_study[1, "COMBINE"])


#-------------------------------------------------------------------------------
  return(bind_rows(df_CumulatedStatistics,
                   df_SeparateStatistics))
}
