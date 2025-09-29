
#' ExploreFeature
#'
#' Get characterizing statistics about a feature of arbitrary data type.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return \code{list}
#'            \itemize{ \item FeatureInfo
#'                      \item Statistics }
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExploreFeature <- function(TableName,
                           FeatureName,
                           DSConnections = NULL,
                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  # Stop execution if referred table object is not a data.frame
  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get feature meta data (total and effective/valid sample size)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  df_FeatureInfo <- ds.GetFeatureInfo(TableName = TableName,
                                      FeatureName = FeatureName,
                                      DSConnections = DSConnections)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get statistics depending on feature data type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initiate df_Statistics
  df_Statistics <- tibble()

  # Get data type of feature in question
  FeatureType <- filter(df_FeatureInfo, Server == "All")$DataType


  if (FeatureType == "numeric")
  {
      df_Statistics <- ds.GetSampleStatistics(TableName = TableName,
                                              MetricFeatureName = FeatureName,
                                              DSConnections = DSConnections,
                                              ...)
  }

  if (FeatureType %in% c("character", "logical"))
  {
      df_Statistics <- ds.GetFrequencyTable(TableName = TableName,
                                            FeatureName = FeatureName,
                                            DSConnections = DSConnections,
                                            ...)
  }

  if (FeatureType == "Date")
  {
      df_Statistics <- ds.GetSampleStatistics(TableName = TableName,
                                              MetricFeatureName = FeatureName,
                                              DSConnections = DSConnections,
                                              ...)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return statement
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(FeatureInfo = df_FeatureInfo,
              Statistics = df_Statistics))
}
