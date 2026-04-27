
#' ExploreFeature
#'
#' Get characterizing statistics about a feature of arbitrary data type.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return \code{list}
#'            \itemize{ \item FeatureInfo
#'                      \item Statistics }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExploreFeature <- function(TableName,
                           FeatureName,
                           DSConnections = NULL,
                           DS.async = FALSE,
                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(FeatureName),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections,
                                        DS.async = DS.async)

  # Stop execution if referred table object is not a data.frame
  if (TableMetaData$FirstEligible$Class != "data.frame")
  {
      Message <- "The referred table object is not a data.frame."
      cli::cat_bullet(Message, bullet = "cross")
      return(NULL)
  }


#-------------------------------------------------------------------------------
# Get feature meta data (total and effective/valid sample size)
#-------------------------------------------------------------------------------

  FeatureInfo <- ds.GetFeatureInfo(TableName = TableName,
                                   FeatureName = FeatureName,
                                   DSConnections = DSConnections,
                                   DS.async = DS.async)

  # Get data type of feature in question
  FeatureType <- filter(FeatureInfo, Server == "All")$DataType


#-------------------------------------------------------------------------------
# Get statistics depending on feature data type
#-------------------------------------------------------------------------------

  # Initiate Statistics
  Statistics <- NULL

  if (FeatureType %in% c("numeric", "integer", "double"))
  {
      Statistics <- ds.GetSampleStatistics(TableName = TableName,
                                           MetricFeatureName = FeatureName,
                                           DSConnections = DSConnections,
                                           DS.async = DS.async,
                                           ...)
  }

  if (FeatureType %in% c("character", "logical"))
  {
      Statistics <- ds.GetFrequencyTable(TableName = TableName,
                                         FeatureName = FeatureName,
                                         DSConnections = DSConnections,
                                         DS.async = DS.async,
                                         ...)
  }

#-------------------------------------------------------------------------------
  return(list(FeatureInfo = FeatureInfo,
              Statistics = Statistics))
}
