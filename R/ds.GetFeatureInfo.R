
#' ds.GetFeatureInfo
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Obtain data about feature type and sample size.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetFeatureInfoDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{tibble} containing separate and cumulated feature properties about feature type and sample size.
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetFeatureInfo <- function(TableName,
                              FeatureName,
                              DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dsBaseClient)
  require(dplyr)
  require(purrr)

  # --- For Testing Purposes ---
  # TableName <- "CDS_Staging"
  # FeatureName <- "TNM_T"
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  # Stop execution if referred table object is not a data.frame
  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


  # ServerReturns: Obtain feature properties for each server calling dsCCPhos::GetFeatureInfoDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetFeatureInfoDS",
                                                         TableName.S = TableName,
                                                         FeatureName.S = FeatureName))

  # Convert Server returns into tibble containing separate feature meta data
  SeparateProperties <- ServerReturns %>%
                            list_rbind(names_to = "Server")

  # Obtaining return value for cumulated feature data type
  ReturnedFeatureDataTypes <- unique(SeparateProperties$DataType[!is.na(SeparateProperties$DataType)])
  CumulatedDataType <- NA
  if (length(ReturnedFeatureDataTypes) == 1) { CumulatedDataType <- ReturnedFeatureDataTypes }
  if (length(ReturnedFeatureDataTypes) > 1) { CumulatedDataType <- "Inconclusive"}

  # Obtain cumulated feature meta data
  CumulatedProperties <- tibble(Server = "All",
                                DataType = CumulatedDataType,
                                N.Total = sum(SeparateProperties$N.Total),
                                N.Valid = sum(SeparateProperties$N.Valid),
                                ValidProportion = N.Valid / N.Total,
                                N.Missing = sum(SeparateProperties$N.Missing),
                                MissingProportion = N.Missing / N.Total)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Glue cumulated and separate feature meta data together and return tibble
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(bind_rows(CumulatedProperties,
                   SeparateProperties))
}
