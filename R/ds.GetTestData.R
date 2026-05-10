
#' ds.GetTestData
#'
#' Trigger basic preparatory transformations on RawDataSet prior to Curation:
#' \itemize{  \item Feature name harmonization
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Type- or format-conversion of features }
#'
#' Linked to server-side ASSIGN method \code{PrepareRawDataDS()}
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server
#' @param TableName \code{string} - Name of specific table object on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{flag} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return A \code{list}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetTestData <- function(DataSetName = NA_character_,
                           TableName = NA_character_,
                           SampleSize,
                           DSConnections = NULL,
                           DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSetName <- "CCP.RawDataSet"
  # TableName <- NA_character_
  # SampleSize <- 30
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(DataSetName),
              is.string(TableName),
              is.count(SampleSize),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get test data
  TestData <- DSI::datashield.aggregate(conns = DSConnections,
                                        expr = call("GetTestDataDS",
                                                    DataSetName.S = DataSetName,
                                                    TableName.S = TableName,
                                                    SampleSize.S = SampleSize),
                                        async = DS.async)


#--- Return test data ---------------------------------------

  return(TestData)
}
