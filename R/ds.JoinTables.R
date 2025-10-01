
#' ds.JoinTables
#'
#' Join tables on server, making use of \code{dplyr} mutating join operations.
#'
#' Linked to server-side \code{ASSIGN} function \code{JoinTablesDS()}
#'
#' @param TableNameA \code{string} - Name of Table A on server
#' @param TableNameB \code{string} - Name of Table B on server
#' @param ByStatement \code{string} - The insides of a \code{dplyr::join_by()}-Statement defining how to join tables
#' @param JoinType \code{string} - Name of \code{dplyr::join}-function used, one of:
#'                     \itemize{\item 'left_join' (Default)
#'                              \item 'right_join'
#'                              \item 'full_join'
#'                              \item 'inner_join'}
#' @param OutputName \code{string} - Name of resulting table on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.JoinTables <- function(TableNameA,
                          TableNameB,
                          ByStatement,
                          JoinType = "left_join",
                          OutputName,
                          DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)

  # --- For Testing Purposes ---
  # TableNameA <- "ADS_Patient"
  # TableNameB <- "ADS_Diagnosis"
  # ByStatement <- "PatientID"
  # JoinType <- "left_join"
  # OutputName <- "PatientAnalysis"
  # DSConnections <- CCPConnections

  # --- Argument Assertions ---
  assert_that(is.string(TableNameA),
              is.string(TableNameB),
              is.string(ByStatement),
              is.string(JoinType),
              is.string(OutputName))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side ASSIGN function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("JoinTablesDS",
                                      TableNameA.S = TableNameA,
                                      TableNameB.S = TableNameB,
                                      ByStatement.S = ByStatement,
                                      JoinType.S = JoinType))

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(OutputName,
                                       DSConnections = DSConnections)

#-------------------------------------------------------------------------------
  return(AssignmentInfo)
}
