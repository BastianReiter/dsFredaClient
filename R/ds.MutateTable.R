
#' ds.MutateTable
#'
#' Make use of \code{dplyr::mutate()} to create new features in a given table (\code{data.frame}) on the servers.
#'
#' Linked to server-side \code{ASSIGN} function \code{MutateTableDS()}
#'
#' @param TableName \code{string} - Name of \code{data.frame} on server
#' @param MutateExpression \code{string} - \code{dplyr::mutate} expression as string
#' @param GroupBy \code{string} - Optional \code{dplyr::group_by} expression as string
#' @param OutputName \code{string} - Name of resulting \code{data.frame} on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.MutateTable <- function(TableName,
                           MutateExpression,
                           GroupBy = NULL,
                           OutputName,
                           DSConnections = NULL,
                           DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName <- "CDS_Patient"
  # MutateExpression <- "LastVitalStatus == 'Alive' & str_starts(Gender, 'Ma')"
  # GroupBy <- NULL
  # OutputName <- "Test"
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(MutateExpression),
              is.string(OutputName),
              is.flag(DS.async))
  if (!is.null(GroupBy)) { assert_that(is.string(GroupBy)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Encode string in 'MutateExpression' to make it passable through DSI
  MutateExpression <- .encode_tidy_eval(MutateExpression, .get_encode_dictionary())

  # Execute server-side assign function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("MutateTableDS",
                                      TableName.S = TableName,
                                      MutateExpression.S = MutateExpression,
                                      GroupBy.S = GroupBy),
                         async = DS.async)

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(OutputName,
                                       DSConnections = DSConnections)

#-------------------------------------------------------------------------------
  return(AssignmentInfo)
}
