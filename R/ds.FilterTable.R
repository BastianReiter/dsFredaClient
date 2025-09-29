
#' ds.FilterTable
#'
#' Filter a table on server, making use of \code{dplyr} and \code{stringr}.
#'
#' Linked to server-side \code{ASSIGN} function \code{FilterTableDS()}
#'
#' @param TableName \code{string} - Name of \code{data.frame} on server
#' @param FilterExpression \code{string} - \code{dplyr::filter} expression as string
#' @param GroupBy \code{string} - Optional \code{dplyr::group_by} expression as string
#' @param OutputName \code{string} - Name of resulting \code{data.frame} on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.FilterTable <- function(TableName,
                           FilterExpression,
                           GroupBy = NULL,
                           OutputName,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)

  # --- For Testing Purposes ---
  # TableName <- "CDS_Patient"
  # FilterExpression <- "LastVitalStatus == 'Alive' & str_starts(Gender, 'Ma')"
  # GroupBy <- NULL
  # OutputName <- "Test"
  # DSConnections <- CCPConnections

  # --- Argument assertions ---
  assert_that(is.string(TableName),
              is.string(FilterExpression),
              (is.null(GroupBy) || is.string(GroupBy)),
              is.string(OutputName))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Encode string in 'FilterExpression' to make it passable through DSI
  FilterExpression <- .encode_tidy_eval(FilterExpression, .get_encode_dictionary())

  # Execute server-side assign function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("FilterTableDS",
                                      TableName.S = TableName,
                                      FilterExpression.S = FilterExpression,
                                      GroupBy.S = GroupBy))

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(OutputName,
                                       DSConnections = DSConnections)

  return(AssignmentInfo)
}
