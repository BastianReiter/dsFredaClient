
#' GetServerResourcesInfo
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Check if tables are available in server Opal data bases.
#'
#' @param ServerSpecifications \code{data.frame} - Same data frame used for login. Used here only for akquisition of server-specific project names (in case they are differing). - Default: NULL for virtual project
#' @param RequiredResourceNames \code{character} - The resource names expected/required to be on servers
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{tibble}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetServerResourcesInfo <- function(ServerSpecifications = NULL,
                                   RequiredResourceNames,
                                   DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications = NULL
  # DSConnections = CCPConnections

  # --- Argument Assertions ---
  assert_that(is.character(RequiredResourceNames))
  if (!is.null(ServerSpecifications)) { assert_that(is.data.frame(ServerSpecifications)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get server names (sorted alphabetically)
  ServerNames <- sort(names(DSConnections))

  # Get overview of available tables on servers
  TableAvailability <- DSI::datashield.tables(conns = DSConnections)

  # Initiate data frame containing info about table availability
  RequiredTableAvailability <- tibble(TableName = RequiredTableNames)

  for (i in 1:length(ServerNames))
  {
      # When connecting to virtual servers 'ServerSpecifications' can be NULL or project name can be 'Virtual' (e.g. in CCPhosApp)
      if (is.null(ServerSpecifications))
      {
          ServerProjectName <- "Virtual"
      }
      else
      {
          # Get server-specific project name
          ServerProjectName <- ServerSpecifications %>%
                                    filter(ServerName == ServerNames[i]) %>%
                                    select(ProjectName) %>%
                                    pull()
      }

      # In case project is virtual, server Opal table names are just raw table names
      ServerTableNames <- RequiredTableNames

      if (ServerProjectName != "Virtual")
      {
          # Create vector with server-specific table names (raw table names concatenated with server-specific project name)
          ServerTableNames <- paste0(ServerProjectName, ".", RequiredTableNames)
      }

      # For every server, check if raw data tables with server-specific correspondent names are existent in 'TableAvailability'
      RequiredTableAvailability <- RequiredTableAvailability %>%
                                        mutate(!!ServerNames[i] := ServerTableNames %in% TableAvailability[[ServerNames[i]]])
  }

  RequiredTableAvailability <- RequiredTableAvailability %>%
                                    rowwise() %>%
                                    mutate(IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                           NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                   paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                   NA),
                                           .after = TableName) %>%
                                    ungroup()

#-------------------------------------------------------------------------------
  return(RequiredTableAvailability)
}
