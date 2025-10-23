
#' GetServerOpalDBInfo
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Check if tables are available in server Opal data bases.
#'
#' @param ServerSpecifications \code{data.frame} - Same data frame used for login. Used here only for akquisition of server-specific project names (in case they are differing). - Default: NULL for virtual project
#' @param OpalTableNames.Required \code{character} - The table names expected/required in server Opal data base
#' @param OpalTableNames.Dictionary Optional \code{list} of named \code{character vectors} - To enable server-specific mapping of deviating to required Opal data base table names. Names of list elements must match server names. For rules that should be applied on all servers, choose form \code{list(All = c('LookupName' = 'RequiredName'))}.
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list}:
#'            \itemize{ \item OpalTables.Available (\code{tibble})
#'                      \item OpalTables.Required (\code{tibble})
#'                      \item Summary (\code{tibble}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetServerOpalDBInfo <- function(ServerSpecifications = NULL,
                                OpalTableNames.Required = NULL,
                                OpalTableNames.Dictionary = NULL,
                                DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications = NULL
  # OpalTableNames.Required <- dsCCPhosClient::Meta.Tables$TableName.Raw
  # OpalTableNames.Dictionary <- NULL
  # DSConnections = CCPConnections

  # --- Argument Validation ---
  if (!is.null(ServerSpecifications)) { assert_that(is.data.frame(ServerSpecifications)) }
  if (!is.null(OpalTableNames.Required)) { assert_that(is.character(OpalTableNames.Required)) }
  if (!is.null(OpalTableNames.Dictionary)) { assert_that(is.list(OpalTableNames.Dictionary)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get server names (sorted alphabetically)
  ServerNames <- sort(names(DSConnections))

  # Create 'ServerSpecifications' tibble for virtual setting
  if (is.null(ServerSpecifications)) { ServerSpecifications <- tibble(ServerName = ServerNames,
                                                                      URL = "Virtual",
                                                                      ProjectName = "Virtual",
                                                                      Token = NA) }

  # Create tibble containing server-specific Opal table name mapping based on optionally passed dictionaries
  if (!is.null(OpalTableNames.Dictionary))
  {
      # Turn passed list into a data.frame enabling subsequent mapping. Lookups stated in server-specific character vectors are privileged (overruling lookups stated in "All" vector)
      OpalTableNames.Dictionary <- OpalTableNames.Dictionary %>%
                                      map(\(Vector) tibble::enframe(Vector, name = "Lookup", value = "OpalTableName.Generic")) %>%
                                      list_rbind(names_to = "Server") %>%
                                      mutate(IsPrimary = ifelse(Server == "All", FALSE, TRUE),
                                             Server = ifelse(Server == "All", list(ServerNames), Server)) %>%
                                      unnest(Server) %>%
                                      arrange(Server, Lookup, desc(IsPrimary)) %>%      # This makes sure that server-specific lookups overrule generic lookups stated in list element "All"
                                      distinct(Server, Lookup, .keep_all = TRUE) %>%
                                      select(-IsPrimary)
  } else {

      OpalTableNames.Dictionary <- tibble(Server = ServerNames,
                                          Lookup = NA,
                                          OpalTableName.Generic = NA)
  }

  # Get overview of available (not necessarily required) Opal table names on servers and their name processing
  OpalTables.Available <- DSI::datashield.tables(conns = DSConnections) %>%
                              tibble::enframe(name = "Server", value = "OpalTableName") %>%
                              unnest(OpalTableName) %>%
                              left_join(select(ServerSpecifications, c(ServerName, ProjectName)),
                                        by = join_by(Server == ServerName)) %>%
                              mutate(OpalTableName.Stripped = str_remove(OpalTableName, pattern = paste0(ProjectName, "."))) %>%
                              left_join(OpalTableNames.Dictionary,
                                        by = join_by(Server, OpalTableName.Stripped == Lookup)) %>%
                              mutate(OpalTableName.Generic = coalesce(OpalTableName.Generic, OpalTableName.Stripped),
                                     IsAvailable = TRUE)

  # If no OpalTableNames.Required are passed, just take all available (generic) Opal table names
  if (is.null(OpalTableNames.Required)) { OpalTableNames.Required <- unique(OpalTables.Available$OpalTableName.Generic) }

  # Create data.frame containing info about required Opal table availability
  OpalTables.Required <- crossing(Server = ServerNames,      # Get all combinations of server names and required Opal table names
                                  OpalTableName.Generic = OpalTableNames.Required) %>%
                             left_join(OpalTables.Available, by = join_by(Server, OpalTableName.Generic)) %>%
                             mutate(IsAvailable = replace_na(IsAvailable, FALSE),
                                    IsRequired = TRUE)


  # Create data.frame summarizing availability of required Opal tables on all servers
  Summary <- OpalTables.Required %>%
                 select(Server,
                        OpalTableName.Generic,
                        IsAvailable) %>%
                 rename(c("TableName" = "OpalTableName.Generic")) %>%
                 pivot_wider(names_from = Server,
                             values_from = IsAvailable) %>%
                 rowwise() %>%
                 mutate(IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                        NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                NA),
                        .after = TableName) %>%
                 ungroup()

#-------------------------------------------------------------------------------
  return(list(OpalTables.Available = OpalTables.Available,
              OpalTables.Required = OpalTables.Required,
              Summary = Summary))
}
