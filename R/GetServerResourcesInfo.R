
#' GetServerResourcesInfo
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Check if tables are available in server Opal data bases.
#'
#' @param ServerSpecifications Optional \code{data.frame} - Same data frame used for login. Used here only for akquisition of server-specific project names (in case they are differing). - Default: NULL for virtual project
#' @param ResourceNames.Required Optional \code{character} - The resource names expected/required to be on servers. If none are passed all available resources are assumed to be required.
#' @param ResourceNames.Dictionary Optional \code{list} of named \code{character vectors} - To enable server-specific mapping of deviating to required resource names. Names of list elements must match server names. For rules that should be applied on all servers, choose form \code{list(All = c('LookupName' = 'RequiredName'))}.
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list}:
#'            \itemize{ \item Resources.Available (\code{tibble})
#'                      \item Resources.Required (\code{tibble})
#'                      \item Summary (\code{tibble}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetServerResourcesInfo <- function(ServerSpecifications = NULL,
                                   ResourceNames.Required = NULL,
                                   ResourceNames.Dictionary = NULL,
                                   DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications <- NULL
  # ServerSpecifications <- read.csv(file = "SiteSpecs_Test.csv")
  # ResourceNames.Required <- dsFredaP21Client::Meta.Tables$TableName.Raw
  # ResourceNames.Dictionary <- NULL
  # ResourceNames.Dictionary <- list(All = c("FAB" = "fab", "ICD" = "ice"),
  #                                  ServerA = c("FAB" = "fabeee"))
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  if (!is.null(ServerSpecifications)) { assert_that(is.data.frame(ServerSpecifications)) }
  if (!is.null(ResourceNames.Required)) { assert_that(is.character(ResourceNames.Required)) }
  if (!is.null(ResourceNames.Dictionary)) { assert_that(is.list(ResourceNames.Dictionary)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- dsFredaClient::CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get server names (sorted alphabetically)
  ServerNames <- sort(names(DSConnections))

  # Create 'ServerSpecifications' tibble for virtual setting
  if (is.null(ServerSpecifications)) { ServerSpecifications <- tibble(ServerName = ServerNames,
                                                                      URL = "Virtual",
                                                                      ProjectName = "Virtual",
                                                                      Token = NA) }

  # Create tibble containing server-specific resource name mapping based on optionally passed dictionaries
  if (!is.null(ResourceNames.Dictionary))
  {
      ResourceNames.Dictionary <- ResourceNames.Dictionary %>%
                                      map(\(Vector) tibble::enframe(Vector, name = "Lookup", value = "ResourceName.Generic")) %>%
                                      list_rbind(names_to = "Server") %>%
                                      mutate(IsPrimary = ifelse(Server == "All", FALSE, TRUE),
                                             Server = ifelse(Server == "All", list(ServerNames), Server)) %>%
                                      unnest(Server) %>%
                                      arrange(Server, Lookup, desc(IsPrimary)) %>%      # This makes sure that server-specific lookups overrule generic lookups stated in list element "All"
                                      distinct(Server, Lookup, .keep_all = TRUE) %>%
                                      select(-IsPrimary)

  } else {

      ResourceNames.Dictionary <- tibble(Server = ServerNames,
                                         Lookup = NA,
                                         ResourceName.Generic = NA)
  }

  # Get overview of available (not necessarily required) resources on servers and their name processing
  Resources.Available <- DSI::datashield.resources(conns = DSConnections) %>%
                              tibble::enframe(name = "Server", value = "ResourceName") %>%
                              unnest(ResourceName) %>%
                              left_join(select(ServerSpecifications, c(ServerName, ProjectName)),
                                        by = join_by(Server == ServerName)) %>%
                              mutate(ResourceName.Stripped = str_remove(ResourceName, pattern = paste0(ProjectName, "."))) %>%
                              left_join(ResourceNames.Dictionary,
                                        by = join_by(Server, ResourceName.Stripped == Lookup)) %>%
                              mutate(ResourceName.Generic = coalesce(ResourceName.Generic, ResourceName.Stripped),
                                     IsAvailable = TRUE,
                                     IsRequired = ifelse(is.null(ResourceNames.Required),
                                                         TRUE,
                                                         ResourceName.Generic %in% ResourceNames.Required))

  # If no ResourceNames.Required are passed, just take all available (generic) resource names
  if (is.null(ResourceNames.Required)) { ResourceNames.Required <- unique(Resource.Available$ResourceName.Generic) }

  # Create data.frame containing info about required resource availability
  Resources.Required <- crossing(Server = ServerNames,      # Get all combinations of server names and required resource names
                                 ResourceName.Generic = ResourceNames.Required) %>%
                            left_join(Resources.Available, by = join_by(Server, ResourceName.Generic)) %>%
                            mutate(IsAvailable = replace_na(IsAvailable, FALSE),
                                   IsRequired = TRUE)

  # Create data.frame summarizing availability of required resources on all servers
  Summary <- Resources.Required %>%
                 select(Server,
                        ResourceName.Generic,
                        IsAvailable) %>%
                 rename(c("ResourceName" = "ResourceName.Generic")) %>%
                 pivot_wider(names_from = Server,
                             values_from = IsAvailable) %>%
                 rowwise() %>%
                 mutate(IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                        NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                NA),
                        .after = ResourceName) %>%
                 ungroup()

#-------------------------------------------------------------------------------
  return(list(Resources.Available = Resources.Available,
              Resources.Required = Resources.Required,
              Summary = Summary))
}
