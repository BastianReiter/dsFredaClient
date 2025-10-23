
#' GetServerWorkspaceInfo
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Check which objects live in server-side R sessions and collect meta data about them.
#'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing overview and details of server-side workspace objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetServerWorkspaceInfo <- function(DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Get server names (sorted alphabetically)
  ServerNames <- sort(names(DSConnections))


# Get the names of all objects living in the server-side R sessions and check whether they occur on every server
#-------------------------------------------------------------------------------

  ServerObjectNames <- DSI::datashield.symbols(conns = DSConnections)

  # Get all uniquely occurring object names across servers (although usually the set of symbol names should be the same on all servers)
  UniqueObjectNames <- sort(unique(unlist(ServerObjectNames)))

  # If the server workspaces are completely empty, stop function and return NULL
  if (length(UniqueObjectNames) == 0) { return(NULL) }

  # Initiate objects
  Overview <- list()
  ObjectDetails <- list()

  for (servername in ServerNames)
  {
      ServerOverview <- tibble(Object = UniqueObjectNames) %>%
                            mutate(Exists = Object %in% ServerObjectNames[[servername]])

      # Collect meta data about existing objects and attach some of it to 'ObjectInfo'
      #-------------------------------------------------------------------------
      ExistingObjects <- ServerOverview %>%
                            filter(Exists == TRUE)

      # Get meta data
      MetaData <- ExistingObjects %>%
                      pull(Object) %>%
                      map(function(objectname)
                          {
                              ObjectMetaData <- ds.GetObjectMetaData(ObjectName = objectname,
                                                                     DSConnections = DSConnections[servername])
                              return(ObjectMetaData[[servername]])
                          }) %>%
                      stats::setNames(ExistingObjects$Object)

      # Add some meta data to 'ServerOverview'
      ServerOverview <- ServerOverview %>%
                            rowwise() %>%
                            mutate(Class = ifelse(!is.null(MetaData[[Object]]$Class), MetaData[[Object]]$Class, NA),
                                   Length = as.character(ifelse(!is.null(MetaData[[Object]]$Length), MetaData[[Object]]$Length, NA)),
                                   RowCount = ifelse(!is.null(MetaData[[Object]]$RowCount), MetaData[[Object]]$RowCount, NA),
                                   .after = Object) %>%
                            ungroup() %>%
                            mutate(ServerName = servername, .before = 1)

      # Extract structural details from object meta data
      ServerObjectDetails <- MetaData %>%
                                map(\(ObjectMetaData) ObjectMetaData$Structure)

      # Add server-specific overview table and object details to overall lists
      Overview[[servername]] <- ServerOverview
      ObjectDetails[[servername]] <- ServerObjectDetails
  }


#-------------------------------------------------------------------------------
# Summarize server-specific overviews in 'Overview.All'
#-------------------------------------------------------------------------------
  Overview.All <- Overview %>%
                      list_rbind() %>%
                      group_by(Object) %>%
                      summarize(ServerName = "All",
                                Exists.Info = case_when(n_distinct(Exists) == 1 ~ "Uniform",
                                                        .default = "Varied"),
                                Exists = case_when(all(Exists == TRUE) ~ TRUE,
                                                   .default = NA),
                                Class.Info = case_when(n_distinct(Class) == 1 ~ "Uniform",
                                                       .default = "Varied"),
                                Class = case_when(n_distinct(Class) == 1 ~ first(Class),
                                                  .default = "Varied"),
                                Length.Info = case_when(n_distinct(Length) == 1 ~ "Uniform",
                                                        .default = paste0("Varied (", min(Length, na.rm = TRUE), " - ", max(Length, na.rm = TRUE), ")")),
                                Length = case_when(n_distinct(Length) == 1 ~ first(Length),
                                                   n_distinct(Length) > 1 ~ paste0(min(Length, na.rm = TRUE), " - ", max(Length, na.rm = TRUE)),
                                                   .default = NA),
                                RowCount = sum(RowCount, na.rm = TRUE)) %>%
                      ungroup() %>%
                      relocate(ServerName, .before = Object)

  # Row-bind cumulative and server-specific overview data.frames
  Overview <- c(list(All = Overview.All),
                Overview)


#-------------------------------------------------------------------------------
# Object details
#-------------------------------------------------------------------------------

  # For easier handling
  ObjectDetails <- ObjectDetails %>%
                      list_transpose(simplify = FALSE)

  # For all objects that are not of class 'data.frame', summarize server-specific object details
  NonTableDetails <- Overview.All %>%
                          filter(!(Class == "data.frame")) %>%
                          pull(Object, name = Object) %>%
                          map(function(objectname)
                              {
                                  # Row-bind all server-specific tables containing object structure details
                                  ObjectDetails.All <- ObjectDetails[[objectname]] %>%     # This is a list with server-specific structural details for the current object
                                                            list_rbind()
                                                            #{ if (!is.data.frame(.) & !is.vector(.)) { list_rbind(.) } }

                                  if (!length(ObjectDetails.All) == 0)
                                  {
                                      # Create a summarizing structure table
                                      ObjectDetails.All <- ObjectDetails.All %>%
                                                                group_by(Element) %>%
                                                                    summarize(ExistsEverywhere = case_when(n() == length(ServerNames) ~ TRUE,
                                                                                                           .default = FALSE),
                                                                              Type = case_when(length(unique(Type)) == 1 ~ unique(Type),
                                                                                               .default = "Varied")) %>%
                                                                ungroup()

                                  } else { ObjectDetails.All <- NULL }

                                  return(c(list(All = ObjectDetails.All),
                                           ObjectDetails[[objectname]]))
                              })

  # For all objects of class 'data.frame' use 'ds.GetTableCheck' to get more meta data
  TableDetails <- Overview.All %>%
                      filter(Class == "data.frame") %>%
                      pull(Object, name = Object) %>%
                      map(function(objectname)
                          {
                              ds.GetTableCheck(TableName = objectname,
                                               DSConnections = DSConnections) %>%
                                  pluck("FeatureCheckOverview")
                          })

  # Re-consolidate in 'ObjectDetails'
  ObjectDetails <- c(NonTableDetails, TableDetails) %>%
                        list_transpose()


#-------------------------------------------------------------------------------
# Get eligible value sets from meta data
#-------------------------------------------------------------------------------
  EligibleValues <- Overview.All %>%
                        filter(Class == "data.frame") %>%
                        select(Object) %>%
                        mutate(TableWithoutPrefix = str_replace(Object, "^(RDS.|CDS.|ADS.)", ""),
                               Stage = case_when(str_starts(Object, "RDS.") ~ "Raw",
                                                 str_starts(Object, "CDS.") ~ "Curated",
                                                 str_starts(Object, "ADS.") ~ "Augmented",
                                                 .default = NA))

  EligibleValues.RDS.CDS <- EligibleValues %>%
                                filter(Stage %in% c("Raw", "Curated")) %>%
                                left_join(dsCCPhosClient::Meta.Values, by = join_by(TableWithoutPrefix == Table), relationship = "many-to-many") %>%
                                mutate(Feature = case_when(Stage == "Raw" ~ FeatureName.Raw,
                                                           Stage == "Curated" ~ FeatureName.Curated,
                                                           .default = NA),
                                       Value = case_when(Stage == "Raw" ~ Value.Raw,
                                                         Stage == "Curated" ~ Value.Curated,
                                                         .default = NA),
                                       Label = case_when(Stage == "Raw" ~ Label.Raw,
                                                         Stage == "Curated" ~ Label.Curated,
                                                         .default = NA)) %>%
                                filter(!is.na(Feature) & !is.na(Value)) %>%
                                select(Object,
                                       Feature,
                                       Value,
                                       Label)

  EligibleValues.ADS <- EligibleValues %>%
                            filter(Stage == "Augmented") %>%
                            left_join(dsCCPhosClient::Meta.ADS, by = join_by(TableWithoutPrefix == TableName), relationship = "many-to-many") %>%
                            rename("Feature" = "FeatureName") %>%
                            filter(!is.na(Value)) %>%
                            select(Object,
                                   Feature,
                                   Value,
                                   Label)

  EligibleValues <- EligibleValues.RDS.CDS %>%
                        bind_rows(EligibleValues.ADS) %>%
                        base::split(., .$Object) %>%
                        map(\(PerObject) base::split(PerObject, PerObject$Feature))


#-------------------------------------------------------------------------------
  return(list(Overview = Overview,
              ObjectDetails = ObjectDetails,
              EligibleValues = EligibleValues))
}
