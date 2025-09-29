
#' LoadRawDataSet
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Load raw data set from Opal data base into R session on servers.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param RawTableNames \code{character vector} - The expected names of the Opal data base tables
#' @param CuratedTableNames \code{character vector} - The corresponding curated table names (used for assignment in R session)
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoadRawDataSet <- function(ServerSpecifications = NULL,
                           RawTableNames = dsCCPhosClient::Meta_Tables$TableName_Raw,
                           CuratedTableNames = dsCCPhosClient::Meta_Tables$TableName_Curated,
                           RunAssignmentChecks = TRUE,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(dsBaseClient)
  require(DSI)
  require(purrr)
  require(tidyr)

  # --- For Testing Purposes ---
  # ServerSpecifications <- NULL
  # RawTableNames <- dsCCPhosClient::Meta_Tables$TableName_Raw
  # CuratedTableNames <- dsCCPhosClient::Meta_Tables$TableName_Curated
  # RunAssignmentChecks <- TRUE
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- c(Topic = "Object assignment on servers") }

  # Get server names
  ServerNames <- names(DSConnections)

  # Compile a vector of table names to look for in server Opal data bases
  # Usually this should be 'RawTableNames' but also check 'CuratedTableNames' because some servers might already have adopted 'curated' table names
  TableNamesToLookFor <- c(RawTableNames,
                           CuratedTableNames)

  # Check Opal table availability
  OpalTableAvailability <- GetServerOpalInfo(ServerSpecifications = ServerSpecifications,
                                             RequiredTableNames = TableNamesToLookFor,
                                             DSConnections = DSConnections)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assignment in R server sessions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Loop through all participating servers
  for (i in 1:length(ServerNames))
  {
      # In case ServerSpecifications are NULL, server Opal table will not be concatenated with server-specific project names
      ServerTableNames <- TableNamesToLookFor
      ServerProjectName <- NULL

      # If ServerSpecifications are assigned, there can be server-specific project names and therefore server-specific Opal table names
      if (!is.null(ServerSpecifications))
      {
          # Get server-specific project name
          ServerProjectName <- ServerSpecifications %>%
                                    filter(ServerName == ServerNames[i]) %>%
                                    select(ProjectName) %>%
                                    pull()

          # If ServerProjectName is "Virtual" (as it is the case when using virtual infrastructure in CCPhosApp) make the variable empty so that server Opal table names are just raw table names
          if (ServerProjectName == "Virtual") { ServerProjectName <- "" }

          # Else add a dot ('.') to ServerProjectName according to Opal table name nomenclature
          else { ServerProjectName <- paste0(ServerProjectName, ".") }
      }

      # Create tibble that takes only available Opal tables and matches their names to R symbol names that are used in following assignment
      TableNameMatching <- OpalTableAvailability %>%
                                rename(IsAvailable = ServerNames[i]) %>%
                                select(TableName,
                                       IsAvailable) %>%
                                filter(IsAvailable == TRUE) %>%
                                distinct() %>%
                                mutate(# Create feature with server-specific table names (server-specific project name concatenated with generic table names)
                                       OpalTableName = paste0(ServerProjectName, TableName),
                                       # Turn 'RawTableNames' into 'CuratedTableNames' where necessary and concatenate with prefix 'RDS_'
                                       RTableName = case_when(TableName %in% RawTableNames ~ paste0("RDS_", setNames(CuratedTableNames, nm = RawTableNames)[TableName]),
                                                              .default = paste0("RDS_", TableName)))

      # Loop through available Opal DB tables and assign their content to objects (data.frames) in R session
      for (j in 1:nrow(TableNameMatching))
      {
          datashield.assign(conns = DSConnections[[i]],
                            symbol = TableNameMatching$RTableName[j],
                            value = TableNameMatching$OpalTableName[j],
                            id.name = "_id")
      }
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if assignment on servers succeeded
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (RunAssignmentChecks == TRUE)
  {
      BundledMessages <- list()

      # Loop through all tables to get info about assignment on servers
      for(i in 1:length(CuratedTableNames))
      {
          # Make sure assignment was successful on all servers
          ObjectStatus_Table <- ds.GetObjectStatus(ObjectName = paste0("RDS_", CuratedTableNames[i]),
                                                   DSConnections = DSConnections)

          # Add info about table assignment to Messages
          BundledMessages <- c(BundledMessages,
                               ObjectStatus_Table["ObjectValidity"])   # Must select list element 'ObjectValidity' this way to keep naming of vector and thus class 'Success', 'Warning' and so forth
      }

      # Turn list into (named) vector and add it to Messages
      Messages$Assignment <- c(Messages$Assignment,
                               purrr::list_c(BundledMessages))
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign list 'RawDataSet' on all servers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Create list of vectors (one for each server) containing names of actually existing data.frames
  ExistingRDSTables <- paste0("RDS_", CuratedTableNames) %>%
                            map(function(tablename) { if (!is.na(tablename)) { unlist(ds.exists(x = tablename, datasources = DSConnections)) } else { return(NULL) } }) %>%
                            set_names(paste0("RDS_", CuratedTableNames)) %>%
                            list_transpose() %>%
                            map(\(TableNames) names(TableNames[TableNames == TRUE]))

  # For every server, consolidate all existing Raw Data Set tables in one list object called "RawDataSet"
  ExistingRDSTables %>%
      purrr::iwalk(function(RDSTableNames, servername)
                   {
                      dsBaseClient::ds.list(x = RDSTableNames,
                                            newobj = "RawDataSet",
                                            datasources = DSConnections[servername])
                   })

  if (RunAssignmentChecks == TRUE)
  {
      # Make sure assignment of RawDataSet was successful on all servers
      ObjectStatus_RawDataSet <- ds.GetObjectStatus(ObjectName = "RawDataSet",
                                                    DSConnections = DSConnections)

      # Add info about RawDataSet assignment to Messages
      Messages$Assignment <- c(Messages$Assignment,
                               ObjectStatus_RawDataSet$ObjectValidity)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Print and return Messages object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  PrintMessages(Messages)

  # Return Messages invisibly
  invisible(Messages)
}
