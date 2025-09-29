# Login-Script
# load required libraries
library(dplyr)
library(dsCCPhosClient)
library(dsTidyverseClient)
library(DSI)
library(ggplot2)


# Created from Token Manager
# Headers: "SiteName","URL","ProjectName","Token"
csv_content <- '
"SiteName","URL","ProjectName","Token"
"dktk-test","https://dktk-test/opal/","PROJECT-184eead7768448e381e3","XXXXXXXXXXXXXXXXXXXXXXXX"
'

# Create data frame with authentification information
Credentials <- read.csv(textConnection(csv_content), header = TRUE)

# Connect to CCP bridgeheads
CCPConnections <- dsCCPhosClient::ConnectToCCP(CCPSiteSpecifications = Credentials)




# view tables available in the respective opal databases
DSI::datashield.tables(CCPConnections)

# get info about installed packages on servers
DSI::datashield.pkg_status(conns = CCPConnections)

# load data from opal to R session (convenience function within dsCCPhosClient)
Messages <- dsCCPhosClient::LoadRawDataSet(CCPSiteSpecifications = Credentials,
                                           DataSources = CCPConnections)


# collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- dsCCPhosClient::GetServerWorkspaceInfo(DataSources = CCPConnections)

# overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview)

# apply data curation and unpack curated data set from server-side list element
dsCCPhosClient::ds.CurateData(DataSources = CCPConnections)
dsCCPhosClient::ds.UnpackCuratedDataSet(DataSources = CCPConnections)

# collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- dsCCPhosClient::GetServerWorkspaceInfo(DataSources = CCPConnections)

# overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview)

# get curation reports: under construction
CurationReports <- dsCCPhosClient::ds.GetCurationReport(DataSources = CCPConnections)

# augment data
dsCCPhosClient::ds.AugmentData(DataSources = CCPConnections)
dsCCPhosClient::ds.UnpackAugmentedDataSet(DataSources = CCPConnections)

# collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- dsCCPhosClient::GetServerWorkspaceInfo(DataSources = CCPConnections)

# overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# examples for analytic steps #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# column names of ADS_Patients
ds.names("ADS_Patients", datasources = CCPConnections)

FeatureInfo

# ICD10Code
# List object containing tables of absolute and relative frequencies
Frequencies <- ds.GetFrequencyTable(DataSources = CCPConnections,
                                    TableName = "ADS_Patients",
                                    FeatureName = "ICD10Code",
                                    MaxNumberCategories = 5)


# Extract table of relative frequencies and perform some formatting
RelativeFrequencies <- Frequencies$RelativeFrequencies %>%
  mutate(across(-Site, ~ paste0("(", round(.x * 100, 0), "%)")))

# Compile table of absolute and relative frequencies in one
TableData <- Frequencies$AbsoluteFrequencies %>%
  mutate(across(everything(), as.character)) %>%
  bind_rows(RelativeFrequencies) %>%
  group_by(Site) %>%
  summarize(across(everything(), ~ paste0(.x, collapse = "  ")))

TableData

# do time to event modeling with covariate gender
TTE <- ds.GetTTEModel(
  DataSources = CCPConnections,
  TableName = "ADS_Patients",
  TimeFeature = "TimeFollowUp",
  EventFeature = "IsDocumentedDeceased",
  CovariateA = "Gender",
  ModelType = "survfit"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DSI::datashield.logout(CCPConnections)
