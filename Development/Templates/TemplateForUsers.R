
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#





install.packages("devtools")

# Install CCPhos R packages
#devtools::install_github(repo = "BastianReiter/dsCCPhos")
devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
#devtools::install_github(repo = "BastianReiter/CCPhosApp")

library(dplyr)
library(dsCCPhosClient)
library(DSI)
# library(CCPhosApp)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USING CCPHOS APP (look for manual approach below)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# StartCCPhosApp()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANUAL APPROACH (without app)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Automatically print DataSHIELD errors
options(datashield.errors.print = TRUE)


# Read in CCP site specifications from uploaded file (first upload 'SiteSpecs.csv' using RStudio) ...
Credentials <- read.csv(file = "SiteSpecs.csv")

# ... or enter specifications manually
# Credentials <- data.frame(SiteName = c("Sissy", "Franz", "Mannheim"),
#                           URL = c("https://dktk-datashield-test/opal/", "https://dktk-test/opal/", "https://mannheim/opal/"),
#                           Token = c("xxx", "xxx", "xxx"))

# Filtering for sites that work
Credentials <- Credentials %>%
  filter(SiteName %in% c(# "Sissi",
    # "Franz"      # Not available
    "Berlin",
    "Dresden",
    "Mainz",
    # "Mannheim",   # No connection
    "MunichLMU",
    "MunichTU",
    "Essen",
    # "Freiburg",   # No Opal tables
    # "Ulm",   # No connection
    "Wuerzburg",
    "Hannover"
  ))


# Establish connection to servers using convenience funtion 'ConnectToCCP'
CCPConnections <- ConnectToCCP(CCPSiteSpecifications = Credentials)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ServerRequirements <- CheckServerRequirements(CCPSiteSpecifications = Credentials,
                                              DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Source Data Set (SDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages <- LoadSourceDataSet(CCPSiteSpecifications = Credentials,
                           DataSources = CCPConnections)


# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check SDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SDSTableCheck <- ds.CheckSDSTables(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get random samples from Source Data Set on servers for easier testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.DrawSample(SourceDataSetName = "SourceDataSet",
              SampleSize = "2000",
              SampleName = "SDSSample",
              DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA REFINEMENT: Transform Source Data Set (SDS) into Refined Data Set (RDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.RefineData(DataSources = CCPConnections,
              Settings = list(DiagnosisRedundancy_Check = FALSE,
                              DiagnosisAssociation_Check = FALSE),
              SourceDataSetName = "SourceDataSet")



# Make tables from Refined Data Set directly addressable by unpacking them into R server session
Messages <- ds.UnpackRefinedDataSet(RefinedDataSetName = "RefinedDataSet",
                                    DataSources = CCPConnections)

# Get Refinement reports
RefinementReport <- dsCCPhosClient::ds.GetRefinementReport(DataSources = CCPConnections)

View(RefinementReport$IneligibleEntries)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA ENRICHMENT: Transform Refined Data Set (RDS) into Enriched Data Set (EDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.EnrichData
Messages <- ds.EnrichData(RefinedDataSetName = "RefinedDataSet",
                          OutputName = "EnrichmentOutput",
                          DataSources = CCPConnections)


# Make tables from Enriched Data Set directly addressable by unpacking them into R server session
Messages <- ds.UnpackEnrichedDataSet(EnrichedDataSetName = "EnrichedDataSet",
                                     DataSources = CCPConnections)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)
