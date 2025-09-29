
# install.packages("devtools")

# Install CCPhos R packages
# devtools::install_github(repo = "BastianReiter/dsCCPhos")
# devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
# devtools::install_github(repo = "BastianReiter/CCPhosApp")

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optional during test phase: Loop through servers and get Curation Reports
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PerformanceTable <- tibble(Site = character(),
                           DurationConnection = double(),
                           DurationLoading = double(),
                           DurationCuration = double())

for (i in 1:nrow(Credentials))
{
    CurrentCredentials <- Credentials[i,]

    TimeInitial <- Sys.time()

    try(CCPConnections <- ConnectToCCP(CCPSiteSpecifications = CurrentCredentials))

    TimePastConnect <- Sys.time()

    try(Messages <- LoadRawDataSet(CCPSiteSpecifications = Credentials,
                                   DataSources = CCPConnections))

    TimePastLoading <- Sys.time()

    try(ds.CurateData(DataSources = CCPConnections))

    TimePastCuration <- Sys.time()

    try(CurationReport <- dsCCPhosClient::ds.GetCurationReport(DataSources = CCPConnections))
    try(saveRDS(CurationReport, file = paste0("CurationReport_", Credentials[i,]$SiteName, lubridate::today(), ".rds")))

    PerformanceTable <- PerformanceTable %>%
                            add_row(Site = Credentials[i,]$SiteName,
                                    DurationConnection = as.double(lubridate::as.duration(TimePastConnect - TimeInitial)),
                                    DurationLoading = as.double(lubridate::as.duration(TimePastLoading - TimePastConnect)),
                                    DurationCuration = as.double(lubridate::as.duration(TimePastCuration - TimePastLoading)))

    DSI::datashield.logout(CCPConnections)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Establish connection to servers using convenience funtion 'ConnectToCCP'
CCPConnections <- ConnectToCCP(CCPSiteSpecifications = Credentials)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ServerRequirements <- CheckServerRequirements(CCPSiteSpecifications = Credentials,
                                              DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages <- LoadRawDataSet(CCPSiteSpecifications = Credentials,
                           DataSources = CCPConnections)


# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- ds.CheckRDSTables(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get random samples from Raw Data Set on servers for easier testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.DrawSample(RawDataSetName = "RawDataSet",
              SampleSize = "2000",
              SampleName = "RDSSample",
              DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA CURATION: Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.CurateData(DataSources = CCPConnections,
              Settings = list(DiagnosisRedundancy_Check = FALSE,
                              DiagnosisAssociation_Check = FALSE),
              RawDataSetName = "RawDataSet")



# Make tables from Curated Data Set directly addressable by unpacking them into R server session
Messages <- ds.UnpackCuratedDataSet(CuratedDataSetName = "CuratedDataSet",
                                    DataSources = CCPConnections)

# Get curation reports
CurationReport <- dsCCPhosClient::ds.GetCurationReport(DataSources = CCPConnections)

View(CurationReport$IneligibleEntries)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA AUGMENTATION: Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.AugmentData
Messages <- ds.AugmentData(CuratedDataSetName = "CuratedDataSet",
                           OutputName = "AugmentationOutput",
                           DataSources = CCPConnections)


# Make tables from Augmented Data Set directly addressable by unpacking them into R server session
Messages <- ds.UnpackAugmentedDataSet(AugmentedDataSetName = "AugmentedDataSet",
                                      DataSources = CCPConnections)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)
