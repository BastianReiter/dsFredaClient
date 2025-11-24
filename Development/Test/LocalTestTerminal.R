
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Virtual DataSHIELD infrastructure for testing purposes -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install newest base DataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github(repo = "datashield/dsBase")
# devtools::install_github(repo = "datashield/dsBaseClient")

# Install own DataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github(repo = "BastianReiter/dsCCPhos")
# devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
# devtools::install_github(repo = "BastianReiter/CCPhosApp")

# Install additional Datashield-packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages("dsTidyverse")
# install.packages("dsTidyverseClient")
#
# devtools::install_github("tombisho/dsSynthetic", dependencies = TRUE)
# devtools::install_github("tombisho/dsSyntheticClient", dependencies = TRUE)
#
# devtools::install_github("neelsoumya/dsSurvival")
# devtools::install_github("neelsoumya/dsSurvivalClient")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dsBaseClient)
library(dsCCPhosClient)
library(dsTidyverseClient)
library(resourcer)

# Print DataSHIELD errors right away
options(datashield.errors.print = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")
TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

# Definition of test resource, exemplary with local csv-file
TestResource <- resourcer::newResource(name = "TestResource",
                                       #url = "file://./Development/Test/DummyData.csv",
                                       url = "file://localhost/C:/Users/Basti/ARBEIT Lokal/dsCCPhosClient/Development/Test/DummyData.csv",
                                       format = "csv")


CCPConnections <- ConnectToVirtualCCP(CCPTestData = TestData,
                                      NumberOfServers = 3,
                                      NumberOfPatientsPerServer = 2000,
                                      AddedDsPackages = "dsTidyverse",
                                      Resources = list(TestResource = TestResource))


QuickProcessingRun()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CheckServerRequirements()


# datashield.pkg_status(conns = CCPConnections)
# datashield.method_status(conns = CCPConnections)
# datashield.methods(conns = CCPConnections, type = "assign")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LoadRawDataSet(ServerSpecifications = NULL)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optionally load additional data from a Resource to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List all resources at servers
DSI::datashield.resources(conns = CCPConnections)

# Status/Accessibility of a specific resource
DSI::datashield.resource_status(conns = CCPConnections,
                                resource = "TestResource")

# We know that there is a resource (class 'resource') on the servers pointing to a local csv-file
# Note: Currently, if unknown, there is no way to find out from client-side what exactly a specific resource points at (CSV, DB, Computational resource...)
# First step is to create a 'ResourceClient' on the servers to handle the resource:

DSI::datashield.assign.resource(conns = CCPConnections,
                                symbol = "TestResourceClient",
                                resource = "TestResource")

# The first step requires a suitable 'ResourceResolver' to be registered on the servers. For csv-files this is already given by loading the 'resourcer' package.

# Then we can actually load the data of the resource into the server R session by calling 'as.resource.data.frame' on it
datashield.assign.expr(conns = CCPConnections,
                       symbol = "TestDataFrame",
                       expr = quote(as.resource.data.frame(TestResourceClient,
                                                           strict = 'TRUE')))
                                                           #col_types = cols(.default = 'c'))))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- ds.GetDataSetCheck(DataSetName = "RawDataSet",
                                    AssumeCCPDataSet = TRUE)

View(RDSTableCheck$TableStatus)

View(RDSTableCheck$TableRowCounts$RDS_Diagnosis)
View(RDSTableCheck$FeatureExistence$RDS_Diagnosis)
View(RDSTableCheck$FeatureTypes$RDS_Diagnosis)
View(RDSTableCheck$NonMissingValueRates$RDS_Diagnosis)

RDSTableCheck$TableStatus



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate RDS data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSValidationReports <- ds.GetRDSValidationReport()

# ValidationSummaries <- RDSValidationReports %>%
#                             map(function(Server)
#                                 {
#                                     map()
#                                 })summary(report))


# ValidationReportTables <- ValidationSummaries %>%
#                               map(\(summary) as.data.frame(summary, check.names = FALSE))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optionally: Draw random sample from Raw Data Set on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.DrawSample(RawDataSetName = "RawDataSet",
              SampleSize = "1000",
              SampleName = "RDSSample")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
ds.CurateData(RawDataSetName = "CCP.RawDataSet",
              Settings = NULL,
              OutputName = "CCP.CurationOutput")

CDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CuratedDataSet",
                                    AssumeCCPDataSet = TRUE)

# Integrated in ds.CuratedData: Make tables from Curated Data Set directly addressable by unpacking them into R server session
# ds.UnpackCuratedDataSet(CuratedDataSetName = "CuratedDataSet")

# Get curation reports
CurationReport <- ds.GetCurationReport()

View(CurationReport$EntryCounts$BioSampling)

# Exemplary look at a curation report table
#View(CurationReport$Transformation$All$Monitors$Staging)
#View(CurationReport$Transformation$All$EligibilityOverviews$Staging)
#View(CurationReport$Transformation$All$ValueSetOverviews$Raw)

# Get validation report of Curated Data Set (CDS)
# ValidationReportCDS <- ds.GetValidationReport_CDS(Name_CurationOutput = "CurationOutput",
#                                                   DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot data on value eligibility for exemplary table in CDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Restructure eligibility overview table to meet requirements of plot function
# - Create separate data frames for each 'Feature' value
# - Columns in final object:
#   - 'Feature': contains names of features
#   - 'data': plot data for feature-specific plot
#-------------------------------------------------------------------------------

PlotData <- CurationReports$All$Transformation$EligibilityOverviews$Staging %>%
                select(-ends_with("_Proportional")) %>%
                pivot_longer(cols = c(Raw, Harmonized, Recoded, Final),
                             names_to = "Stage",
                             values_to = "Count") %>%
                pivot_wider(names_from = "Eligibility",
                            values_from = "Count") %>%
                nest(.by = Feature)      # 'Split' the whole table into smaller data frames for each 'Feature' value


library(plotly)

plot_ly(data = filter(PlotData, Feature == "UICCStage")$data[[1]],
        x = ~Stage,
        y = ~Eligible,
        type = "bar",
        name = "Eligible",
        color = I(dsCCPhosClient::CCPhosColors$Green)) %>%
    add_trace(y = ~Ineligible,
              name = "Ineligible",
              color = I(dsCCPhosClient::CCPhosColors$Red)) %>%
    add_trace(y = ~Missing,
              name = "Missing",
              color = I(dsCCPhosClient::CCPhosColors$MediumGrey)) %>%
    layout(xaxis = list(categoryorder = "array",
                        categoryarray = c("Raw", "Harmonized", "Recoded", "Final")),
           yaxis = list(title = "Count"),
           barmode = "stack")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.AugmentData
ds.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet",
               OutputName = "CCP.AugmentationOutput")

ADSTableCheck <- ds.GetDataSetCheck(DataSetName = "AugmentedDataSet")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get overview of objects in server workspaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Using dsCCPhosClient::GetServerWorkspaceInfo() and dsCCPhosClient::ds.GetObjectMetaData()
#-------------------------------------------------------------------------------

# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo()

# Overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview$All)

# Detailed meta data of a particular object (also part of ServerWorkspaceInfo)
ObjectMetaData <- ds.GetObjectMetaData(ObjectName = "CDS_Patient")

# Explore Object meta data: Structural overview
View(ObjectMetaData$ServerA$Structure)

# Get type of feature 'PatientID'
ObjectMetaData$FirstEligible$DataTypes["PatientID"]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process ADS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# dsTidyverse
# ds.filter(df.name = "ADS_Patient",
#           tidy_expr = list(CountDiagnoses == 1),
#           newobj = "ADS_Patient_OneDiagnosis")
#
#
# ds.filter(df.name = "ADS_Patient",
#           tidy_expr = list(Gender == "Female"),
#           newobj = "ADS_Patient_OneDiagnosis")




Messages <- ds.JoinTables(TableNameA = "CCP.ADS.Patient",
                          TableNameB = "CCP.ADS.Diagnosis",
                          ByStatement = "PatientID",
                          OutputName = "AnalysisDataSet")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CohortDescription <- ds.GetCohortDescription(DataSetName = "AugmentedDataSet",
                                             CCPDataSetType = "ADS")


# Transform data into display-friendly time series tables using auxiliary function 'DisplayTimeSeries()'
PatientCount_TimeSeries <- DisplayTimeSeries(TimeSeriesData = CohortDescription$CohortSize_OverTime,
                                             TimePointFeature = DiagnosisYear,
                                             ValueFeature = PatientCount,
                                             GroupingFeature = Server,
                                             IncludeMissingTimePoints = TRUE)

Plot <- CohortDescription$CohortSize_OverTime %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = DiagnosisYear,
                           YFeature = PatientCount,
                           GroupingFeature = Server)


Plot <- CohortDescription$GenderDistribution %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = Gender,
                           YFeature = N,
                           GroupingFeature = Server)

Plot <- CohortDescription$AgeDistribution %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = AgeGroup,
                           YFeature = N,
                           GroupingFeature = Server)




ds.GetSampleStatistics(TableName = "CCP.ADS.Diagnosis",
                       FeatureName = "PatientAgeAtDiagnosis",
                       GroupingFeatureName = "TNM.M",
                       DSConnections = CCPConnections)


ds.GetFrequencyTable(TableName = "CCP.ADS.Diagnosis",
                     FeatureName = "TNM.T",
                     GroupingFeatureName = "TNM.M",
                     DSConnections = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Test <- ds.GetCrossTab(TableName = "ADS_Patient",
                       FeatureNames = c("Sex", "LastVitalStatus", "CountDiagnoses"),
                       RemoveNA = TRUE)

ds.MutateTable(TableName = "ADS_Diagnosis",
               MutateExpression = "UICCStageClassification = case_when(str_starts(TNM_T, '3') ~ 'III', .default = '<NA>')",
               OutputName = "TestUICC")


ds.names("TestUICC")


Test <- dsBaseClient::ds.corTest(x = "AnalysisDataSet$PatientAgeAtDiagnosis",
                                 y = "AnalysisDataSet$TimeDiagnosisToDeath")




Test <- ds.GetTTEModel(TableName = "AnalysisDataSet",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "survfit",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)

library(ggplot2)
library(ggsurvfit)

Test$ServerC %>%
    ggsurvfit()


Test <- ds.GetFeatureInfo(TableName = "AnalysisDataSet",
                          FeatureName = "TNM_T")

Test <- ds.GetSampleStatistics(TableName = "AnalysisDataSet",
                               MetricFeatureName = "PatientAgeAtDiagnosis")

Test <- ds.GetFrequencyTable(TableName = "ADS_Diagnosis",
                             FeatureName = "TNM_T",
                             MaxNumberCategories = 20)

RelativeFrequencies <- Test$RelativeFrequencies %>%
                            mutate(across(-Server, ~ paste0("(", round(.x * 100, 0), "%)")))

TableData <- Test$AbsoluteFrequencies %>%
                  mutate(across(everything(), as.character)) %>%
                  bind_rows(RelativeFrequencies) %>%
                  group_by(Server) %>%
                      summarize(across(everything(), ~ paste0(.x, collapse = "  ")))


library(ggplot2)

PlotData <- Test$AbsoluteFrequencies %>%
                pivot_longer(cols = -Server,
                             names_to = "Value",
                             values_to = "AbsoluteFrequency") %>%
                filter(Server != "All")

Plot <- ggplot(data = as.data.frame(PlotData),
               mapping = aes(fill = Server,
                             x = Value,
                             y = AbsoluteFrequency)) +
            geom_bar(position = "stack",
                     stat = "identity")

Plot <- MakeColumnPlot(DataFrame = PlotData,
                       XFeature = Value,
                       YFeature = AbsoluteFrequency,
                       GroupingFeature = Server)



Test <- ExploreFeature(TableName = "AnalysisDataSet",
                       FeatureName = "TimeDiagnosisToDeath")




ds.GetObjectMetaData(ObjectName = "AugmentationOutput")


ds.mean(x = "ADS_Patients$PatientAgeAtDiagnosis",
        datasources = CCPConnections)


MetaData_ADS_Patients <- ds.GetObjectMetaData(ObjectName = "ADS_Patients")

View(MetaData_ADS_Patients$ServerA$ContentOverview)


SampleStatistics <- ds.GetSampleStatistics(TableName = "ADS_Patients",
                                           MetricFeatureName = "PatientAgeAtDiagnosis")


TestPlot <- MakeBoxPlot(SampleStatistics = SampleStatistics,
                        AxisTitle_y = "Patient age at diagnosis",
                        FillPalette = c("All" = CCPhosColors$MediumGrey,
                                        "ServerA" = CCPhosColors$Primary,
                                        "ServerB" = CCPhosColors$Secondary,
                                        "ServerC" = CCPhosColors$Tertiary))

TestPlot


# dsSurvivalClient::ds.Surv(time = "ADS_Patients$TimeFollowUp",
#                           event = "ADS_Patients$IsDocumentedDeceased",
#                           objectname = "TestSurv",
#                           datasources = CCPConnections)
#
# dsSurvivalClient::ds.survfit(formula = 'TestSurv',
#                              object = "TestSurvfit")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)






# Generate synthetic data using package dsSynthetic (which in turn makes use of packages synthpop and simstudy)

# library(dsSyntheticClient)
#
# SyntheticData <- ds.syn(data = "Diagnosis")
#
#                         method = "cart",
#                         m = 1,
#                         seed = 123)
#
# SyntheticData <- SyntheticData$ServerTotal$Warning
#
#
# SyntheticData



#OpalDB_A <- dsCCPhos::MakeTestDB(CCPTestData_A)
#res_CCPTestData_A <- resourcer::newResource(name = "CCPTest")
#resourcer::PostgresResourceConnector$new()

# res_CCPTestData_A <- resourcer::newResource(name = "CCPTestData",
#                                             url = "file://./Development/Data/RealData/CCPTestData_A.RData",
#                                             format = "list")

#Test <- resourcer::FileResourceGetter$new()
