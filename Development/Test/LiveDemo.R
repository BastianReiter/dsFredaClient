
#===============================================================================
#   - Virtual DataSHIELD infrastructure for testing purposes -
#===============================================================================

# Install newest base DataSHIELD packages
#-------------------------------------------------------------------------------
pak::pak("datashield/dsBase")
pak::pak("datashield/dsBaseClient")

# Install own DataSHIELD packages
#-------------------------------------------------------------------------------
pak::pak("BastianReiter/dsCCPhos")
pak::pak("BastianReiter/dsCCPhosClient")
pak::pak("BastianReiter/FredaGUI")

# Install additional Datashield-packages
#-------------------------------------------------------------------------------
install.packages("dsTidyverse")
install.packages("dsTidyverseClient")


#===============================================================================
# Load required packages
#===============================================================================

library(dsFredaClient)
library(dsTidyverseClient)
library(dsBaseClient)
library(resourcer)

# Print DataSHIELD errors right away
options(datashield.errors.print = TRUE)


#===============================================================================
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#===============================================================================

TestData <- readRDS("../Data/CCP/CCPTestData2026.rds")

CCPConnections <- dsCCPhosClient::ConnectToVirtualCCP(CCPData = TestData,
                                                      AddedDsPackages = "dsTidyverse",
                                                      NumberOfServers = 1,
                                                      NumberOfPatientsPerServer = 500)

#===============================================================================
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#===============================================================================

dsCCPhosClient::CheckServerRequirements()


#===============================================================================
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#===============================================================================

dsCCPhosClient::CCP.LoadRawDataSet(ServerSpecifications = NULL)


#===============================================================================
# Check RDS tables for existence and completeness
#===============================================================================

# RDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
#                                     Module = "CCP",
#                                     Stage = "Raw")

#===============================================================================
# Optionally: Draw random sample from Raw Data Set on servers
#===============================================================================

# dsCCPhosClient::ds.CCP.DrawSample(RawDataSetName = "CCP.RawDataSet",
#                                   SampleSize = 500,
#                                   SampleName = "RDSSample")

#===============================================================================
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#===============================================================================

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
ds.CurateData(RawDataSetName = "CCP.RawDataSet",
              Module = "CCP",
              OutputName = "CCP.CurationOutput")




FredaGUI::Widget.DataSetCheck(RDSCheckData = ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                                                Module = "CCP",
                                                                Stage = "Raw"),
                              CDSCheckData = ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                                                Module = "CCP",
                                                                Stage = "Curated"))

FredaGUI::Widget.CurationReport(Module = "CCP",
                                CurationReport = ds.GetCurationReport())














#===============================================================================
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#===============================================================================

# Run ds.AugmentData
dsCCPhosClient::ds.CCP.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet",
                                   OutputName = "CCP.AugmentationOutput")

ADSTableCheck <- ds.GetDataSetCheck(DataSetName = "AugmentedDataSet")




#===============================================================================
# Get overview of objects in server workspaces
#===============================================================================
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


#===============================================================================
# Process ADS tables
#===============================================================================


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



#===============================================================================
# Data Exploration
#===============================================================================

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


#===============================================================================
# Perform exemplary analyses
#===============================================================================


Test <- ds.GetCrossTab(TableName = "ADS_Patient",
                       FeatureNames = c("Sex", "LastVitalStatus", "CountDiagnoses"),
                       RemoveNA = TRUE)

ds.MutateTable(TableName = "ADS_Diagnosis",
               MutateExpression = "UICCStageClassification = case_when(str_starts(TNM_T, '3') ~ 'III', .default = '<NA>')",
               OutputName = "TestUICC")


ds.names("TestUICC")


Test <- dsBaseClient::ds.corTest(x = "AnalysisDataSet$PatientAgeAtDiagnosis",
                                 y = "AnalysisDataSet$TimeDiagnosisToDeath")



#-------------------------------------------------------------------------------

Test <- ds.GetTTEModel(TableName = "AnalysisDataSet",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "survfit",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)

library(ggplot2)

Plot <- ggplot2::ggplot(Test$All, ggplot2::aes(x = time, y = surv, color = .data[["strata"]])) +
            ggplot2::geom_step() +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Time", y = "Survival probability", color = "strata") +
            ggplot2::theme(legend.title = ggplot2::element_text(size = 10), legend.text = ggplot2::element_text(size = 10))



#-------------------------------------------------------------------------------


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



#===============================================================================
# Log out from all servers
#===============================================================================

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
