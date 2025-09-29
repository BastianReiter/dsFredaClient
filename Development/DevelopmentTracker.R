

#===============================================================================
#
# dsCCPhosClient Package DEVELOPMENT TRACKER
#
#===============================================================================


library(devtools)
library(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set preferred license in description
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_ccby_license()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define part of project that should not be distributed in the package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_build_ignore("Development")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding package dependencies using usethis::use_package()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_package("assertthat")
# use_dev_package("CCPhosApp", type = "Suggests", remote = "devtools::BastianReiter/CCPhosApp")
# use_package("cli")
# use_package("dplyr")
# use_dev_package("dsBaseClient", remote = "devtools::datashield/dsBaseClient")
# use_dev_package("dsCCPhos", remote = "devtools::BastianReiter/dsCCPhos")
# use_package("dsTidyverseClient")
# use_package("DSLite", type = "Suggests")
# use_package("DSI")
# use_package("purrr")
# use_package("ggplot2", type = "Suggests")
# use_package("gt", type = "Suggests")
# use_package("quarto", type = "Suggests")
# use_package("rmarkdown", type = "Suggests")
# use_package("scales", type = "Suggests")
# use_package("sysfonts", type = "Suggests")
# use_package("tibble")
# use_package("tidyr")
# use_package("dsTidyverse")
# use_package("utils", type = "Suggests")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding R script files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# General / Auxiliary functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("Auxiliary.R")
# use_r("CheckServerRequirements.R")
# use_r("ConnectToCCP.R")
# use_r("ConnectToVirtualCCP.R")
# use_r("DisplayTimeSeries.R")
# use_r("ExploreFeature.R")
# use_r("GetEligibleValues.R")
# use_r("GetServerOpalDBInfo.R")
# use_r("GetServerWorkspaceInfo.R")
# use_r("LoadRawDataSet.R")
# use_r("QuickProcessingCheck.R")
# use_r("QuickProcessingRun.R")

# Linked to dataSHIELD AGGREGATE functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("ds.GetAugmentationReport.R")
# use_r("ds.GetCohortDescription.R")
# use_r("ds.GetCrossTab.R")
# use_r("ds.GetCurationReport.R")
# use_r("ds.GetDataSetCheck.R")
# use_r("ds.GetFeatureInfo.R")
# use_r("ds.GetFrequencyTable.R")
# use_r("ds.GetObjectMetaData.R")
# use_r("ds.GetObjectStatus.R")
# use_r("ds.GetReportingObject.R")
# use_r("ds.GetSampleStatistics.R")
# use_r("ds.GetTableCheck.R")
# use_r("ds.GetRDSValidationReport.R")
# use_r("ds.GetCDSValidationReport.R")
# use_r("ds.GetADSValidationReport.R")

# Linked to dataSHIELD ASSIGN functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("ds.AugmentData.R")
# use_r("ds.CurateData.R")
# use_r("ds.ExtractFromList.R")
# use_r("ds.FilterTable.R")
# use_r("ds.JoinTables.R")
# use_r("ds.MutateTable.R")
# use_r("ds.UnpackAugmentationOutput.R")
# use_r("ds.UnpackCurationOutput.R")

# Data visualization
#~~~~~~~~~~~~~~~~~~~
# use_r("ExportPlot")
# use_r("ggTheme_CCP")
# use_r("gtTheme_CCP")
# use_r("MakeBoxPlot")
# use_r("MakeColumnPlot")
# use_r("MakeCurationReport")


