
library(dplyr)
library(readxl)
library(usethis)


#===============================================================================
# Meta Data transported from dsCCPhos package
#===============================================================================


# Objects <- c("Meta.Tables",
#              "Meta.Features",
#              "Meta.Values",
#              "Proc.EventFeatures",
#              "Proc.TableNormalization",
#              "Set.FeatureObligations",
#              "Set.FeatureTracking",
#              "Set.DataHarmonization",
#              "Set.TransformativeExpressions",
#              "Set.Dictionary",
#              "Set.FuzzyStringMatching")
#
# for (objectname in Objects)
# {
#     Object <- eval(parse(text = paste0("dsCCPhos::", objectname)))
#
#     assign(x = objectname,
#            value = Object)
#
#     # Save data in .rda-file and make it part of the package
#     do.call(use_data, list(as.name(objectname), overwrite = TRUE))
# }


#===============================================================================
# Define server requirements that are checked before running of CCPhos functions
#===============================================================================

Set.ServerRequirements <- list(#--- Data frame containing names of required packages ---
                                RequiredPackages = data.frame(PackageName = character()) %>%
                                                        add_row(PackageName = "dsBase") %>%
                                                        add_row(PackageName = "dsFreda"),
                                #--- Data frame containing names and types of required functions ---
                                RequiredFunctions = data.frame(FunctionName = character(),
                                                               FunctionType = character()) %>%
                                                        add_row(FunctionName = "GetReportingObjectDS", FunctionType = "aggregate") %>%
                                                        add_row(FunctionName = "AugmentDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "CurateDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "ExtractFromListDS", FunctionType = "assign"))

# Save data in .rda-file and make it part of package
use_data(Set.ServerRequirements, overwrite = TRUE)



#===============================================================================
# Template data frame: Server specifications
#===============================================================================

# Initiate tibble that holds credentials of participating servers
ServerSpecifications <- tibble(ServerName = character(),
                               URL = character(),
                               ProjectName = character(),
                               Token = character())

# Add site "Sissy"
ServerSpecifications <- add_row(ServerSpecifications,
                                ServerName = "Sissi",
                                URL = "https://Sissi/",
                                ProjectName = "Project",
                                Token = "1234567890")

# Save data in .rda-file and make it part of package
use_data(ServerSpecifications, overwrite = TRUE)


#===============================================================================
# Module Register
#===============================================================================

Meta.Modules <- list(CCP = "dsCCPhosClient",
                     P21 = "dsFredaP21Client")

use_data(Meta.Modules, overwrite = TRUE)


#===============================================================================
# Data from dsFreda
#===============================================================================

FredaColors <- dsFreda::FredaColors
FredaAlphaPalettes <- dsFreda::FredaAlphaPalettes

use_data(FredaColors, overwrite = TRUE)
use_data(FredaAlphaPalettes, overwrite = TRUE)
