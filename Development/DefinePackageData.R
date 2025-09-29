
library(dplyr)
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FredaAlphaPalettes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FredaAlphaPalettes <- list(Levels_2 <- c(0.5, 0.9),
                           Levels_3 <- c(0.2, 0.5, 0.9),
                           Levels_4 <- c(0.3, 0.5, 0.7, 0.9),
                           Levels_5 <- c(0.4, 0.5, 0.6, 0.7, 0.8))

# Save data in .rda-file and make it part of package
use_data(FredaAlphaPalettes, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Freda Colors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FredaColors <- list(LightGrey = "#EDEDED",
                    MediumGrey = "#D0D0D0",
                    DarkGrey = "#595959",
                    #---------
                    Primary = "#054996",
                    PrimaryLight = "#05499650",
                    Secondary = "#8e1e39",
                    SecondaryLight = "#8e1e3950",
                    Tertiary = "#2B8C88",
                    TertiaryLight = "#2B8C8850",
                    #---------
                    Accent = "#960551",
                    AccentLight = "#96055150",
                    #---------
                    BlueNice = "#7EA6E0",
                    Green = "#269D27",
                    Orange = "#DE8F02",
                    Red = "#A90939")

# Save data in .rda-file and make it part of package
use_data(FredaColors, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data transported from dsCCPhos package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Objects <- c("Meta.Tables",
             "Meta.Features",
             "Meta.Values",
             "Proc.EventFeatures",
             "Proc.TableNormalization",
             "Set.FeatureObligations",
             "Set.FeatureTracking",
             "Set.DataHarmonization",
             "Set.TransformativeExpressions",
             "Set.Dictionary",
             "Set.FuzzyStringMatching")

for (objectname in Objects)
{
    Object <- eval(parse(text = paste0("dsCCPhos::", objectname)))

    assign(x = objectname,
           value = Object)

    # Save data in .rda-file and make it part of the package
    do.call(use_data, list(as.name(objectname), overwrite = TRUE))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define server requirements that are checked before running of CCPhos functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Template data frame: Server specifications
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

