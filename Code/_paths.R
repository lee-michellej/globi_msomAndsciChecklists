

# 3 - format data checklist


# Set working directory
setwd("~/globi_tritrophic_networks/")

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This function sets the working directory to wherever this file is located
setwd("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/")
# Check the working directory
getwd()


# 
OneDrive_path <- "/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi"
github_path <- "/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/"


# Read in data
# This data was downloaded from globi 
# resolvedplantnamesglobi_12feb24.csv = This is the whole Globi csv file- with the new resolvedPLant names column
# dat <- read.csv("./Data/resolvedplantnamesglobi_12feb24.csv")
# dat <- read.csv("/Volumes/SanDisk/LARGE_globiDataFiles/resolvedplantnamesglobi_12feb24.csv")
dat <- read.csv(paste0(OneDrive_path, "/Data/resolvedplantnamesglobi_12feb24.csv"))



# Read in bee phenology data with the bee checklist names
bee.list <- read.csv("./Data/SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv")


# In this code, we will go through all of the scientific bee names and identify synonyms & update them all to the latest nomenclature
# To do this - we need to load several files, which were compiled from different sources with scientific bee name synonyms 
#----- We need 6 separate files that store bee names 

# Bee name list from Discover Life (published on Zenodo:https://zenodo.org/records/10463762)
#bee.names.jan24 <- read_tsv("/Volumes/SanDisk/LARGE_globiDataFiles/discoverlife-January-05-2024.tsv") %>% 
bee.names.jan24 <- read_tsv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/discoverlife-January-05-2024.tsv") %>% 
  dplyr::select(1:11)
# Add column names
colnames(bee.names.jan24) <- c("providedExternalId",
                               "providedName",
                               "providedAuthorship",
                               "providedRank",
                               "providedPath",
                               "relationName",
                               "resolvedExternalID",
                               "resolvedExternalId",
                               "resolvedAuthorship",
                               "resolvedRank",
                               "resolvedPath"
)
# Quick note about column names in this file:
# providedName (old scientific name) --> relationName (latest scientific name)


# Bee name list from Discover Life that was used in previous script
bee.names.orig <- read.csv("./Data/discoverlife-Anthophila-2022 04 01.csv")
# Quick note about column names in this file:
# providedName (old scientific name) --> resolvedName (latest scientific name)


# Bee name list that Katja generated for Globi Names not included in Discover Life
bee.names.globi <- read.csv("./Data/Globi-names-not-in-discoverlife - Sheet1 2022 04 01.csv")
# these are names that were manually edited by Katja
# sourceTaxonSpeciesName (old scientific name) --> resolvedBeeNames (latest scientific name)

# Bee name list from the Big Bee Network that Katja and others.
# This file deals with a lot of the typos we were having issues with.
# bee.names.bigbeenetwork <- read.csv("/Volumes/SanDisk/LARGE_globiDataFiles/names-aligned.csv")
bee.names.bigbeenetwork <- read.csv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/names-aligned.csv")
# Quick note about column names in this file:
# providedName (old scientific name) --> alignedSpeciesName (latest scientific name)

# Bee name list from Michelle that was a manual edit to match the last few names that were unmatched by above lists.
# bee.names.editmar24 <- read.csv("./Data/globibees_namesmissing_26mar24.csv")
bee.names.editmar24 <- read.csv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/globibees_namesmissing_26mar24.csv")

# Quick note about column names in this file:
# name (old scientific name) --> resolvedName (latest scientific name)

#------ End files for bee matching (synonyms)




# Institution codes
# This file matches the institution codes in the globi database to a name
institution.codes <- read.csv("./Data/institutioncodes_2021_12_16.csv")


# Read in plant phenology data
# Resolving the plant names (i.e., updating the synonyms to the latest scientific name) was done in a previous file
plant.phenology <- read.csv("./Data/resolvedplantsci_12feb24.csv")


# This is the bee phenology data with the following modification:
# Remove Apis mellifera
bee.list <- bee.list[bee.list$specificEpithet != "mellifera",]







#-------- Review model convergence


# Set library paths:
library_paths <- c( .libPaths(),
                    "/home/gdirenzo/R/x86_64-redhat-linux-gnu-library/4.2",
                    "/home/software/hovenweep/arc/apps/R/library/4.2/GNU/12.1",
                    "/opt/cray/pe/R/4.2.1.2/lib64/R/library"
)



# Set working directory
setwd(paste0("/Volumes/DIRENZO/globi20250717-MCMC-ModOutput/MCMC/"))




#--------- model code file



#------------  Upload the data & format
# object name = bee.plant.cite
# 3-D array
load("/home/gdirenzo/globi/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")


# Load covariates
load("/home/gdirenzo/globi/Data/model_covariates - 2025 01 22 - no apis.rds")




# set working directory for Hovenweep (HPC):
setwd("/home/gdirenzo/globi/")



# Specify the library location
.libPaths( c(
  "/home/gdirenzo/R/x86_64-redhat-linux-gnu-library/4.2",
  "/home/software/hovenweep/arc/apps/R/library/4.2/GNU/12.1",
  "/opt/cray/pe/R/4.2.1.2/lib64/R/library"
))




# ----- 7


# Add globi folder name
globi_out_folder <- "/Volumes/DIRENZO/globi20250717-out-ModOutput/out"
globi_result_folder <- "/Volumes/DIRENZO/globi20250717-quarter_results-ModOutput/quarter-results"
globi_MCMC_folder <- "/Volumes/DIRENZO/globi20250717-MCMC-ModOutput/MCMC"

# Add github folder path
github_folder_path <- "/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/"






#------ Model comparison document


# Add globi folder name
globi_out_folder <- "/Volumes/DIRENZO/globi20250717-out-ModOutput/out"
globi_result_folder <- "/Volumes/DIRENZO/globi20250717-quarter_results-ModOutput/quarter-results"


# Add github folder path
github_folder_path <- "/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/"





#------------  Upload the data & format
# object name = bee.plant.cite
# 3-D array
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")



# Read in the dat_info
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/dat_info_2025_01_22.rds")
# object name = dat_info



# Load covariates
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/model_covariates - 2025 01 22 - no apis.rds")

