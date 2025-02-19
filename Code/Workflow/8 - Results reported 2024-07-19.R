#######################################
#######################################
## Author: Dr. Graziella DiRenzo
##
## Date Created: 2022-05-12
##
## Copyright (c) Graziella DiRenzo, 2022
## Email: gdirenzo@umass.edu
#######################################
#######################################


#######################################
## Code objectives:
#######################################



# To generate the values reported in the results section of the Globi manuscript



#######################################
## Output of code:
#######################################




#######################################
############ Table of Contents ########
#######################################


# 1. Load libraries & set working directory
# 2. Load data
# 3. Calculate values reported in the results

#######################################
#######################################
#######################################




# 1. Load libraries & set working directory -------------------------------------------------------



# Load libraries
library(tidyverse)
library(mcmcr)



# Add globi folder name
globi_folder <- "globi-20250210"

# Set working directory
setwd(paste0("/Volumes/DIRENZO/", globi_folder, "/gdirenzo/globi/"))
#setwd("/Users/gdirenzo/OneDrive - University of Massachusetts/Dropbox_transfer/Globi/")


# Add github folder path
github_folder_path <- "/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/"



# date object for folder
date_folder <- "2025 01 26"
date <- "2025 01 26"







# 2. Load data -------------------------------------------------------






#---- bee_species model

# Model name
mod_name <- "bee_species"

# Load the model output - out
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_species <- out

out_bee_species_df <- as.data.frame(out_bee_species)


# Load the model output - result
load(file = paste0("./ModelOutput/", date, "/result-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
result_bee_species <- result





#------------  Upload the data & format
# object name = bee.plant.cite
# 3-D array
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")



# Read in the dat_info
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/dat_info_2025_01_22.rds")
# object name = dat_info



# Load covariates
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/model_covariates - 2025 01 22 - no apis.rds")



# Load in the number of observations
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/obs_dat-2025-02-11.rds")
# object name = obs_dat







# 3. Calculate values reported in the results -------------------------------------------------------



##### Calculate: Number of total unique bee-plant interactions in raw data

# Collapse across date & citation
y.bee.plant <- apply(bee.plant.cite, c(1, 2), max, na.rm = TRUE)
y.bee.plant[y.bee.plant == "-Inf"] <- 0

# Total
sum(y.bee.plant)


##### Calculate: Max number of interactions per bee species
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)

max(y.bee.plant)

y.bee.plant[which(y.bee.plant == max(y.bee.plant))]


##### Calculate: Mean number of interactions per bee species
mean(y.bee.plant)

sd(y.bee.plant)/sqrt(length(y.bee.plant))

min(y.bee.plant)

max(y.bee.plant)

##### Calculate: Number of bee species with 0 plant interactions

length(which(y.bee.plant == 0))


##### Calculate: Number of bee species with ==1 plant interactions

length(which(y.bee.plant ==1))

##### Calculate: Number of bee species with ==2 plant interactions

length(which(y.bee.plant ==2))

##### Calculate: Number of bee species with ==3 plant interactions

length(which(y.bee.plant ==3))


##### Calculate: Number of bee-plant interactions from occupancy model



# Summarize z
# # Latent state variables
out_num <- as.mcmc(data.frame(rbind(result[[1]]$samples2[,grep("n_plants_per_bee", colnames(result[[1]]$samples2))],
                                  result[[2]]$samples2[,grep("n_plants_per_bee", colnames(result[[1]]$samples2))],
                                  result[[3]]$samples2[,grep("n_plants_per_bee", colnames(result[[1]]$samples2))])))



# Extract the mean values for z = true bee, plant, by month interactions
# Look at the number of dimensions
dim(out_num)


# Now, we want the number of unique plants per bee

# To do this, we first calculate the mean number of interactions per bee species by taking the column mean
plant_interactions_per_bee <- apply(out_num, 2, mean)


mean(plant_interactions_per_bee)

sd(plant_interactions_per_bee)/sqrt(length(plant_interactions_per_bee))

min(plant_interactions_per_bee)

max(plant_interactions_per_bee)








# End script

