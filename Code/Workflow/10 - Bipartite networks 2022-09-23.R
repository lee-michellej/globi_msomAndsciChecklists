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



# To format the output from the occupancy model for the bipartite network analysis



#######################################
## Input Data:
#######################################


# Model output files (from Script 5):
#   - out-bee_species-with-priors-1-NIMBLE.rds
#   - result-quarter-bee_species-with-priors-1-NIMBLE.rds

# 3D array data (from Script 3):
#   - globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds
#   - dat_info_2025_01_22.rds



#######################################
## Output of code:
#######################################


# Formatted csv file for the bipartite network analysis:
#   - 2025 08 05 - bee-plant-mod-probabilities.csv
# Contains bee-plant interaction probabilities from the occupancy model



#######################################
############ Table of Contents ########
#######################################


# 1. Load libraries & set working directory
# 2. Load data
# 3. Format and export bee-plant interaction probabilities


#######################################
#######################################
#######################################




# 1. Load libraries & set working directory -------------------------------------------------------



# Load libraries
library(tidyverse)
library(reshape2)
library(splitstackshape)







# 2. Load data -------------------------------------------------------


# Add github path
github_path <- ""

# Add globi folder name
globi_out_folder    <- ""
globi_result_folder <- ""
globi_MCMC_folder   <- ""




# Load the model output - out
load(paste0(globi_out_folder, "/out-bee_species-with-priors-1-NIMBLE.rds"))

# object = out
# MCMC object


# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-bee_species-with-priors-1-NIMBLE.rds"))

# Save the out object with a new model specific name
result <- subset_result_quarter

# object = result
# MCMC object



# Upload the data

load(paste0(github_path, "/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds"))

# object name = bee.plant.cite
# 3-D array


# Read in the dat_info
load(paste0(github_path, "/Data/dat_info_2025_01_22.rds"))
# object name = dat_info


# Load in the number of observations
load(paste0(github_path, "/Data/obs_dat-2025-02-11.rds"))
# object name = obs_dat.rds



# Extract bee and plant names
bee.names <- dat_info$bee.species
plant.names <- dat_info$plant.species






# 3. Convert the data to long format -------------------------------------------------------






# Pull out the columns with a z in the name
z.cols <- grep("z", colnames(result[[1]]$samples2))


# Determine how many MCMC iterations to keep
row.subset <- 5000

# Just call the 1st MCMC chain - if you try to row bind all 3 chains, it is a LARGE object
z.MCMC <- data.frame(rbind(result[[1]]$samples2[1:row.subset,z.cols],
                           result[[2]]$samples2[1:row.subset,z.cols],
                           result[[3]]$samples2[1:row.subset,z.cols]))



dim(z.MCMC)

# Collapse it down to 1 observation by taking the column mean
z.prob <- colMeans(z.MCMC, na.rm = TRUE)


z.long <- melt(z.prob)
z.long$names <- rownames(z.long)


z.long2 <- cSplit(z.long, "names", sep=".", type.convert=FALSE)

head(z.long2)

colnames(z.long2) <- c("probability", "z", "bee.species", "empty", "plant.species")

head(z.long2)

z.long3 <- z.long2 %>%
  group_by(bee.species, plant.species) %>%
  dplyr::summarize(max_prob = max(probability))

# View(z.long3)

# Add bee names
z.long3$bee.names <- NA

for(i in 1:length(bee.names)){

  row.numbers <- which(z.long3$bee.species == i)
  
  z.long3$bee.names[row.numbers] <- bee.names[i]
  
}

# Add plant names
z.long3$plant.names <- NA

for(i in 1:length(plant.names)){
  
  row.numbers <- which(z.long3$plant.species == i)
  
  z.long3$plant.names[row.numbers] <- plant.names[i]
  
}



# Save file
write.csv(z.long3, paste0(github_folder_path, "/Data/2025 08 05 - bee-plant-mod-probabilities.csv"))



# End script

