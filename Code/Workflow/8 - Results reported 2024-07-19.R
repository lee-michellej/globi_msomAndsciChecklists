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


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Load data -------------------------------------------------------





# Load the data
load("./ModelOutput/globi-short plant list- 2024 07 17 - all cov - NO apis - NIMBLE.rds")
# object = out
# MCMC object


## Load the entire nimble output
load("./ModelOutput/OUTPUT - globi-short plant list- 2024 07 17 - all cov - NO apis - NIMBLE.rds")
# object = result
# MCMC object



# Upload the data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2024_07_11 - short plant list - no apis.rds")
# object name = bee.plant.cite
# 3-D array



# Load covariates
load("./Data/model_covariates - 2024 07 11 - no apis.rds")
#covariates


# Read in the dat_info
load("./Data/dat_info_2024_07_11.rds")
# object name = dat_info


# Load in the number of observations
load("./Data/obs_dat.rds")
# object name = obs_dat.rds







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

