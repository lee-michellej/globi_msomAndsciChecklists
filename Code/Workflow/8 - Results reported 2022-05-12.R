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
load("./ModelOutput/globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")
# object = out
# MCMC object


## Load the entire nimble output
# load("./ModelOutput/OUTPUT - globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")
# object = result
# MCMC object



# Upload the data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_11 - short plant list - no apis.rds")
# object name = bee.plant.date.cite
# 4-D array


# Load the possible bee-plant-interactions
load("./Data/bee_plant_inter_2022_04_11 - short plant - no apis.rds")
# object = bee.plant.inter
# 2-D matrix





# 3. Calculate values reported in the results -------------------------------------------------------



##### Calculate: Number of total unique bee-plant interactions in raw data

# Collapse across date & citation
y.bee.plant <- apply(bee.plant.date.cite, c(1, 2), max, na.rm = TRUE)
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


##### Calculate: Number of bee species with >0 plant interactions

length(which(y.bee.plant > 0))




##### Calculate: Number of bee-plant sinteractions from occupancy model



# Determine how many MCMC iterations to keep
row.subset <- 200

# Summarize z
# # Latent state variables
out_z <- as.mcmc(data.frame(rbind(result[[1]]$samples2[row.subset,grep("z", colnames(result[[1]]$samples2))],
                                  result[[2]]$samples2[row.subset,grep("z", colnames(result[[1]]$samples2))],
                                  result[[3]]$samples2[row.subset,grep("z", colnames(result[[1]]$samples2))])))



# Extract the mean values for z = true bee, plant, by month interactions
# Look at the number of dimensions
dim(out_z[,grep("z", colnames(out_z))])

# Number of MCMC iterations
MCMC <- nrow(out_z)

# We will put those values in an array
out2_array <- array(out_z[,grep("z", colnames(out_z))], 
                    dim = c(MCMC, 
                            dim(bee.plant.date.cite)[1:3]))

# look at dimensions
dim(out2_array)


# Row names - MCMC iterations
rownames(out2_array) <- 1:MCMC

# column names - bee species
colnames(out2_array) <- rownames(bee.plant.date.cite)

# sheet names - plant species
dimnames(out2_array)[[3]] <- colnames(bee.plant.date.cite) 

# 4th dimension - 
dimnames(out2_array)[[4]] <- dimnames(bee.plant.date.cite)[[3]]

# Now, we want to sum the number of unique plants per bee per month
# First - we will calculate the mean 

# To do this, first we will determine if the bee EVER interacts with the plant
bee.plant.mean<- apply(out2_array, c(2, 3, 4), mean, na.rm = TRUE)
bee.plant <- apply(bee.plant.mean, c(1, 2), max, na.rm = TRUE)
bee.plant[bee.plant == "-Inf"] <- NA
# And then, we will sum across plant IDs
bee.interactions <- apply(bee.plant, 1, sum, na.rm = TRUE)



mean(bee.interactions)

sd(bee.interactions)/sqrt(length(bee.interactions))

min(bee.interactions)

max(bee.interactions)

##### Calculate: Number of bee species with 0 plant interactions

length(which(bee.interactions == 0))


##### Calculate: Number of bee species with >0 plant interactions

length(which(bee.interactions > 0))




# ###### Calculate the total number of possible bee-plant interactions per bee # species
# Calcilate the total number of POSSIBLE bee-plant interactions (regardless of month & citation)
bee.plant.pos <- apply(bee.plant.date.cite, c(1, 2), max, na.rm = TRUE)
bee.plant.pos[bee.plant.pos == "-Inf"] <- NA
bee.plant.pos[bee.plant.pos == 0] <- 1
bee.plant.pos <- apply(bee.plant.pos, 1, sum, na.rm = TRUE)


mean(bee.plant.pos)

sd(bee.plant.pos)/sqrt(length(bee.plant.pos))

min(bee.plant.pos)

max(bee.plant.pos)





# End script

