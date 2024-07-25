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


# Read in the dat_info
load("./Data/dat_info_2024_07_11.rds")
# object name = dat_info


# Load in the number of observations
load("./Data/obs_dat.rds")
# object name = obs_dat.rds



# Extract bee and plant names
bee.names <- dat_info$bee.species
plant.names <- dat_info$plant.species






# 3. Convert the data to long format -------------------------------------------------------






# Pull out the columns with a z in the name
z.cols <- grep("z", colnames(result[[1]]$samples2))


# Determine how many MCMC iterations to keep
row.subset <- 1000

# Just call the 1st MCMC chain - if you try to row bind all 3 chains, it is a LARGE object
z.MCMC <- data.frame(rbind(result[[1]]$samples2[1:row.subset,z.cols],
                           result[[2]]$samples2[1:row.subset,z.cols],
                           result[[3]]$samples2[1:row.subset,z.cols]))



dim(z.MCMC)

# Collapse it down to 1 observation by taking the column mean
z.prob <- colMeans(z.MCMC, na.rm = TRUE)

library(reshape2)

z.long <- melt(z.prob)
z.long$names <- rownames(z.long)


library(splitstackshape)
z.long2 <- cSplit(z.long, "names", sep=".", type.convert=FALSE)

head(z.long2)

colnames(z.long2) <- c("probability", "z", "bee.species", "empty", "plant.species")

head(z.long2)

library(tidyverse)
z.long3 <- z.long2 %>%
  group_by(bee.species, plant.species) %>%
  dplyr::summarize(max_prob = max(probability))

View(z.long3)

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
write.csv(z.long3, "./Data/2024 07 19 - bee-plant-mod-probabilities.csv")



# End script

