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
load("./ModelOutput/globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")
# object = out
# MCMC object


## Load the entire nimble output
load("./ModelOutput/OUTPUT - globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")
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



# Upload the data
  # object name = bee.plant.date.cite
  # 4-D array
  # Used for bee and plant names
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_11 - short plant list - no apis.rds")


# Extract bee and plant names
bee.names <- rownames(bee.plant.date.cite)
plant.names <- colnames(bee.plant.date.cite)






# 3. Convert the data to long format -------------------------------------------------------






# Pull out the columns with a z in the name
z.cols <- grep("z", colnames(result[[1]]$samples2))

# Just call the 1st MCMC chain - if you try to row bind all 3 chains, it is a LARGE object
z.MCMC <- result[[1]]$samples2[,z.cols]


# Collapse it down to 1 observation
  # By taking the column mean - we are converting it to a probability
z.prob <- colMeans(z.MCMC, na.rm = TRUE)

library(reshape2)
z.long <- melt(z.prob)
z.long$names <- rownames(z.long)


str_split_fixed(z.long$names, pattern = "[", n = 6)



library(splitstackshape)
z.long2 <- cSplit(z.long, "names", sep=", ", type.convert=FALSE)

colnames(z.long2) <- c("probability", "bee.species", "plant.species", "month")

for(i in 1:nrow(z.long2)){
  z.long2$bee.species[i] <- str_replace_all(z.long2$bee.species[i], "[[:punct:]]", "")
  z.long2$bee.species[i] <- str_replace_all(z.long2$bee.species[i], "z", "")
  z.long2$month[i] <- str_replace_all(z.long2$month[i], "[[:punct:]]", "")
}



head(z.long2)



z.long3 <- z.long2 %>%
  group_by(bee.species, plant.species) %>%
  summarize(max_prob = max(probability))

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
write.csv(z.long3, "./Data/bee-plant-mod-probabilities.csv")



# End script

