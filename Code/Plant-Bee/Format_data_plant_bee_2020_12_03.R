################################## 
# This code was written by: G. V. DiRenzo
# If you have any questions, please send them to: grace.direnzo@gmail.com
################################## 




# Code objective: To format the data needed to run in an N-mixture model framework

# The data will be arranged in a 2-D matrix with bee species/genera along the rows, the study ID along the columns, and each cell will have the total number of plants that each bee interacts with



################################## 
########  Table of Contents ######
################################## 


# 1. Load libraries & set working directory
# 2. Load data
# 3. Determine number of unique bee sp. and studies
# 4. Summarize the total number of parasites on each bee per study
# 5. Save the data



################################## 
################################## 
################################## 




# 1. Load libraries & set working directory -------------------------------------------------------



# Load libraries
library(tidyverse)
library(rglobi)
library(bipartite)
library(igraph)
library(ggplot2)


# Set working directory
setwd("~/globi_tritrophic_networks/")



# 2. Load data -------------------------------------------------------


# Read in data
dat <- read.csv("~/Desktop/Data_globi/all_bee_data_unique.csv")
                  #header = TRUE,
                  #sep = "\t")



# Look at data structure
str(dat)




# 3. Subset to only plant records -------------------------------------------------------


# Next step would be to create new datasets that also only contain records that also reference plants (Plantae). 
# I would do this independent of the interaction type as authors may use many interactions type names. 



# Pull out if the source or target are bees
plant.source <- grep("Plantae", dat$targetTaxonPathNames)
plant.target <- grep("Plantae", dat$sourceTaxonPathNames)


# Only take unique observations
plant.rows <- unique(c(plant.source, plant.target))

# Keep only bee rows
dat1 <- dat[plant.rows,]


# Pull out all unique bee names
bee.names <- as.character(
               dat$sourceTaxonGenusName[grep("Insecta", dat$sourceTaxonPathNames)],
               dat$targetTaxonGenusName[grep("Insecta", dat$targetTaxonPathNames)])

# Determine the number of unique bee genera there are
n.bee <- length(unique(bee.names))

# Determine the unique number of studies
n.study <- length(unique(dat1$sourceCitation))

# Add a presence column
dat1$Prez <- 1




# 4. Make Insect & Plant columns -------------------------------------------------------




# There are 7 bee families
bee.family <- c("Apidae", "Megachilidae", "Halictidae", "Andrenidae", "Colletidae", "Melittidae", "Stenotritidae")


# Create an empty column
dat1$Insect <- NA
dat1$Plant <- NA


# We will keep any records where bee is reported to genus & plant is reported to family
  # All other records will be discarded
for(i in 1:nrow(dat1)){
  
  bee.check.source <- which(bee.family %in% dat1$sourceTaxonFamilyName[i] == TRUE)
  bee.check.target <- which(bee.family %in% dat1$targetTaxonFamilyName[i] == TRUE)

  # Bees are matched to Genus
  if(length(bee.check.source) > 0){
    dat1$Insect[i] <- as.character(dat1$sourceTaxonGenusName[i])
  } else if(length(bee.check.target) > 0){
    dat1$Insect[i] <- as.character(dat1$targetTaxonGenusName[i])
  } else {
    dat1$Insect[i] <- NA
  }
  
  # Plant is matched to Family
  if(length(grep("Plantae", dat1$sourceTaxonPathNames[i])) > 0){
    dat1$Plant[i] <- as.character(dat1$sourceTaxonFamilyName[i])
  } else if(length(grep("Plantae", dat1$targetTaxonPathNames[i])) > 0){
    dat1$Plant[i] <- as.character(dat1$targetTaxonFamilyName[i])
  } else {
    dat1$Plant[i] <- NA
  }
  
}

# Determine the number of records that will be discarded
na.rows <- unique(c(which(is.na(dat1$Insect) == TRUE), which(is.na(dat1$Plant) == TRUE)))
length(na.rows)


# Should these be removed?
unique(dat1$sourceTaxonFamilyName[na.rows])
# [1] Hesperiidae             Lamiaceae
# 22 records

# Remove NA rows so that it isn't a row in our dataframe
dat2 <- dat1[-na.rows,]

View(dat1[na.rows,])

# Determine how many rows have empty spots
dat3 <- dat2[dat2$Insect == "",]

View(dat3)

nrow(dat3)

nrow(dat3[grep("Insect", dat3$sourceTaxonPathNames),])
nrow(dat3[grep("Plantae", dat3$sourceTaxonPathNames),])


# Determine how many rows have empty spots
dat4 <- dat2[dat2$Plant == "",]

nrow(dat4)

View(dat4)


# Create a dataframe without any of that missing information
dat5 <- dat2[dat2$Insect != "" & dat2$Plant != "", ]

nrow(dat5)


# 4. Summarize the total number of plants each bee interacts with in each study -------------------------------------------------------



# Now, we need to make the wide formatted data
  # Along the rows = each bee genus
  # Along the columns = each study
  # In each cell = the number of unique plant-bee interactions
# We need to be mindful that we are not double counting plant-bee interactions
  # To do this - we will use the unique function

# Use tidyverse to group, summarize, and pivot the data frame
globi.dat <- dat5 %>%
            group_by(Insect, sourceCitation) %>%
            summarise(n.plant = length(unique(Plant))) %>%
            pivot_wider(names_from = sourceCitation, 
                        values_from = n.plant)

# Look at the data frame
View(globi.dat)


# Look at the dimensions
dim(globi.dat)


# Genus names
globi.dat[1,]

# Look at the distribution of counts
hist(apply(globi.dat[,-1], 1, max, na.rm = TRUE), main = "")



# 5. Save the data -------------------------------------------------------


save(globi.dat, file= "./Data/globi_data_formatted_bee_plant_2020_12_04.rds")

write.csv(globi.dat, file= "./Data/globi_data_formatted_bee_plant_2020_12_04.csv")



# End script
