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
dat <- read.csv("./Data/all_bee_data_unique.csv")
                  #header = TRUE,
                  #sep = "\t")



# Look at data structure
str(dat)




# 3. Subset to only plant records -------------------------------------------------------


# next step would be to create new datasets that also only contain records that also reference plants (Plantae). I would do this independent of the interaction type as authors may use many interactions type names. 



# Pull out if the source or target are bees
plant.source <- grep("Plantae", dat$targetTaxonPathNames)
plant.target <- grep("Plantae", dat$sourceTaxonPathNames)


# Only take unique observations
plant.rows <- unique(c(plant.source, plant.target))

# Keep only bee rows
dat1 <- dat[plant.rows,]


# Determine number of bee species
n.bee <- length(unique(dat1$sourceTaxonGenusName))

# Determine the unique number of studies
n.study <- length(unique(dat1$sourceCitation))


# Add a presence column
dat1$Prez <- 1



# 4. Summarize the total number of parasites on each bee per study -------------------------------------------------------



####### NEED TO REMOVE PLANTS THAT ARE BEING DOUBLE COUNTED #######
# EACH BEE-PLANT INTERACTION BEING COUNTED SHOULD BE UNIQUE
# RIGHT NOW - THEY ARE BEING DOUBLE COUNTED


globi.dat <- dat1 %>%
            group_by(sourceTaxonGenusName, sourceCitation) %>%
            summarise(n.plant = sum(Prez)) %>%
            pivot_wider(names_from = sourceCitation, 
                        values_from = n.plant)

View(globi.dat)



# 5. Save the data -------------------------------------------------------


save(globi.dat, file= "./Data/globi_data_formatted_bee_plant.rds")




# End script
