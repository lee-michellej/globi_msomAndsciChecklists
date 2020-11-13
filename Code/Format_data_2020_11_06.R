################################## 
# This code was written by: G. V. DiRenzo
# If you have any questions, please send them to: grace.direnzo@gmail.com
################################## 




# Code objective: To format the data needed to run in an N-mixture model framework
# The data will be arranged in a 2-D matrix with Bee species along the rows, the study ID along the columns, and each cell will have the total number of parasites reported (per bee species and study)



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
dat <- read.csv("./Parasite_Interaction_fromGlobi.csv")


# Look at data structure
str(dat)


# Add presence column
dat$Prez <- 1



# 3. Determine number of unique bee sp. and studies -------------------------------------------------------



# Pull out if the source or target are bees
bee.source <- grep("Apidae", dat$source_taxon_path)
bee.target <- grep("Apidae", dat$target_taxon_path)

# Only take unique observations
bee.rows <- unique(c(bee.source, bee.target))

# Keep only bee rows
dat1 <- dat[bee.rows,]

# Need to pull out the rows where the bee is the targe
bee.target1 <- grep("Apidae", dat1$target_taxon_path)

dat1[bee.target1,]

# I think these bees are already accounted for? Can bees parasitize other bees?

# Determine number of bee species
n.bee <- length(unique(dat$source_taxon_name))

# Determine the unique number of studies
n.study <- length(unique(dat$study_source_citation))




# 4. Summarize the total number of parasites on each bee per study -------------------------------------------------------


# Question: Would Bee names be included in the target_taxon_name column?

globi.dat <- dat1 %>%
            group_by(source_taxon_name, study_source_citation) %>%
            summarise(n.parasite = sum(Prez)) %>%
            pivot_wider(names_from = study_source_citation, 
                        values_from = n.parasite)

View(globi.dat)



# 5. Save the data -------------------------------------------------------


save(globi.dat, file= "./Data/globi_data_formatted.rds")


