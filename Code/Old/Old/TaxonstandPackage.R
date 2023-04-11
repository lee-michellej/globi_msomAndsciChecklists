##############################################
# try out plant package for code integration #
##############################################

# written by M.J.Lee Oct 2021 to explore a plant taxon name package


# will do the following:
### 1 - load libraries
### 2 - load plant datalist (txt) from SBBG and edited by Katja
### 3 - load plant list from GloBI interactions list
### 4 - explore taxon name package and results





# 1 - Load necessary libraries
library(tidyverse)
library(taxize)
library(Taxonstand)



# 2 - Load plant dataset

# will use local data path for my own computer for the plant list
# as we do not have the plant list online

setwd("~/Desktop/2020-2021")
plantlist <- read_tsv("plants-SCI_traits_ml_2021_10_02.txt")

head_plantlist <- head(plantlist)



# globi code
setwd("~/Desktop/globi_bees/Data")
interactiondata <- read_csv("globi_data_subset_bee_plant_2021_05_14.csv")




# 3 - use taxonstand code for SCI checklist


# this function takes the plant list and returns a file with the corrected/accepted name based on TPL (The Plant List database)
# want to take the first column of this file "Taxon"
# and rejoin it to a new version of the dataset with in a new column
resolvelist_SCIchecklist <- left_join(plantlist, 
                                      as.data.frame(TPL(plantlist$scientificName)) %>%
                                        select(Taxon, Genus, Species),
                                      by = c("Genus" = "Genus", "Species" = "Species"))
# Use column "Taxon" (can be renamed) to match two datasets in the future
# full SCI checklist came back with 4 errors:
#1: Gamochaeta ustulata has more than one valid synonym; illegitimate/invalid names were avoided.
#2: Beta vulgaris subsp. maritima has more than one valid synonym; the first entry was selected.
#3: The specific epithet of Quercus x could not be matched, and multiple corrections are possible.
#4: Lycopersicumgenus doesn't exist






# 4 - use taxonstand code for subset of globi dataset
#targetTaxonName = full species name for plant
#targetTaxonGenusName = genus for plant
#targetTaxonSpeciesName = species name (with genus); not a complete dataset


