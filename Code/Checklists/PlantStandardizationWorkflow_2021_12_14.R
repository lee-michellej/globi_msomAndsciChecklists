###############################
## Standardizing plant lists ##
###############################

# written by M.J.Lee December 2021
# this code is part of the cleaning process for matching the SCI plant list and Globi interactions. the goal is to make a standardized name column for each dataset and then merge the files




# this script will do the following:
### 1 - load libraries
### 2 - load plant datalist (txt) from SBBG and edited by Katja
### 3 - run taxonstand for SCI plant list
### 4 - load GloBI interactions list
### 5 - run taxonstand for globi lists
### 6 - merge datasets based on standardized plant name
### 7 - write new file to be added to ongoing cleaning workflow




#### 1 - load libraries ####
library(tidyverse)
library(Taxonstand)






#### 2 - load SCI plant list ####
# txt file from SBBG that we will not publish on github 
# file is sourced from google drive. titled ""

setwd("~/Desktop/2020-2021")
sci <- read_tsv("plants-SCI_traits_ml_2021_10_02.txt")





#### 3 - run taxonstand for SCI plant list ####

# "TPL" function takes the plant list and returns a file with the accepted name based on The Plant List database
# want to take the first column of this file "Taxon"
# and rejoin it to a new version of the SCI dataset with in a new column


stand_sci <- left_join(sci, as.data.frame(TPL(sci$scientificName)) %>%
                         select(Taxon, Genus, Species), 
                       by = c("Genus" = "Genus", "Species" = "Species"))


# full SCI checklist came back with 4 errors:
#1: Gamochaeta ustulata has more than one valid synonym; illegitimate/invalid names were avoided.
#2: Beta vulgaris subsp. maritima has more than one valid synonym; the first entry was selected.
#3: The specific epithet of Quercus x could not be matched, and multiple corrections are possible.
#4: Lycopersicumgenus doesn't exist





#### 4 - load GloBI interactions list ####
# csv file from GloBI that is too large for github
# file is sourced from google drive. titled ""

# filter for only plants and only species names
setwd("~/Downloads")
globi <- read_tsv("all_bee_data_unique.txt")

# look for just plants
plant.source <- grep("Plantae", globi$targetTaxonPathNames)
plant.target <- grep("Plantae", globi$sourceTaxonPathNames)
plant.rows <- unique(c(plant.source, plant.target))
length(plant.rows) #300166 entries
# Keep only plant rows
globi1 <- globi[plant.rows,] %>%
  filter()


setwd("~/Desktop/globi_bees/Data")
matched <- read_csv("matched_rows_2021_09_13.csv")



#### 5 - run taxonstand for globi lists ####

stand_globi1 <- left_join(globi1, as.data.frame(TPL(globi1$scientificName)) %>%
                         select(Taxon, Genus, Species), 
                       by = c("Genus" = "Genus", "Species" = "Species"))

stand_globi1 <- data.frame(TPL(globi1$scientificName))



#### 6 - merge datasets based on standardized plant name ####
#### 7 - write new file to be added to ongoing cleaning workflow ####

