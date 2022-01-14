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

setwd("~/Desktop/GradGeneral/2020-2021")
sci.plants <- read_tsv("plants-SCI_traits_ml_2021_10_02.txt")





#### 3 - run taxonstand for SCI plant list ####

# "TPL" function takes the plant list and returns a file with the accepted name based on The Plant List database
# want to take the first column of this file "Taxon"
# and rejoin it to a new version of the SCI dataset with in a new column


stand_sci <- TPL(sci.plants$scientificName)


# full SCI checklist came back with 4 errors:
#1: Gamochaeta ustulata has more than one valid synonym; illegitimate/invalid names were avoided.
#2: Beta vulgaris subsp. maritima has more than one valid synonym; the first entry was selected.
#3: The specific epithet of Quercus x could not be matched, and multiple corrections are possible.
#4: Lycopersicumgenus doesn't exist






#### 3.5 - create new species column and remerge with sci list ----

# using TPL output, create new column with species name called "plant_stand"
stand_sci_short <- stand_sci %>% 
  select(Taxon, New.Genus, New.Species) %>% 
  unite("plant_stand", New.Genus:New.Species, sep= " ", remove = FALSE)

# remerge with SCI plant list
sci.plants.stand <- left_join(sci.plants, stand_sci_short, by = c("scientificName" = "Taxon"))

# check for duplicates in SCI plant list using the standardized "plant_stand" column
dup.sci.plants.stand <- sci.plants.stand %>% 
  group_by(plant_stand) %>%  
  mutate(num_rows = sum(n())) %>% 
  filter(num_rows > 1)
# a total of 36 species from the SCI plant list are considered duplicates of one another for a total of 16 extras
# however, merging this dataset with the globi dataset is adding a total of 3708 entries to the globi interaction list because of the duplicates

slice.sci.plants.stand <- sci.plants.stand %>% 
  group_by(plant_stand) %>%  
  slice(1)



# write csv for taxonstand sci list for easy use later

setwd("~/Desktop/GradGeneral/2020-2021")
write.csv(stand_sci, "standardsci_010622.csv", row.names = FALSE)








#### 4 - load GloBI interactions list ####
# csv file from GloBI that is too large for github
# file is sourced from google drive. titled ""

# using globi dataset cleaned using the capstone student workflow
setwd("~/Downloads")
globi.dat <- read_csv("interactions.csv")







#### 5 - run taxonstand for globi lists ####

# make unique list of globi plants so the TPL doesn't take as long
plants.globi.dat <- globi.dat %>% 
  select(targetTaxonSpeciesName)

unique.plants.globi.dat <- data.frame(unique(plants.globi.dat))


stand_globi1 <- data.frame(TPL(unique.plants.globi.dat$targetTaxonSpeciesName))






#### 5.5 - create new species column and remerge with globi list ----

# using TPL output, create new column with species name called "plant_stand"
stand_globi_short <- stand_globi1 %>% 
  select(Taxon, New.Genus, New.Species) %>% 
  unite("plant_stand", New.Genus:New.Species, sep= " ", remove = FALSE)

# remerge with SCI plant list
globi.dat.plantstand <- left_join(globi.dat, stand_globi_short, by = c("targetTaxonSpeciesName" = "Taxon"))


# write csv for taxonstand sci list for easy use later

setwd("~/Downloads")
write.csv(globi.dat.plantstand, "standardplantglobi_010622.csv", row.names = FALSE)








#### 6 - merge datasets based on standardized plant name ####

attempt_merge <- left_join(globi.dat.plantstand, slice.sci.plants.stand, by = "plant_stand", keep = FALSE)
# length of 259009

#check the number of globi-based observations without a match to the sci list
failed_merge <- anti_join(globi.dat.plantstand, slice.sci.plants.stand, by = "plant_stand")
# length of 239692
# 19317 should have merged with the SCI dataset




#### 7 - write new file to be added to ongoing cleaning workflow ####

setwd("~/Desktop/globi_bees/Data")
write.csv(mergedfinal, "mergedplantlists.csv", row.names = FALSE)
