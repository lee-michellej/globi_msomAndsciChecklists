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

sci.plants <- read_tsv("Data/plants-SCI_traits_ml_2021_10_02.txt")





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
stand_sci_1 <- stand_sci %>%
  select(Taxon, New.Genus, New.Species) %>% 
  unite("resolvedPlantNames", New.Genus:New.Species, sep= " ", remove = FALSE)

# remerge to main stand_sci
stand_sci_2 <- left_join(sci.plants, stand_sci_1, by = c("scientificName" = "Taxon"))

# check for duplicates in SCI plant list using the standardized "plant_stand" column
dup.sci.plants.stand <- stand_sci_2 %>% 
  group_by(resolvedPlantNames) %>%  
  mutate(num_rows = sum(n())) %>% 
  filter(num_rows > 1)
# a total of 36 species from the SCI plant list are considered duplicates of one another for a total of 16 extras
# however, merging this dataset with the globi dataset is adding a total of 3708 entries to the globi interaction list because of the duplicates



# extract one value from the duplicates that has the longest phenology period
slice.sci.plants.stand1 <- dup.sci.plants.stand %>%
  ungroup(.) %>% 
  mutate(sumPhenology = rowSums(.[27:38])) %>% 
  group_by(resolvedPlantNames) %>%  
  slice_max(sumPhenology, n = 1) %>% 
  slice(1) %>% 
  select(-num_rows, -sumPhenology)


# take df that had all SCI plants and remove all potential duplicates
stand_sci_3 <- anti_join(stand_sci_2, dup.sci.plants.stand, by = "resolvedPlantNames") %>% 
  # remerge extracted duplicates to the SCI plant list
  rbind(., slice.sci.plants.stand1)






# write csv for taxonstand sci list for easy use later

write.csv(stand_sci_3, "Data/resolvedplantsci_011722.csv", row.names = FALSE)











#### 4 - load GloBI interactions list ####
# csv file from GloBI that is too large for github
# file is sourced from google drive. titled ""

# using globi dataset cleaned using the capstone student workflow
# NOTE: The CSV file generated from the previous workflow is too large for github. Should be pulled from a local location and called "globi.dat"
#globi.dat <- read_csv("interactions.csv")







#### 5 - run taxonstand for globi lists ####

# make unique list of globi plants so the TPL doesn't take as long
plants.globi.dat <- globi.dat %>% 
  select(targetTaxonSpeciesName)

unique.plants.globi.dat <- data.frame(unique(plants.globi.dat))
# taking unique list of globi plants ends up with 5654 plants

startTime <- Sys.time()
resolved_globi1 <- data.frame(TPL(unique.plants.globi.dat$targetTaxonSpeciesName))
endTime <- Sys.time()
startTime - endTime
# runs about 40 minutes




#### 5.5 - create new species column and remerge with globi list ----

# using TPL output, create new column with species name called "plant_stand"
stand_globi_short <- resolved_globi1 %>% 
  select(Taxon, New.Genus, New.Species) %>% 
  unite("resolvedPlantNames", New.Genus:New.Species, sep= " ", remove = FALSE)

# remove NAs
stand_globi_short1 <- na.omit(stand_globi_short)
# three NAs removed


# remerge unique list of species names with whole globi dataset (so there will be duplicates)
globi.dat.plantstand <- left_join(globi.dat, stand_globi_short1, by = c("targetTaxonSpeciesName" = "Taxon"))


# write csv for taxonstand sci list for easy use later
# this file is also very large and should be downloaded to a local location
# write.csv(globi.dat.plantstand, "resolvedplantnamesglobi_011722.csv", row.names = FALSE)








#### 6 - merge datasets based on standardized plant name ####

attempt_merge <- left_join(globi.dat.plantstand, slice.sci.plants.stand, by = "resolvedPlantNames", keep = FALSE)
# length of 259009

#check the number of globi-based observations without a match to the sci list
failed_merge <- anti_join(globi.dat.plantstand, slice.sci.plants.stand, by = "resolvedPlantNames")
# length of 239692
# 19317 should have merged with the SCI dataset




#### 7 - write new file to be added to ongoing cleaning workflow ####
write.csv(mergedfinal, "Data/mergedplantlists.csv", row.names = FALSE)
