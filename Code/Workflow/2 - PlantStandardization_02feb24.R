###############################
## Standardizing plant lists ##
###############################

# written by M.J.Lee February 2024
# this code is part of the cleaning process for matching the SCI plant list and Globi interactions. the goal is to make a standardized name column for each dataset and then merge the files




# this script will do the following:
### 1 - load libraries
### 2 - load plant datalist (txt) from SBBG and edited by Katja
### 3 - run the SCI plant list through the Taxonomic Name Resolution Service
### 4 - load GloBI interactions list
### 5 - run the globi list through the Taxonomic Name Resolution Service
### 6 - merge datasets based on standardized plant name
### 7 - write new file to be added to ongoing cleaning workflow








#### 1 - load libraries ####
library(tidyverse)
#library(Taxonstand) Taxonstand is no longer available because ThePlantList has been deprecated
library(here)
library(TNRS)







#### 2 - load SCI plant list ####
# txt file from SBBG that we will not publish on github 
# file is sourced from google drive. titled ""

sci.plants <- read_csv(here("Data/plantsci_namesAndTraits_021124.csv")) %>% 
  mutate(id = row_number())

spec.names <-sci.plants %>%
  mutate(spec.name = paste(Genus, Species, " ")) %>% 
  dplyr::select(id, spec.name) %>% 
  filter(id != 56)







#### 3 - run TNRS for SCI plant list ####

stand.sci <- TNRS(
  spec.names,
  sources = "wfo",
  classification = "wfo",
  mode = "resolve",
  matches = "best"
)

# check1 <- stand.sci %>% 
#   filter(Overall_score < 1)

update.stand.sci <- stand.sci %>% 
  dplyr::select(ID, Name_submitted, Overall_score, Name_matched) %>% 
  mutate(resolvedName = ifelse(Overall_score == 1 & Overall_score < 0.5, Name_submitted, Name_matched)) %>% 
  mutate(plant_stand = resolvedName)
# check1 <- update.stand.sci %>% 
#    filter(Overall_score < 1)


# full SCI checklist came back with 4 errors:
#1: The specific epithet of Quercus x could not be matched, and multiple corrections are possible. This entry will be filtered out
#2: Erythranthe cardinalis is not being matched. It is only matched at the genus level. We will keep the full species name here.
#3: Looks like one entry might have been dropped -- two species of Cirsium occidentale

# manual corrections:
update.stand.sci$plant_stand[update.stand.sci$plant_stand=="Erythranthe"] <- "Erythranthe cardinalis"










#### 3.5 - create new species column and remerge with sci list ----

# create new version of sci.plants
sci.plants.join <- sci.plants %>% 
  mutate(name_old = paste(Genus, Species, " "))


# using TNRS output, create new column with species name called "plant_stand"
stand_sci_1 <- update.stand.sci %>%
  select(-Name_matched, -Overall_score) %>% 
  left_join(sci.plants.join, by = c("Name_submitted" = "name_old"))

# check for duplicates in SCI plant list using the standardized "plant_stand" column
dup.sci.plants.stand <- stand_sci_1 %>% 
  group_by(plant_stand) %>%  
  mutate(num_rows = sum(n())) %>% 
  filter(num_rows > 1)
# 2 duplicates since the update in 2024
# a total of 36 species from the SCI plant list are considered duplicates of one another for a total of 16 extras
# however, merging this dataset with the globi dataset is adding a total of 3708 entries to the globi interaction list because of the duplicates



# extract one value from the duplicates that has the longest phenology period
slice.sci.plants.stand1 <- dup.sci.plants.stand %>%
  ungroup(.) %>% 
  mutate(sumPhenology = rowSums(.[31:42])) %>% 
  group_by(plant_stand) %>%  
  slice_max(sumPhenology, n = 1) %>% 
  slice(1) %>% 
  select(-num_rows, -sumPhenology)


# take df that had all SCI plants and remove all potential duplicates
# I think this needs to be renamed as "resolvedPlantNames"
stand_sci_2 <- anti_join(stand_sci_1, dup.sci.plants.stand, by = "plant_stand") %>% 
  # remerge extracted duplicates to the SCI plant list
  rbind(., slice.sci.plants.stand1) %>% 
  rename(resolvedPlantNames = plant_stand) %>% 
  dplyr::select(-ID, -id, -resolvedName, -Name_submitted, -'Current Name Full') %>% 
  # remove entry where species == x (i.e., the Quercus stand alone genus)
  filter(Species != "x")






# write csv for taxonstand sci list for easy use later

write.csv(x = stand_sci_2, file = here("Data/resolvedplantsci_12feb24.csv"), row.names = FALSE)













#### 4 - load GloBI interactions list ####
# csv file from GloBI that is too large for github
# file is sourced from google drive. titled ""

# using globi dataset cleaned using the capstone student workflow
# NOTE: The CSV file generated from the previous workflow is too large for github. Should be pulled from a local location and called "globi.dat"
globi.dat <- read_tsv("~/Downloads/all_bee_data_unique_01feb24.tsv")







#### 5 - run taxonstand for globi lists ####

# make unique list of globi plants so the TPL doesn't take as long
plants.globi.dat <- globi.dat %>% 
  select(targetTaxonSpeciesName)

unique.plants.globi.dat <- data.frame(unique(plants.globi.dat)) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
# feb 2024: updated to 9147 plants
# taking unique list of globi plants ends up with 5654 plants

startTime <- Sys.time()
stand.globi <- TNRS(
  unique.plants.globi.dat,
  sources = "wfo",
  classification = "wfo",
  mode = "resolve",
  matches = "best"
)
endTime <- Sys.time()
endTime - startTime
# feb 2024: 34 seconds
# mar 2024: 37 seconds



# check for values that have less than 1 of overall score
globcheck <- stand.globi %>% 
  filter(Overall_score < 1) %>% 
  filter(Name_matched_rank == "species") # these are the ones to keep


# list of =1
fullglob <- stand.globi %>% 
  filter(Overall_score == 1) %>% 
  rbind(globcheck)
# 9088 entries from the original 9145
# loss of 58 plant entries





#### 5.5 - create new species column and remerge with globi list ----

# take the output from the TNRS workflow and choose columns, rename to match globi later

globi.stand.join <- fullglob %>% 
  dplyr::select(Name_submitted, Name_matched) %>% 
  rename(resolvedPlantNames = Name_matched)


# join standardized plant name list with the original globi dataset

globi.stand <- globi.dat %>% 
  left_join(globi.stand.join, by = c("targetTaxonSpeciesName" = "Name_submitted"))

# how many entries will be dropped?
check.globi.stand <- globi.stand %>% 
  filter(!is.na(resolvedPlantNames)) 
# 185832 dropped
# which means the dataset of interactions moving into the next phase will be 380 597
# HOWEVER, I think this still includes Apis mellifera interactions which will need to be removed at some point

# this file is also very large and should be downloaded to a local location
write.csv(check.globi.stand, here("Data/resolvedplantnamesglobi_12feb24.csv"), row.names = FALSE)



