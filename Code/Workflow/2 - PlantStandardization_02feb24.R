########################################
########################################
# This code was written by: M. J. Lee
# Date: February 2024
# If you have any questions, please email: michellejlee@ucsb.edu
########################################
########################################



##################################
######## Code Objective ##########
##################################


# This code standardizes plant species names using the Taxonomic Name Resolution
# Service (TNRS). Plant names in different datasets may use synonyms or outdated
# nomenclature. By resolving all names to a standard taxonomy (World Flora Online),
# we can accurately match plant species between:
#   1. The Santa Cruz Island (SCI) plant checklist
#   2. The GloBI interaction dataset



##################################
######## Input Data ##############
##################################


# 1. plantsci_namesAndTraits_021124.csv - SCI plant checklist from SBBG
# 2. all_bee_data_unique_01feb24.tsv - Cleaned GloBI data (from Script 0)



##################################
######## Output of Code ##########
##################################


# 1. resolvedplantsci_12feb24.csv - SCI plant list with standardized names
# 2. resolvedplantnamesglobi_12feb24.csv - GloBI data with standardized plant names



##################################
######## Table of Contents #######
##################################


# 1. Load libraries
# 2. Load SCI plant list
# 3. Run TNRS for SCI plant list
# 3.5. Handle duplicates and merge with SCI data
# 4. Load GloBI interactions list
# 5. Run TNRS for GloBI plant list
# 5.5. Merge standardized names with GloBI data



##################################
##################################
##################################








# 1. Load libraries -------------------------------------------------------



library(tidyverse)
library(here)
library(TNRS)  # Taxonomic Name Resolution Service
# Note: Taxonstand package is no longer available (ThePlantList deprecated)







# 2. Load SCI plant list -------------------------------------------------------



# Load Santa Cruz Island plant checklist from Santa Barbara Botanic Garden (SBBG)
# Note: This file is too large for GitHub and should be sourced locally
sci.plants <- read_csv(here("Data/plantsci_namesAndTraits_021124.csv")) %>%
  mutate(id = row_number())

# Create a simplified species name list for TNRS lookup
# Format: "Genus Species" (with trailing space for TNRS compatibility)
spec.names <- sci.plants %>%
  mutate(spec.name = paste(Genus, Species, " ")) %>%
  dplyr::select(id, spec.name) %>%
  filter(id != 56)  # Remove problematic entry







# 3. Run TNRS for SCI plant list -------------------------------------------------------



# Use Taxonomic Name Resolution Service to standardize plant names
# Source: World Flora Online (wfo) - the current standard for plant taxonomy
stand.sci <- TNRS(
  spec.names,
  sources = "wfo",
  classification = "wfo",
  mode = "resolve",
  matches = "best"
)

# Process TNRS results
# Overall_score = 1 means perfect match; < 1 means partial match
update.stand.sci <- stand.sci %>%
  dplyr::select(ID, Name_submitted, Overall_score, Name_matched) %>%
  mutate(resolvedName = ifelse(Overall_score == 1 & Overall_score < 0.5,
                               Name_submitted, Name_matched)) %>%
  mutate(plant_stand = resolvedName)

# TNRS matching issues encountered:
# 1. "Quercus x" - specific epithet could not be matched (will be filtered out)
# 2. "Erythranthe cardinalis" - only matched at genus level (manual correction below)
# 3. Two species of Cirsium occidentale - one entry may have been dropped

# Manual correction for Erythranthe cardinalis
update.stand.sci$plant_stand[update.stand.sci$plant_stand == "Erythranthe"] <- "Erythranthe cardinalis"










# 3.5. Handle duplicates and merge with SCI data -------------------------------------------------------



# Create version of sci.plants with original name for joining
sci.plants.join <- sci.plants %>%
  mutate(name_old = paste(Genus, Species, " "))

# Join TNRS results back to original SCI plant data
stand_sci_1 <- update.stand.sci %>%
  select(-Name_matched, -Overall_score) %>%
  left_join(sci.plants.join, by = c("Name_submitted" = "name_old"))

# Identify duplicate species after name standardization
# (Different original names may resolve to the same standardized name)
dup.sci.plants.stand <- stand_sci_1 %>%
  group_by(plant_stand) %>%
  mutate(num_rows = sum(n())) %>%
  filter(num_rows > 1)
  # Result: 2 duplicates (36 species total considered duplicates, 16 extras)

# For duplicates, keep the entry with the longest phenology period
# (Columns 31-42 contain monthly phenology data)
slice.sci.plants.stand1 <- dup.sci.plants.stand %>%
  ungroup(.) %>%
  mutate(sumPhenology = rowSums(.[31:42])) %>%
  group_by(plant_stand) %>%
  slice_max(sumPhenology, n = 1) %>%
  slice(1) %>%
  select(-num_rows, -sumPhenology)

# Create final SCI plant list:
# 1. Remove all duplicates from original
# 2. Add back the selected duplicate entries
# 3. Clean up columns
stand_sci_2 <- anti_join(stand_sci_1, dup.sci.plants.stand, by = "plant_stand") %>%
  rbind(., slice.sci.plants.stand1) %>%
  rename(resolvedPlantNames = plant_stand) %>%
  dplyr::select(-ID, -id, -resolvedName, -Name_submitted, -'Current Name Full') %>%
  filter(Species != "x")  # Remove "Quercus x" (genus-only entry)




# Export standardized SCI plant list
write.csv(x = stand_sci_2, file = here("Data/resolvedplantsci_12feb24.csv"), row.names = FALSE)













# 4. Load GloBI interactions list -------------------------------------------------------



# Load cleaned GloBI dataset from Script 0 (RemoveDupsInGloBI.R)
# Note: This file is too large for GitHub - must be sourced locally
globi.dat <- read_tsv("~/Downloads/all_bee_data_unique_01feb24.tsv")







# 5. Run TNRS for GloBI plant list -------------------------------------------------------



# Extract unique plant names from GloBI to speed up TNRS processing
plants.globi.dat <- globi.dat %>%
  select(targetTaxonSpeciesName)

unique.plants.globi.dat <- data.frame(unique(plants.globi.dat)) %>%
  mutate(id = row_number()) %>%
  select(id, everything())
  # Result: ~9,147 unique plant names (Feb 2024)

# Run TNRS on unique GloBI plant names
# Processing time: ~34-37 seconds
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

# Filter TNRS results to keep only reliable matches:
# 1. Perfect matches (Overall_score = 1)
# 2. Partial matches that resolved to species level
globcheck <- stand.globi %>%
  filter(Overall_score < 1) %>%
  filter(Name_matched_rank == "species")  # Keep partial matches at species level

fullglob <- stand.globi %>%
  filter(Overall_score == 1) %>%
  rbind(globcheck)
  # Result: 9,088 entries kept from original 9,145 (58 plant entries lost)





# 5.5. Merge standardized names with GloBI data -------------------------------------------------------



# Prepare TNRS results for joining with GloBI data
globi.stand.join <- fullglob %>%
  dplyr::select(Name_submitted, Name_matched) %>%
  rename(resolvedPlantNames = Name_matched)

# Join standardized plant names back to original GloBI dataset
globi.stand <- globi.dat %>%
  left_join(globi.stand.join, by = c("targetTaxonSpeciesName" = "Name_submitted"))

# Filter to keep only records with successfully resolved plant names
check.globi.stand <- globi.stand %>%
  filter(!is.na(resolvedPlantNames))
  # Result: 380,597 interactions retained (185,832 dropped due to unmatched names)
  # Note: This still includes Apis mellifera interactions (removed in Script 3)

# Export GloBI data with standardized plant names
# Note: This file is large and should be stored locally
write.csv(check.globi.stand, here("Data/resolvedplantnamesglobi_12feb24.csv"), row.names = FALSE)


# End script



