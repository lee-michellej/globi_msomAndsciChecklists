########################################
########################################
# This code was written by: Michelle Lee & Katja Seltmann
# Date: 02 February 2024
# If you have any questions, please email: michellejlee@ucsb.edu
########################################
########################################



##################################
######## Code Objective ##########
##################################


# The purpose of this code is to take the latest download of Globi data for bees
# and remove duplicates that have been added due to recent publications and SCAN
# additions. These artificial duplicates could impact the probabilities of
# detecting these various interactions. These additions were discovered by
# Katja Seltmann.



##################################
######## Input Data ##############
##################################


# The input data is downloaded from Zenodo: https://zenodo.org/records/10552937
# We used the file "indexed_interactions_bees.tsv.zip"
# This file was processed through the standardization step (Script 1) to create
# "interactions.csv"



##################################
######## Output of Code ##########
##################################


# 1. allDups_01feb24.tsv - File containing all duplicate records (for reference)
# 2. all_bee_data_unique_01feb24.tsv - Cleaned Globi file with duplicates removed



##################################
######## Table of Contents #######
##################################


# 1. Load libraries and data
# 2. Identify records with and without sourceCatalogNumber
# 3. Create duplicate check column and identify duplicates
# 4. Keep one record per duplicate group
# 5. Combine and export final dataset



##################################
##################################
##################################





# 1. Load libraries and data -------------------------------------------------------



library(tidyverse)

# Read in the standardized interactions data
# This file comes from the output of Script 1 (SourceTargetStandardization.Rmd)
dat <- read_csv("~/Downloads/interactions.csv")
  # Total observations: 656,525






# 2. Identify records with and without sourceCatalogNumber -------------------------------------------------------


# Key columns for identifying duplicates:
#   - sourceInstitutionCode (column 25)
#   - sourceCollectionCode (column 26)
#   - sourceCatalogNumber (column 27)
#
# We use sourceCatalogNumber as the primary filter because:
#   - 668,044 records have sourceCatalogNumber
#   - 662,169 records have sourceInstitutionCode
#   - 530,885 records have sourceCollectionCode
#   - sourceCatalogNumber captures the most records while being specific enough
#     to identify duplicates from SCAN additions









# Split data into two groups:
#   1. Records WITH sourceCatalogNumber - need to check for duplicates
#   2. Records WITHOUT sourceCatalogNumber - keep as-is (not from SCAN, unlikely duplicates)

# Records WITH sourceCatalogNumber (will check for duplicates)
hasCatNumber <- dat %>%
  filter(!is.na(sourceCatalogNumber))
  # Result: 540,456 observations

# Records WITHOUT sourceCatalogNumber (keep immediately - not from SCAN updates)
keep1 <- dat %>%
  filter(is.na(sourceCatalogNumber))
  # Result: 116,069 observations
  # These come from sources other than SCAN and are unlikely to be duplicated




# NOTE: Alternative filtering approaches were considered but not used:
# - Using sourceCollectionCode: 530,885 observations
# - Using sourceInstitutionCode: 662,169 observations
# - Using all three columns: 508,912 observations
# sourceCatalogNumber was chosen as it captures the most relevant records














# 3. Create duplicate check column and identify duplicates -------------------------------------------------------


# Create a unique identifier by concatenating institution, collection, and catalog codes
# This allows us to identify records that represent the same specimen
dupsAssigned <- hasCatNumber %>%
  mutate(dupCheck = paste0(sourceInstitutionCode, "-",
                           sourceCollectionCode, "-",
                           sourceCatalogNumber)) %>%
  group_by(dupCheck) %>%
  mutate(dupLength = length(dupCheck)) %>%
  ungroup()

# Separate records into duplicates and non-duplicates
hasDups <- dupsAssigned %>%
  filter(dupLength > 1)
  # Result: 175,521 observations have duplicates

# Keep records that appear only once (no duplicates)
keep2 <- dupsAssigned %>%
  filter(dupLength == 1) %>%
  select(-dupLength, -dupCheck)
  # These are unique records - keep all of them


# 4. Keep one record per duplicate group -------------------------------------------------------


# For records with duplicates, keep only the first occurrence of each unique specimen
keep3 <- hasDups %>%
  group_by(dupCheck) %>%
  slice(1) %>%
  ungroup()
  # Result: 85,425 unique records from 175,521 duplicate observations
  # This means 90,096 duplicate records were removed (10.7% of original)

# Remove the helper columns used for duplicate detection
keep3_edit <- keep3 %>%
  dplyr::select(-dupLength, -dupCheck)


# 5. Combine and export final dataset -------------------------------------------------------


# Combine all three "keep" datasets:
#   keep1 = records without sourceCatalogNumber (116,069 obs)
#   keep2 = unique records with sourceCatalogNumber (no duplicates)
#   keep3_edit = one record per duplicate group (85,425 obs)
keep <- rbind(keep1, keep2, keep3_edit)
  # Final dataset: 566,429 observations
  # Retained 67.4% of the original dataset

# Export the duplicate records for reference/documentation
write_tsv(hasDups, "~/Downloads/allDups_01feb24.tsv")

# Export the cleaned dataset for use in subsequent analyses
write_tsv(keep, "~/Downloads/all_bee_data_unique_01feb24.tsv")


# End script

