# Clean globi dataset
# written by Michelle Lee and Katja Seltmann
# 02 February 2024

# The purpose of this code is to take the latest download of Globi data for bees and remove duplicates that have been added due to recent publications and SCAN additions. These artificial duplicates could impact the probabilities of detecting these various interactions. These additions were discovered by Katja Seltmann.

# To begin, we download the latest bee interaction dataset that was pulled from Globi and then cleaned by KS. This data is published to Zotero: https://zenodo.org/records/10552937. We used the file "indexed_interactions_bees.tsv.zip".





# Libraries and data needed ======
library(tidyverse)
dat <- read_csv("~/Downloads/interactions.csv")
# from the standardization step --> 656 525






# The following columns will be used to help sort the data as needed ==========
# filter out data that does not have all three columns:
# sourceInstitutionCode -- column 25
# sourceCollectionCode 26
# sourceCatalogNumber 27









# First, we will filter data that does NOT have a sourceCatalogNumber =======

# we want to check for duplicates that DO have sourceCatalogNumber
hasCatNumber <- dat %>% 
  filter(!is.na(sourceCatalogNumber))
# this leaves 540 456 observations with sourceCatalogNumber



# here we will keep data that does not have a sourceCatalogNumber as these come from another source that are not likely to have been duplicated in the last SCAN update
keep1 <- dat %>% 
  filter(is.na(sourceCatalogNumber))
# 116 069 observations without sourceCatalogNumber
# we immediately keep these




# QUESION -- does filtering in the above step result in a different end value if we were to use a different column such as sourceCollectionCode or sourceInstitutionCode
# hasCollectionCode <- dat %>% 
#   filter(!is.na(sourceCollectionCode))
# this leaves 530885 observations with sourceCollectionCode
# 
# hasInstituionCode <- dat %>% 
#   filter(!is.na(sourceInstitutionCode))
# this leaves 662,169 observations


# some summary information
# 668044 have sourceCatalogNumber
# 662169 have sourceInstitutionCode
# 530885 have sourceCollectionCode
# 508912 have all three variables

# so, sourceCatalogNumber encompasses more variables than either of the other two.
# ? how many variables have all three columns?
# 
# allthree <- dat %>% 
#   filter(!is.na(sourceCatalogNumber) & !is.na(sourceInstitutionCode) & !is.na(sourceCollectionCode))














# create a column that concatenates the three above columns =======
dupsAssigned <- hasCatNumber %>% 
  mutate(dupCheck = paste0(sourceInstitutionCode, "-",
                           sourceCollectionCode, "-",
                           sourceCatalogNumber)) %>%
  group_by(dupCheck) %>% 
  mutate(dupLength = length(dupCheck)) %>% 
  ungroup()


# filter for set with replicates =======  
hasDups <- dupsAssigned %>% 
  filter(dupLength > 1)
# 175 521 observations with replicates

keep2 <- dupsAssigned %>% 
  filter(dupLength == 1) %>% 
  select(-dupLength, -dupCheck)


# only select first of each duplicate ========
keep3 <- hasDups %>% 
  group_by(dupCheck) %>% 
  slice(1) %>% 
  ungroup()
# then gets broken down to 85 425 files
# 175521-85425
# 90096/840315 were dups based on this filtering process 10.7%
# but this doesn't include the values that were removed in the standardization workflow

keep3_edit <- keep3 %>% 
  dplyr::select(-dupLength, -dupCheck)


# new file =========

keep <- rbind(keep1, keep2, keep3_edit)
#566429/840315
# keeping 67.4% of the original dataset

# new file for duplicates
write_tsv(hasDups, "~/Downloads/allDups_01feb24.tsv")

# new file to take into analyses
write_tsv(keep, "~/Downloads/all_bee_data_unique_01feb24.tsv")


#check <- read_tsv("~/Downloads/allDups_01feb24.tsv")

