##### create new institution code list for globi workflow #####
# code written by M.J.Lee Dec 2021
# the goal of this code is to edit institution list and write new csv file for workflow

##### information about the data ----
# data were pulled from https://scan-bugs.org/portal/collections/index.php and cleaned for workflow
# additional K. Seltmann was able to identify instiutions in the globi dataset that were not on the SCAN list
# institution codes in both the SCAN list and the GloBI list may vary based on the additional information added using hyphens (e.g. there may be records from the same instituion, but under different collections)
# the goal is to match in many possible ways with and without added hyphenated codes


##### code will do the following ----
### 1 load libraries
### 2 open institution list
### 3 edit institution list for many variations of the institution
### 4 write new csv
# note, after step 3, this new file should be included in the checklist workflow. there may need to be some similar edits to the globi "sourceInstitutionCode" as well


# 1 load libraries ----

library(tidyverse)



# 2 open institution list ----

setwd("~/Desktop/globi_bees/Data")
inst_preedit <- read_csv("institutioncodes_2021_12_15.csv")



# 3 edit institution list for many variations of the institution ----
# i.e. separate codes by hyphens/underscores and create additional columns

inst_edit <- inst_preedit %>%
  separate(fullcode, 
           into = c("firstcode", "secondcode"), 
           sep = "-",
           remove = FALSE)



# 4 write new csv file for workflow ----

# this file has more options for the institutions for GloBI interactions to match to
setwd("~/Desktop/globi_bees/Data")
write.csv(inst_edit, "institutioncodes_2021_12_16.csv", row.names = FALSE)
