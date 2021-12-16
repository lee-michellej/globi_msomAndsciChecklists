##### create a workflow for the format checklist to match institution codes #####
# code written by M.J.Lee Dec 2021
# the goal of this code is to create a workflow to match the intitution code list to the GloBI dataset
# this code should be added to the checklist formatting workflow

##### information about the data ----
# institution code data were pulled from https://scan-bugs.org/portal/collections/index.php and K. Seltmann
# the code data was cleaned and separated to account for multiple variations in the GloBI dataset
# the goal of this code will be to download the globi data and make necessary edits
# then check that all occurences with listed institution data are able to match with pre-exisitng institution code list


##### code will do the following ----
### 1 load libraries
### 2 load institution list
### 3 load globi data
### 4 edit globi sourceInsitutionCode (i.e. separate into multiple variations that can be matched)
### 5 merge globi dataset with instition list and check what institions were not matched
# note, after step 5, this code should be included in the checklist format workflow.


# 1 load libraries ----

library(tidyverse)


# 2 load institution list ----

setwd("~/Desktop/globi_bees/Data")
inst.codes <- read_csv("institutioncodes_2021_12_16.csv")



# 3 load globi data ----




# 4 edit globi sourceInstitutionCode ----




