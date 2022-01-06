##### create a workflow for the format checklist to match institution codes #####
# code written by M.J.Lee Dec 2021
# the goal of this code is to create a workflow to match the institution code list to the GloBI dataset
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
### 4 edit globi sourceInstitutionCode (i.e. separate into multiple variations that can be matched)
### 5 merge globi dataset with institution list and check what institutions were not matched
# note, after step 5, this code should be included in the checklist format workflow.


# 1 load libraries ----

library(tidyverse)


# 2 load institution list ----

setwd("~/Desktop/globi_bees/Data")
inst.codes <- read_csv("institutioncodes_2021_12_16.csv")



# 3 load globi data ----

setwd("~/Downloads")
globi.dat <- read_csv("interactions_aftercapstonecode_3jan22.csv")
View(head(globi.dat))





# 4 edit globi sourceInstitutionCode ----

globi.inst <- globi.dat %>% 
  select(sourceCatalogNumber)

list.globi.inst <- data.frame(unique(globi.inst))

# issue with semi colons
# issue with periods
# issue with full number codes
# some codes need the dash or need to be separated before dash is removed

globi_instcodes <- globi.inst %>% 
  mutate(., sourceCatalogNumber = gsub(" ","", sourceCatalogNumber)) %>% 
  mutate(., sourceCatalogNumber = gsub("_","", sourceCatalogNumber)) %>%
  mutate(., sourceCatalogNumber = gsub("-","", sourceCatalogNumber)) %>% 
  mutate(., sourceCatalogNumber = gsub(":","", sourceCatalogNumber)) %>% 
  separate(sourceCatalogNumber, 
           into = c("code", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )

list.globi.code <- data.frame(unique(globi_instcodes))
# removing periods didn't work for some reason
# resulting code has some periods left
# i think this would be resolved using the newer version of globi dataset with other code section




# 5 antimerge globi dataset with institution list -----

# what didn't merge
df1_antimerge <- anti_join(globi_instcodes, inst.codes, by = c("code" = "firstcode"))

list.antimerge <- data.frame(unique(df1_antimerge$code))
# 1671 unique entries that don't have a matching institution
# want too filter this list for non-numbers

list.antimerge <- data.frame(unique(df1_antimerge$code)) %>% 
  filter(unique.df1_antimerge.code. == "[0-9]")




filteredlist.antimerge <- filter(list.antimerge$unique.df1_antimerge.code., is.character(list.antimerge$unique.df1_antimerge.code.))

data[!grepl("[A-Za-z]", data$VALUE),]



# 6 merge globi dataset with institution list -----





# 7 print new csv with attached institution codes -----



