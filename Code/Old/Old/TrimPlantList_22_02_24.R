# Temporarily trim plant list for model troubleshooting #
# created by mjlee 24 feb 22

# the purpose of this code is to find candidate plant families to trim. the goal is that this list shortening will be temporary, so the model will take less time to run. trimming this list will also be biologically based.


# load libraries and data files ---------
library(tidyverse)



# cleaned globi interactions for model ------
setwd("~/Desktop/globi_bees/Data")
dat.readin <- read_csv("final-globi-list-clean 2022 02 01.csv")
globi.fam.list <- dat.readin %>% 
  select(targetTaxonFamilyName) %>% 
  rename(Family = targetTaxonFamilyName)
# cleaned list of 566 interactions from globi for the dataset

unique.globi.list <- unique(globi.fam.list)
# 32 unique plant families in the cleaned list

globi.famgen.list <- dat.readin %>% 
  select(targetTaxonFamilyName, targetTaxonGenusName) %>% 
  rename(Family = targetTaxonFamilyName,
         Genus = targetTaxonGenusName) %>% 
  mutate(fam.gen = paste(Family, Genus, sep = '.'))


unique.globi.fam.gen <- unique(globi.famgen.list)
# 68 family genus combinations




# sci plant list ------
setwd("~/Downloads")


sci <- read_csv("resolvedplantsci_041122.csv")
# full list is 568 plants (pre-shortening from mid february)

sci.fam <- sci %>% 
  select(Family)

unique.scifam <- unique(sci.fam)
# 86 unique families from this list


sci.famgen <- sci %>% 
  mutate(fam.gen = paste(Family, Genus, sep = '.'))





# match lists by family ------
# match family list with the list of plants that are part of the final globi dataset

shared.fam <- as.list(unique.globi.list)

# take the unique list of families from the santa cruz island list

filtered.byFam <- filter(sci.fam, sci.fam$Family %in% unique.globi.list$Family)
# 370 unique plant entries which cuts down the overall list considerably
# however, the goal is to cut it down even more than this



# how many plants are on this list that don't have an interaction? -----

sci.shortened.byFam <- filter(sci,
                              sci$Family %in% unique.globi.list$Family)

sci.shortened.byFam.matched <- filter(sci.shortened.byFam, sci.shortened.byFam$resolvedPlantNames %in% dat.readin$resolvedPlantNames)
# out of the 370 unique plant entries kept, only 75 are actually in the globi interaction database. thus through this filtering process by FAMILY, we keep 295 plants that do not have a recorded interaction



# match lists by family & genus ------
filtered.byGen <- filter(sci.famgen, 
                         sci.famgen$fam.gen %in% unique.globi.fam.gen$fam.gen)
# this filter down to 151 plants

# how many plants are on this list that don't have an interaction? -----

sci.shortened.byFam <- filter(sci,
                              sci$Family %in% unique.globi.list$Family)

sci.shortened.byGen.matched <- filter(sci.shortened, sci.shortened$resolvedPlantNames %in% dat.readin$resolvedPlantNames)
# out of the 151 unique plant entries kept, only 75 are actually in the globi interaction database. thus through this filtering process by FAMILY & GENUS, we keep 76 plants that do not have a recorded interaction



# print data file of shortened plants (151) ------------

sci.shortened <- filter(sci.famgen,
                        sci.famgen$fam.gen %in% unique.globi.fam.gen$fam.gen)

sci.shortened.final <- sci.shortened %>% 
  select(-fam.gen)
  
write.csv(sci.shortened.final, "short_resolvedplantsci_041122.csv", row.names = FALSE)



# without apis | make shortened list ---------------


globi.apis.list <- dat.readin %>% 
  select(targetTaxonFamilyName, sourceTaxonGenusName) %>% 
  rename(Family = targetTaxonFamilyName,
         beeGenus = sourceTaxonGenusName) %>% 
  filter(beeGenus != "Apis") %>% 
  select(Family)
# cleaned list of 566 interactions from globi for the dataset
# cutting out Apis brings list to 412 interactions

unique.noapis.list <- unique(globi.apis.list)
# 26 unique plant families in the cleaned list




globi.noapis.famgen.list <- dat.readin %>% 
  select(targetTaxonFamilyName, 
         targetTaxonGenusName, 
         sourceTaxonGenusName) %>% 
  rename(Family = targetTaxonFamilyName,
         Genus = targetTaxonGenusName,
         beeGenus = sourceTaxonGenusName) %>% 
  mutate(fam.gen = paste(Family, Genus, sep = '.')) %>% 
  filter(beeGenus != "Apis") %>% 
  select(-beeGenus)


unique.noapis.fam.gen <- unique(globi.noapis.famgen.list)
# 59 family genus combinations








# without apis | match lists by family ------
# match family list with the list of plants that are part of the no apis globi interaction dataset


filtered.byFam <- filter(sci.fam, 
                         sci.fam$Family %in% unique.noapis.list$Family)
# 341 unique plant entries which cuts down the overall list considerably
# however, the goal is to cut it down even more than this





# without apis | match lists by family & genus ------
filtered.byGen <- filter(sci.famgen, 
                         sci.famgen$fam.gen %in% unique.noapis.fam.gen$fam.gen)
# this filter down to 126 plants




# how many plants included not on noapis interaction list -----
sci.shortened.final.noapis.matched <- filter(sci.shortened.final.noapis, sci.shortened.final.noapis$resolvedPlantNames %in% dat.readin$resolvedPlantNames)
# 66 plants kept from original globi match
# thus 126-66 = 60 species not in the globi database in this file



# print data file of shortened plants without apis (126) ------------

sci.shortened.final.noapis <- filtered.byGen %>% 
  select(-fam.gen)

write.csv(sci.shortened.final.noapis, "short_noapis_resolvedplantsci_041122.csv", row.names = FALSE)
