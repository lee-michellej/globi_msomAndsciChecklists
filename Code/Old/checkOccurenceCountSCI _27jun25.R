# What percent of species are represented in occurrence data? #
# july 2025
# mj lee
# The purpose of this code is to compare the existing species in both the bee and plant checklists to available occurrence data

# libraries
library(tidyverse)


# data files =======

plantlist <- read_csv("./Data/plant.phenology2.csv")
# 302 plant species in final manuscript

beelist <- read_csv("./Data/SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv") %>% 
  filter(scientificName != "Apis mellifera",
         scientificName != "Colletes kincaidii", 
         scientificName != "Bombus occidentalis", 
         scientificName != "Halictus harmonius", 
         scientificName != "Eucera lunata"
        ) %>% 
  filter(specificEpithet != "sp.")
# 133 bee species

plantoccur <- read_csv("./Data/plantoccurrences.csv") %>% #6829
  filter(!is.na(specificEpithet)) # 6737
#unique(plantoccur$scientificName) # 991 species

beeoccur <- read_csv("./Data/beeoccurrences.csv") %>% # 3733
  filter(!is.na(specificEpithet)) # this step takes the list down to 2203
#unique(beeoccur$scientificName) # using this column, most are not to species, leaving 103 unique species

# 1. All ocurrence data =======
# count of occurrences for bees ======

countbeeoccur <- beeoccur %>% 
  group_by(scientificName) %>% 
  summarise(sppcount = length(scientificName))

beeover3 <- countbeeoccur %>% 
  filter(sppcount > 3)
# how many species have over 3 = 51 bee species
beeover100 <- countbeeoccur %>% 
  filter(sppcount > 100)

hist(countbeeoccur$sppcount)

###########
beeoverlap <- filter(beeover3, beeover3$scientificName %in% beelist$scientificName)
# 45 bee species in the SCI bee list
# 45/133 = 34%
###########

# count occurrences for plants =====

# need to filter occurrences by what is actually flowering
flowfilter <- plantoccur %>% 
  filter(
    str_detect(reproductiveCondition, "fl|FL|Fl")
  )
# 5436

countplantoccur <- flowfilter %>% 
  group_by(scientificName) %>% 
  summarise(sppcount = length(scientificName))

plantover3 <- countplantoccur %>% 
  filter(sppcount > 3)
# how many species have over 3 = 426 plant species
plantover100 <- countplantoccur %>% 
  filter(sppcount > 100)


###########
plantoverlap <- filter(plantover3, plantover3$scientificName %in% plantlist$scientificName)
# 146 bee species in the SCI bee list
# 146/302 = 48%
###########



# 2. Check globi interaction data just on SCI ======

# read in data (downloaded from drive)
dat4 <- dat2 # pulled from workflow file 3
# however, can also just read in the following which is the same file in theory
dat4 <- read.csv("./Data/matched_rows_2024_04_06.csv")


# Subset the data
# Lat from 31 - 36
# Long from -125 to -116

dat5 <- dat4[dat4$decimalLatitude > 30 & dat4$decimalLatitude < 36 &
               dat4$decimalLongitude > -150 & dat4$decimalLongitude < -116, ]

dat6 <- dat5 %>% filter(!is.na(decimalLatitude))

# 1292 interactions

unique(dat6$resolvedBeeNames) # 66 bees
unique(dat6$resolvedPlantNames) #126


# summarise count of species names here
countbeeint <- dat6 %>% 
  group_by(resolvedBeeNames) %>% 
  dplyr::summarise(sppcount = length(resolvedBeeNames))

hist(countbeeint$sppcount)
beeintover3 <- countbeeint %>% 
  filter(sppcount > 3) #35 bee species
beeintover100 <- countbeeint %>% 
  filter(sppcount > 100) #3 bee species




# summarise count of species names here
countplantint <- dat6 %>% 
  group_by(resolvedPlantNames) %>% 
  dplyr::summarise(sppcount = length(resolvedPlantNames))

hist(countplantint$sppcount)
plantintover3 <- countplantint %>% 
  filter(sppcount > 3) #59 plant species
plantintover100 <- countplantint %>% 
  filter(sppcount > 100) #1 plant species




