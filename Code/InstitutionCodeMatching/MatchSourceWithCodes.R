##############################
# matching institution codes #
##############################

# written by M.J.Lee Oct 2021 to match institution codes to source of specimen


# will do the following:
### 1 open a globi source subsetted data
### 2 open institution lists
### 3 match sources and institutions
### 4 check for unmatched institutions 

library(tidyverse)

setwd("~/Desktop/globi_bees/Data")







# 1 - open a subset of globi data set
### select column with the code information
### remove spaces
### separate institution from catalog number

interactiondata <- read_csv("globi_data_subset_bee_plant_2021_05_14.csv") %>%
  select(sourceCatalogNumber) %>% 
  mutate(., fullcode = sub(" ","",interactiondata$sourceCatalogNumber))

interactiondata %>% 
  mutate(., withdashcode = sub("_","-",interactiondata$fullcode)) %>% 
  separate(withdashcode, 
           into = c("code", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )




# 2 - open institution list taken and cleaned from https://scan-bugs.org/portal/collections/index.php

institution_codes <- read_csv("institutioncodes_oct21.csv")






# 3 - join tables

fulldf <- left_join(interactiondata, institution_codes, by = "code")





# 4 - check for unmatched institutions 

# some of the unmatched institution codes:
### BBSLID, KWC, HOLO, LACMENT, PUB






