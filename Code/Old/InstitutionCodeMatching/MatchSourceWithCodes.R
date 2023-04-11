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

setwd("~/Downloads")


# 1 - open a subset of globi data set
### select column with the code information
### remove spaces
### separate institution from catalog number


#sourceInstitutionCode (shorter version)
interactiondata <- read_tsv("all_bee_data_unique.txt")

# to view a smaller subset
subsetdata <- interactiondata[sample(1:nrow(interactiondata), 50,
                                     replace=FALSE),]










# sourceInstitionCode GLOBI COLUMN

#select just institution code:
globiinstitutioncode <- interactiondata %>% 
  select(sourceInstitutionCode) %>% 
  rename(., code = sourceInstitutionCode)

subsetglobiinstitutioncode <- globiinstitutioncode[sample(1:nrow(globiinstitutioncode), 50, replace=FALSE),]
# doesn't seem to be any dashes needed
# might need to break off institutioncode from SCAN







# sourceCatalogNumber GLOBI COLUMN

#select just catalog number:
globicatalognumber <- interactiondata %>% 
  select(sourceCatalogNumber) %>% 
  mutate(., fullcode = sub(" ","",globicatalognumber$sourceCatalogNumber))
globicatalognumber_edit <- globicatalognumber %>% 
  mutate(., withdashcode = sub("_","-",globicatalognumber$fullcode)) %>% 
  separate(withdashcode, 
           into = c("code", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )


# test above code with subset of data
subsetglobicatalognumber <- globicatalognumber[sample(1:nrow(globicatalognumber), 50, replace=FALSE),]
subsetglobicatalognumber <- subsetglobicatalognumber %>% 
  mutate(., fullcode = sub(" ","",subsetglobicatalognumber$sourceCatalogNumber)) %>% 
  mutate(., withdashcode = sub("_","-",subsetglobicatalognumber$fullcode)) %>% 
  separate(withdashcode, 
           into = c("code", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )




# 2 - open institution list taken and cleaned from https://scan-bugs.org/portal/collections/index.php

setwd("~/Desktop/globi_bees/Data")

institution_codes <- read_csv("institutioncodes_oct21.csv") %>%
  separate(code, 
           into = c("precode", "postcode"), 
           sep = "-",
           remove = FALSE)







# 3 - join with institution code

institutionCodes <- left_join(globiinstitutioncode, institution_codes, by = "code", keep = TRUE) %>% 
  filter(code.x != "NA") %>% 
  filter(is.na(code.y))



# 4 - check for unmatched institutions 

list_needcode <- as.data.frame(unique(institutionCodes$code.x))






# 5 - join with catalog number

catalogNumber <- left_join(globicatalognumber, institution_codes, by = "code", keep = TRUE) %>% 
  filter(code.x != "NA") %>% 
  filter(is.na(code.y))






# retry for a work flow matching by institutioncode and then catalognumbercutup

sourceLists <- interactiondata %>% 
  select(sourceInstitutionCode, sourceCatalogNumber)%>% 
  mutate(., fullcode = sub(" ","",sourceLists$sourceCatalogNumber))
sourceLists_edit <- sourceLists %>% 
  mutate(., withdashcode = sub("_","-",sourceLists$fullcode)) %>% 
  separate(withdashcode, 
           into = c("lettercode", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  ) %>% 
  filter(sourceInstitutionCode != "NA")


# will need to merge separately and then re-merge

df1_institutioncode <- anti_join(sourceLists_edit, institution_codes, by = c("sourceInstitutionCode" = "code"))

df2_institutioncode <- anti_join(df1_institutioncode, institution_codes, by = c("sourceInstitutionCode" = "precode"), keep = TRUE)

df3_instcode <- anti_join(df2_institutioncode, institution_codes, by = c("lettercode" = "code"), keep = TRUE)

df4 <- anti_join(df3_instcode, institution_codes, by = c("lettercode" = "precode"))


neednames <- data.frame("neednames" = unique(df4$sourceInstitutionCode))

df5 <- anti_join(neednames, institution_codes, by = c("neednames" = "precode"))


setwd("~/Desktop")
write.csv(neednames, "institutions_neednames.csv", row.names = FALSE)



### locate list with codes with not much institutiondata
locate1 <- filter(interactiondata, sourceInstitutionCode == "F")
locate2 <- filter(interactiondata, sourceInstitutionCode == "US")
locate <- rbind(locate1, locate2)
setwd("~/Downloads")
write.csv(locate, "FandUScodes.csv", row.names = FALSE)
