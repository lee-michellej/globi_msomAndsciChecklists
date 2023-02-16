########################################
########################################
# This code was written by: G. V. DiRenzo & M. J. Lee
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code objective ##########
##################################


# To format the globi data [4D array (bee species x plant species x month x citation)] for the analysis

# The objective of the analysis is to determine the number of plants that each bee species interacts with while accounting for sampling bias

# We will be using a multi-species occupancy model for the analysis
  # We will account for bee-plant interactions that were not observed
  # We will account for bee-plant interactions that can not occur
    # e.g., bees aren't active when plants are flowering (forbidden links)
  # We will use citations & collections are the "replicate surveys"
  # We will account for the month that citations & collections were searching for bee-plant interactions


##################################
######## Output of Code ##########
##################################


# We should list other outputs
  # maps
  # csv files etc.

# This code generates 1 file: 
  # "./Data/globi_data_formatted_bee_plant_2021_04_05.rds"
  # Object names: globi.dat


##################################
######## Data info ###############
##################################


# The data were downloaded from the Globi database
  # https://www.globalbioticinteractions.org/
  # Observations in Globi are uploaded from museum collections, citizen science obervers, and research studies worldwide
  # These observations vary in terms of objectives, study design, etc.


# The Globi database consists of presence-only data.
  # We subsetted the data to only include bee genera and instances of bees interacting with plants (either as the "source" or "target") that occur in a specific location = Santa Cruz Island + some mainland CA area
    # Lat from 31 - 32
    # Long from -125 to -116
  # We will assign non-detections by compiling a checklist of the bees and plants for Santa Cruz island and assigning a 0 when they are not observed
  

# We do not explicitly consider space in the model, but we subset the records to only include specific localities
  

# We note that databases are plagued by 3 problems:
  # 1. Taxonomic sampling bias
      # Particular species may be sampled more frequently than others because more is known about them or inference is desired on that species
  # 2. Spatial sampling bias
      # Particular areas are easier to reach or sample
  # 3. Detection bias
      # Species detectability changes over time and space as a result of observers or number of surveys
      # Number of observers, quality of observers, length of survey, survey conditions varies

# As a result, ecological patterns may be masked (or there are false patterns) because of observation effort





################################## 
########  Table of Contents ######
################################## 


# 1. Load libraries & set working directory
# 2. Load data
# 3. Format checklist info a little
# 4. General metrics from Globi data
# 5. Format the bee synonym data 
# 6. Subset Globi to rows with the bee-plant interactions that appear in our checklist
# 7. Visualize the data - Make a map with the data point
# 8. Add a new column to globi data with the institution codes to replace SCAN citation
# 9. Format the date column in globi data 
# 10. Create the 4-D array we will fill in
# 11. Determine which bee-plant interactions are possible & which are forbidden
# 12. Fill in the 4-D array for the model
# 13. Create a matrix with the observed bee-plant-month by citation elements
# 14. Save the data



################################## 
################################## 
################################## 







# 1. Load libraries & set working directory -------------------------------------------------------






# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(rgdal)
library(sf)

# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Load data -------------------------------------------------------





# Pre-processing steps to Globi data (that take a while to run):
  # Step 1: Standardizing columns where bee & plant species names appear
    # File names: 1-SourceTargetStandardization.Rmd
  # Step 2: Add resolved plant species names (resolvedPlantName)
            # Will be done on the Globi datset
            # And on the plant phenology list
    # File names: 2-......R

# Read in data
  # Note that this is not in the github repo because the file size is too big
  # This data was downloaded from globi 
  # resolvedplantnamesglobi_011722csv.zip = This is the whole Globi csv file- with the new resolvedPLant names column
dat <- read.csv("~/Dropbox/Globi/Data/Data_globi/resolvedplantnamesglobi_011722.csv")


# Read in bee phenology data with the bee checklist names
bee.list <- read.csv("~/Dropbox/Globi/Data/SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv")

# Other bee names = Synonyms
  # Zenodo synoynm list
  # https://zenodo.org/record/5738043#.YddffBPML0o
bee.names <- read.csv("~/Dropbox/Globi/Data/discoverlife-Anthophila-2022 04 01.csv")

# Other bee names NOT on the Zenodo list but found in Globi
  # Synoynm list for bee species that do not appear in the Zenodo file
    # Heterosarus californicus
    # current valid: Pseudopanurgus californicus (Cresson, 1878)
    # 
    # Exomalopsis cerei
    # current valid: Anthophorula cerei (Timberlake, 1947)
    # 
    # Melissodes lupina
    # current valid: Melissodes lupinus Cresson, 1879
    # 
    # Triepeolus heterurus
    # current valid: Triepeolus utahensis (Cockerell, 1921)
    # 
    # Coelioxys octodentata
    # current valid: Coelioxys octodentatus Say, 1824
bee.names.globi <- read.csv("~/Dropbox/Globi/Data/Globi-names-not-in-discoverlife - Sheet1 2022 04 01.csv")


# Institution codes
institution.codes <- read.csv("Data/institutioncodes_2021_12_16.csv")





################
# Determine if the run will be WITH or WITHOUT Apis mellifera
################



#--------------- WITH Apis



# Read in plant phenology data
# plant.phenology <- read.csv("~/Dropbox/Globi/Data/short_resolvedplantsci_041122.csv")





#--------------- WITHOUT Apis



# Read in plant phenology data
plant.phenology <- read.csv("Data/short_noapis_resolvedplantsci_041122.csv")



# Remove Apis mellifera
bee.list <- bee.list[bee.list$specificEpithet != "mellifera",]



################
################
################




# Look at data structure
str(dat)
  # Look at globi for column definitions

str(bee.list)
  # $ genus : Bee genus
  # $ subgenera : Bee subgenera
  # $ specificEpithet: Bee species name
  # $ infraSpecificEpithet: Bee subspecies name 
  # etc.








# 3. Format checklist info a little -------------------------------------------------------





# The bee list already has the scientific names together:
  # bee.list$scientificName


# The plant list already has the scientific names together:
  # plant.phenology$scientificName


# Number of rows in bee.list = # of species
  # 142
nrow(bee.list)

# Remove species without a species name ( == "sp.")
bee.list <- bee.list[bee.list$specificEpithet != "sp.",]


# Final number of bee species
  # Removed rows not identified to species level
  # 138
nrow(bee.list)




# Confirm that Apis mellifera DOES or DOESNOT appear
which(bee.list$scientificName == "Apis mellifera")







# 4. General metrics from Globi data -------------------------------------------------------






# The downloaded Globi dataset has:
  # 300,465 rows
nrow(dat)



# Remove rows without a bee species name and without plant species name
dat1 <- dat[dat$sourceTaxonSpeciesName != "" & dat$targetTaxonSpeciesName != "", ]
dat1 <- dat1[is.na(dat1$sourceTaxonSpeciesName) == FALSE & 
              is.na(dat1$resolvedPlantNames) == FALSE, ]

# Number of unique observations after removing rows without a bee species name and without plant species name
  # 157,893
nrow(dat1)

# Create a list with all of the unique species in Globi - 
# All of the insect related info in the source column
# All of the plant related info in the target column

# Plant & Bee species
globi.sp <- unique(c(as.character(unique(dat1$sourceTaxonSpeciesName)), 
                     as.character(unique(dat1$targetTaxonSpeciesName))))

# Number of unique bee and plant species (not discriminated)
  # 7,407
length(globi.sp)







# 5. Format the bee synonym data -------------------------------------------------------







# Change the column name from Accepted.Name to resolvedBeeNames
colnames(bee.names.globi)[grep("Accepted.Name", colnames(bee.names.globi))] <- "resolvedBeeNames"


# Look at the structure of the bee synonym list
str(bee.names)
  # resolvedName = latest name
  # providedName = list of all possible synoynms

str(bee.names.globi)
  # resolvedBeeNames = latest name
  # sourceTaxonSpeciesName = list of all possible synoynms


# Filter out homonyms - this resolves the "no:match" issue we were encountering before
bee.names <- bee.names[bee.names$relationName != "HOMONYM_OF",]


# Total number of unique species in the synonym list: 
  # 20,460
length(unique(bee.names$resolvedName))

# Do all of the bee species in our checklist appear in the synoynm list?
  # All listed as true
bee.list$scientificName %in% bee.names$providedName

# One species (#68) does not appear - does it appear in the globi bee list
which(bee.names.globi$sourceTaxonSpeciesName == bee.list$scientificName[68])
  # It does



# Make a new column for the resolvedBeeNames
dat1$resolvedBeeNames <- NA

# Use a loop to go through each globi bee species list and try to find a match in the synonym list - if there is a match, then assign the resolvedName
  # This function takes ~ 8 minutes to run
start.time <- Sys.time()
for(i in 1:nrow(dat1)){

  # 1. Identify the row in the bee.names4 dataframe that the name matches
  # Zenodo list
  row.num.zenodo <- which(dat1$sourceTaxonSpeciesName[i] == bee.names$providedName)[1]
  
  # Check the Katja list
  row.num.kat <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.globi$sourceTaxonSpeciesName)[1]
  

  # The previous line needs to come back with a number to do the next command
  if(is.na(row.num.zenodo) == FALSE){
    
    # 2. Replace the Globi name with the current.name
    dat1$resolvedBeeNames[i] <- as.character(bee.names$resolvedName[row.num.zenodo])
  
  } 
  
  if (is.na(row.num.kat) == FALSE ) {
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.globi$resolvedBeeNames[row.num.kat])
    
  }
  
}
end.time <- Sys.time()
beepr::beep(1)

# Determine amount of time to run for loop
end.time - start.time


# Pull out the rows in the GLobi dataset that were NOT matched with any species names in the synoynm list (from Zenodo) - i.e., produced an NA, a "no:match" or a blank
not.matched.bee.rows <- dat1[is.na(dat1$resolvedBeeNames) == TRUE |
                               dat1$resolvedBeeNames == "no:match" |
                               dat1$resolvedBeeNames == "", ]

# Are there any no matches?
  # 0 
length(which(dat1$resolvedBeeNames == "no:match"))

# How many have NAs?
  # 0 
length(which(is.na(dat1$resolvedBeeNames) == TRUE))

# Are there any blanks?
  # 393
length(which(dat1$resolvedBeeNames == ""))


# Number of rows that did not produce matches
  # 396
nrow(not.matched.bee.rows)

# Drop unused levels
not.matched.bee.rows <- droplevels(not.matched.bee.rows)

# Most of the rows that aren't matching is because they are "sp." species
unique(not.matched.bee.rows$sourceTaxonSpeciesName)

# Select the columns you want to keep
unique.rows <- not.matched.bee.rows %>%
                  select(32:52) %>%
                  unique()

# Save output
# write.csv(unique.rows, "./Data/no-resolved-bee-names-2022 02 18.csv")




# Now we have the Globi dataset with updated bee names






# Next we need to update our bee checklist/phenology object with the resolvedNames

# Make a new column for the resolvedBeeNames
bee.list$resolvedBeeNames <- NA

# Use a loop to go through each scientificName and match with the globi species list
for(i in 1:nrow(bee.list)){
  
  # 1. Identify the row in the bee.names4 dataframe that the name matches
  # Zenodo list
  row.num.zen <- which(bee.list$scientificName[i] == bee.names$providedName)[1]
  
  row.num.kat <- which(bee.list$scientificName[i] == bee.names.globi$sourceTaxonSpeciesName)[1]
  
  
  # The previous line needs to come back with a number to do the next command
  if(is.na(row.num.zen) == FALSE){
    
    # 2. Replace the Globi name with the current.name
    bee.list$resolvedBeeNames[i] <- as.character(bee.names$resolvedName[row.num.zen])
    
  } 
  
  if(is.na(row.num.kat) == FALSE){
    
    bee.list$resolvedBeeNames[i] <- as.character(bee.names.globi$resolvedBeeNames[row.num.kat])
    
    }
  
}

# Pull out the rows in the bee.list dataset that were NOT matched with any species names in the synoynm list (from Zenodo)
  # All should have matches
bee.list[is.na(bee.list$resolvedBeeNames) == TRUE |
               bee.list$resolvedBeeNames == "no:match", ]










# 6. Subset Globi to rows with the bee-plant interactions that appear in our checklist -------------------------------------------------------








# Unique bee species
  # 139 species
bee.species <- unique(bee.list$resolvedBeeNames)
bee.species <- bee.species[is.na(bee.species) == FALSE]
bee.species <- bee.species[bee.species != "no:match"]


grep("Apis mellifera", bee.species)

length(bee.species)

# Unique plant species
  # 516 species = complete list
  # 151 species = shortened list WITH Apis
  # 146 species = shortened list WITHOUT Apis
plant.species <- unique(plant.phenology$scientificName)

length(plant.species)



# Add 2 empty columns to fill in  
dat1$Insect <- NA
dat1$Plant <- NA

# Determine if there is a match between Globi and the bee and plant checklists
  # This function takes: ~7 minutes to run
start.time <- Sys.time()
for(i in 1:nrow(dat1)){
  
  if(dat1$resolvedBeeNames[i] %in% bee.species){
    dat1$Insect[i] <- 1
  } else {dat1$Insect[i] <- 0}
  
  if(dat1$resolvedPlantNames[i] %in% plant.species){
    dat1$Plant[i] <- 1
  } else {dat1$Plant[i] <- 0}

}
end.time <- Sys.time()
# beepr::beep(3)

# How long did it take to run the loop?
end.time - start.time



# Sum the plant & insect columns
  # if it = 1; then, there was only 1 match (either bee or plant)
  # if it = 2; then, both bee and plant matched
  # if it = 0; then, there were no matches (bee and plant did NOT match)
dat1$tot <- dat1$Plant + dat1$Insect


# Determine the number of records that will be discarded
remove.rows <- length(which(dat1$tot < 2))

# How many rows will be removed?
  # 150,009
remove.rows


# How many rows will be kept?
  # 7,884
length(which(dat1$tot == 2))


# Quick check
  # the number of rows removed + those kept should == the total number of rows in the Globi data
remove.rows + length(which(dat1$tot == 2)) == nrow(dat1)


# Remove rows with < 2 in the tot column
dat2 <- dat1[which(dat1$tot == 2),]

# Drop unused levels
dat2 <- droplevels(dat2)

# Save the rows with a complete bee-plant match
# write.csv(dat2, "./Data/matched_rows_2022_02_01.csv")


# look at citations
# View(table(dat2$sourceCitation))

# Number of observations
  #  7,884
nrow(dat2)







# 7. Visualize the data - Make a map with the data point -------------------------------------------------------




# Look at the first few rows
head(dat2)


# Determine which rows do not have lat long data
dat2.5 <- dat2[is.na(dat2$decimalLatitude) == TRUE & is.na(dat2$decimalLongitude) == TRUE,]



# Remove rows with NA in Lat long
dat3 <- dat2[is.na(dat2$decimalLatitude) == FALSE & is.na(dat2$decimalLongitude) == FALSE,]

# Number of rows
  # 7,229
nrow(dat3)

# Add a column with 1's
dat3$Prez <- 1

# Summarize the data by lat/long
dat4 <- ddply(.data = dat3, 
      .variable = c("decimalLongitude", "decimalLatitude"), 
      .fun = summarize,
      total = sum(Prez))


# Coordinates
longlats <- SpatialPoints(dat3[, c("decimalLongitude", "decimalLatitude")], 
                         proj4string=CRS("+proj=longlat +datum=WGS84")) 

names(longlats) <- NULL
min.vals <- apply(as.data.frame(longlats), 2, min)
max.vals <- apply(as.data.frame(longlats), 2, max)

bbox <- c(left = min.vals[1] - 0.02,
          bottom = min.vals[2]- 0.02, 
          right = max.vals[1] + 0.02, 
          top = max.vals[2] + 0.02)

names(bbox) <- c("left", "bottom", "right", "top")

#g.map <- ggmap(get_stamenmap(bbox, zoom = 3, maptype = "terrain"))+
#  geom_point(data = dat4, aes(y = decimalLatitude, x = decimalLongitude, #size = log10(total)))+
#  ylab("Latitude")+
#  xlab("Longitude")+
#  theme_bw()+ 
#  theme(axis.text.x = element_text(size = 17, color = "black"), 
#        axis.text.y = element_text(size = 17, color = "black"), 
#        axis.title.y = element_text(size = 17, color = "black"), 
#        axis.title.x =element_text(size = 17, color = "black"),
#        legend.title =element_text(size = 17, color = "black"),
#        legend.text =element_text(size = 17, color = "black"),
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank())
#
#g.map

#ggsave("./Figures/Globi_map_2022_01_14.pdf", height = 12, width = 15)




# Subset the data from SLO to SD
  # Lat from 31 - 36
  # Long from -125 to -116

dat5 <- dat4[dat4$decimalLatitude > 30 & dat4$decimalLatitude < 36 &
           dat4$decimalLongitude > -150 & dat4$decimalLongitude < -116, ]

sum(dat5$total)
  # 566


# Coordinates
longlats2 <- SpatialPoints(dat5[, c("decimalLongitude", "decimalLatitude")], 
                          proj4string=CRS("+proj=longlat +datum=WGS84")) 

names(longlats2) <- NULL
min.vals2 <- apply(as.data.frame(longlats2), 2, min)
max.vals2 <- apply(as.data.frame(longlats2), 2, max)

bbox2 <- c(left = min.vals2[1] - 0.02,
          bottom = min.vals2[2]- 0.02, 
          right = max.vals2[1] + 0.02, 
          top = max.vals2[2] + 0.02)

names(bbox2) <- c("left", "bottom", "right", "top")

g.map2 <- ggmap(get_stamenmap(bbox2, zoom = 8, maptype = "terrain"))+
  geom_point(data = dat5, aes(y = decimalLatitude, 
                              x = decimalLongitude, 
                              size = total),
             fill = "darkgoldenrod3",
             pch = 21)+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  guides(size=guide_legend(title="Total \nsample \nsize"))

g.map2

ggsave("Figures/2022_05_12/Globi_CA_map_2022_05_12.pdf", 
       height = 10, width = 12)



# Now subset the actual checklist database to the lat/long indicated here
dat6 <- dat3[dat3$decimalLatitude > 30 & dat3$decimalLatitude < 36 &
             dat3$decimalLongitude > -150 & dat3$decimalLongitude < -116, ]

# Number of observations
  # 566
nrow(dat6)






# 8. Add a new column to globi data with the institution codes to replace SCAN citation -------------------------------------------------------






# We will be working with the following 2 objects in this section:
  # dat6 = the subsetted species & CA dataset
  # institution.codes = the name of the institutions

# To look at the data frames, use these functions:
# View(institution.codes)
# View(dat6)


# The institution codes can be found in this column of the Globi database:
  # sourceInstitutionCode
# But we also need to look through 1 other columns:
  # sourceCatalogNumber


# Pull apart the sourceCatalogNumber column into 2 parts:
  # code
  # num
dat6 <- dat6 %>% 
  mutate(., sourceCatalogNumber = gsub(" ","", sourceCatalogNumber)) %>% 
  mutate(., sourceCatalogNumber = gsub("_","", sourceCatalogNumber)) %>%
  mutate(., sourceCatalogNumber = gsub("-","", sourceCatalogNumber)) %>% 
  mutate(., sourceCatalogNumber = gsub(":","", sourceCatalogNumber)) %>% 
  separate(sourceCatalogNumber, 
           into = c("code", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )

# We get a warning for 5 rows: 487, 527, 530, 532, 533
dat6[c(487, 527, 530, 532, 533),]
  # These rows have a code 
dat6[c(487, 527, 530, 532, 533),]$code
  # But no num
dat6[c(487, 527, 530, 532, 533),]$num


# Now we have a code column from dat6 we should use to match 

# The column sourceInstitutionCode does not need to be modified


# institution.codes object
  # 3 columns
  # fullcode
  # firstcode
  # secondcode

# Next we will match 2 columns globi dataset (code, sourceInstitutionCode) with the 3 columns in the institution.codes object


# Make a new column for the resolvedInstitutionName
dat6$resolvedSource <- NA



# Use a loop to go through each row of dat6 and try to match with any of the 3 columns in institution.codes object
for(i in 1:nrow(dat6)){
  
  # Using sourceInstitutionCode, look to see if it matches any of the first 3 columns in the institution.codes object
  first.col <- which(dat6$sourceInstitutionCode[i] == institution.codes$fullcode)
  sec.col <- which(dat6$sourceInstitutionCode[i] == institution.codes$firstcode)
  third.col <- which(dat6$sourceInstitutionCode[i] == institution.codes$secondcode)
  
  
  if(length(first.col) > 0){
    
    # 2. Add in a resolvedSource
    dat6$resolvedSource[i] <- as.character(institution.codes$institution[first.col])
    
  } else if (length(sec.col)  > 0){
    dat6$resolvedSource[i] <- as.character(institution.codes$institution[sec.col])
    
  }else if (length(third.col)  > 0){
    dat6$resolvedSource[i] <- as.character(institution.codes$institution[third.col])
    
  } else{
    dat6$resolvedSource[i] <- NA
    }
  
  
}

# Looks good!
 #View(dat6[,c("resolvedSource", "code", "sourceInstitutionCode", "sourceCitation")])


# Look at the levels of the sourceCitation
dat6 <- droplevels(dat6)

dat6$sourceCitation <- as.factor(dat6$sourceCitation)

levels(dat6$sourceCitation)

# Now we need to merge the resolvedSource column (which replaces the SCAN names) and the sourceCitation column
for(i in 1:nrow(dat6)){
  
  # If the resolvedSource == NA, then do this:
  if(is.na(dat6$resolvedSource[i]) == TRUE){
    
    # Copy over the sourceCitation
    dat6$resolvedSource[i] <- as.character(dat6$sourceCitation[i])
    
  }
  
}


# Write the file with the final globi dataset
#write.csv(dat6, "./Data/final-globi-list-clean 2022 02 01.csv")

# Look at the numnber of unique citations
citations <- levels(as.factor(dat6$resolvedSource))


# Write the file with the final list of source names
# write.csv(citations, "./Data/final-globi-citations-unique 2022 02 01.csv")





# 9. Format the date column in globi data -------------------------------------------------------






# Format the date of observation
# eventDateUnixEpoch
dat6$eventDate <- as.POSIXct(dat6$eventDateUnixEpoch/1000, origin = "1970-01-01")


# Pull apart the info in the event date
  # Year
  # Month
  # Day - this column also includes time - but I won't finish formatting this because we don't need this detail
dat7 <- as_tibble(dat6) %>%
  mutate(year = str_split(eventDate, "-", n = 3, simplify = TRUE)[,1],
         month= str_split(eventDate, "-", n = 3, simplify = TRUE)[,2],
         day = str_split(eventDate, "-", n = 3, simplify = TRUE)[,3])


# Drop unused levels
 dat7 <- droplevels(dat7)


# Remove rows with no month info
dat7 <- dat7[dat7$month != "",]

# Save month as numeric
dat7$month <- as.numeric(dat7$month)
dat7 <- dat7[is.na(dat7$month) == FALSE,]


# Remove repeat observations
dat7 <- dat7 %>%
          distinct()


# remove any NA rows in dat7
dat7 <- dat7[is.na(dat7$resolvedSource) == FALSE,]






# 10. Create the 4-D array we will fill in -------------------------------------------------------





# The data will be a 4-D array
  # Dim 1 = bee species
  # Dim 2 = plant species
  # Dim 3 = month
  # Dim 4 = citation

# Create the array that will be filled in
bee.plant.date.cite <- array(NA, dim = c(length(bee.species), # Number of bee species
                                        length(plant.species), # Number of plant species
                                        12, # 12 months
                                        length(citations))) # Number of unique references


# Add row names
rownames(bee.plant.date.cite) <- bee.species

# Add column names
colnames(bee.plant.date.cite) <- plant.species

# Add sheet names - month
dimnames(bee.plant.date.cite)[[3]] <- c(1:12)

# Add 4th dimension names
dimnames(bee.plant.date.cite)[[4]] <- citations


# Total number of interactions by citations
  # 6,631,968
length(bee.species) *
  length(plant.species)* 
  12*
  length(citations)







# 11. Determine which bee-plant interactions are possible & which are forbidden -------------------------------------------------------





####################
# Determine if you are running with or without apis
####################



# -------------- WITH Apis


# Use this file WITH Apis
  # load("./Data/bee_plant_inter_2022_04_05 - short plant.rds")






# -------------- WITHOUT Apis



# Use this file WITHOUT Apis
  load("Data/bee_plant_inter_2022_04_11 - short plant - no apis.rds")





####################
####################
####################




# # Look at the number of bee species 
# max(bee.plant.inter$beeID)
# 
# # Look at the number of plant species
# max(bee.plant.inter$plantID)
# 
# 
# 
# 
# # Next, we will be working on a long dataframe and specify which bee-plant interactions are:
#   # possible
#   # AND forbidden
# #
# # Create empty dataframe for possible bee-plant interactions
# bee.plant.inter <- data.frame(beeID = NA,
#                               plantID = NA,
#                               monthID = NA)
# 
# # Create empty dataframe for FORBIDDEN bee-plant interactions
# bee.plant.forbid <- data.frame(beeID = NA,
#                                plantID = NA,
#                                monthID = NA)
# 
# # These objects will serve as counters, it will make sense in the loop
# a <- 1
# b <- 1
# 
# # Start the loop
#   # This takes 1.7 hrs to run
# start.time <- Sys.time()
# 
# for(i in 1:nrow(bee.plant.date.cite)){ # For each bee species
#   for(j in 1:ncol(bee.plant.date.cite)){ # For each plant species
#     for(k in 1:dim(bee.plant.date.cite)[3]){ # for each month
#       
#       # There is a row in the plant.phenology table that is all NA - we need to kip it
#      # if(is.na(plant.phenology[j, k + 26]) == FALSE){
#         
#         
#         # Possible interactions
#         if(# If the bee is active (bee.phenology == 1)
#           
#           # Start with January (column == 7)
#           bee.list[i, k+6] == 1 &
#           
#           # AND if the plant is available (plant.phenology == 1)
#           plant.phenology[j, k + 26] == 1){
#           
#           # Then that bee plant interaction can occur and write it in the file: ee.plant#.inter
#           bee.plant.inter[a,1] <- i
#           bee.plant.inter[a,2] <- j
#           bee.plant.inter[a,3] <- k
#           
#           # Add 1 to the counter
#           a <- a + 1
#         }
#         
#         # Forbidden links
#       #  if(# If the bee is NOT active
#       #    
#       #    # Make sure this is the right column############
#       #    bee.list[i, k+6] == 0 |
#       #    
#       #    # OR if the plant is not available
#       #    plant.phenology[j, k + 26] == 0){
#       #    
#       #    # This is a forbidden link, and write it here
#       #    bee.plant.forbid[b,1] <- i
#       #    bee.plant.forbid[b,2] <- j
#       #    bee.plant.forbid[b,3] <- k
#       #    
#       #    # Add 1 to the counter
#       #    b <- b + 1
#       #  }
#         
#       }
#       
#     }
#     
#   }
# 
# end.time <- Sys.time()
# beepr::beep(3)
# 
# # How long did the loop take?
# end.time - start.time
# 
# # How many possible interactions are there?
#   # 216,385
# nrow(bee.plant.inter)
# 
# 
# 
# # Save the bee.plant.inter file
#  # This file takes a while to generate - so in the future - we can just import it into this space
#  save(bee.plant.inter, file= "./Data/bee_plant_inter_2022_04_11 - short plant - no apis.rds")
# 
# 
#






# 12. Fill in the 4-D array for the model -------------------------------------------------------




# Fill in the 4-D array with the data



# First- we fill in the possible interactions with a 0 - then, we will go back and fill them in with a 1 if they were detected
# To do this, we need to determine which months each citation went out to look for the bee-plant interaction

# For each citation - loop through them
start.time <- Sys.time()
for(j in 1:length(citations)){
  
  # Subset the data - sometimes this generates NA rows
  dat.sub <- dat7[dat7$resolvedSource == citations[j],]
  
  # Remove NA months
  dat.sub <- dat.sub[is.na(dat.sub$month) == FALSE,]
  
  # Determine what months they were out looking
  citation.months <- as.numeric(unique(dat.sub$month))
  
  # If the source citation isn't going out on a particular month - we DO NOT want to populate it was a 0
    # We only use a 0 if the source citation is going out
  bee.plant.inter.sub <- bee.plant.inter[bee.plant.inter$monthID %in% citation.months,]
  
  # Now fill in the 4-D array with 0's for the month that the source citation went out
  for(i in 1:nrow(bee.plant.inter.sub)){
    
    bee.plant.date.cite[bee.plant.inter.sub$beeID[i],  # bee species
                        bee.plant.inter.sub$plantID[i],  # plant species
                        bee.plant.inter.sub$monthID[i],  # Month
                        j]     <- 0
    
  }

    
}

end.time <- Sys.time()
beepr::beep(3)

# How long did the loop take?
end.time - start.time


# Given the possible bee-plant interactions and the month that each citation went to the field, how many possible observations are there?
  # 611,237
length(which(bee.plant.date.cite == 0))




# Now we will fill in the 4-D array with the presence data
start.time <- Sys.time()
for(i in 1:nrow(dat7)){
  
  # Determine which citation
  cit.pos <- which(citations %in% dat7$resolvedSource[i] == TRUE)
  
  # Determine which bee
  bee.pos <- which(bee.species %in% dat7$resolvedBeeNames[i]  == TRUE)
  
  # Determine which month
  month.pos <- as.numeric(dat7$month[i])
  
  # Determine which plant
  plant.pos <- which(plant.species %in% dat7$resolvedPlantNames[i]  == TRUE)
  
  print(paste("Working on row ", i, "; cit = ", cit.pos, "; bee = ", bee.pos, "; plant = ", plant.pos, "; month = ", month.pos, "\r.."))
  
  # If we have a value for each of the positions, then add a 1
# if(is.na(cit.pos) == FALSE &
#    is.na(bee.pos) == FALSE &
#    is.na(month.pos) == FALSE &
#    is.na(plant.pos) == FALSE) {
    
    # Add a 1
    bee.plant.date.cite[bee.pos, 
                        plant.pos, 
                        month.pos, 
                        cit.pos] <- 1
    
 # }
  
  
}

end.time <- Sys.time()
beepr::beep(3)

# How long did the loop take?
end.time - start.time

# How many detections are in the dataframe?
  # 278
  # Note that in some cases the same citation documents the same bee and plant interactions during the same month
length(which(bee.plant.date.cite == 1))







# 13. Create a matrix with the observed bee-plant-month by citation elements -------------------------------------------------------





# Next, we will be working on a long dataframe and specify which bee-plant interactions were observed


## Create empty dataframe for possible bee-plant interactions
bee.plant.obs <- data.frame(beeID = NA,
                            plantID = NA,
                            monthID = NA,
                            sourceID = NA)


# We start by identifying the unique combinations of source citation by month
source.month <- data.frame(monthID = NA,
                           sourceID = NA)

# pull out the source and month
for(i in 1:nrow(dat7)){ 
  
  # Determine which citation
  cit.pos <- which(citations %in% dat7$resolvedSource[i] == TRUE)
  
  # Determine which month
  month.pos <- as.numeric(dat7$month[i])

  source.month[i, 1] <- month.pos
  source.month[i, 2] <- cit.pos
  
}

# Number of rows
nrow(source.month)

# Identify the unique month-source combinations
source.month <- source.month %>%
  distinct()

# Number of unique combinations
nrow(source.month)


## Start the loop
start.time <- Sys.time()
for(i in 1:nrow(source.month)){ # For each row in source.month
      
      # Determine which citation
      cit.pos <- source.month$sourceID[i]
      
      # Determine which month
      month.pos <-source.month$monthID[i]
      
      # Subset the bee-plant interactions dataframe to the specific month & add a new column with the source ID
      bee.plant.inter.sub <- data.frame(
                                  bee.plant.inter[bee.plant.inter$monthID == month.pos,1:3],
                                  sourceID = cit.pos)
      
      # Row bind the subsetted data to the previous dataframe
      bee.plant.obs <- rbind(bee.plant.obs, bee.plant.inter.sub)
      
      print(i)
  }

end.time <- Sys.time()
#beepr::beep(3)

# How long did the loop take?
end.time - start.time

# Number of rows
nrow(bee.plant.obs)

# Remove repeat observations
bee.plant.obs <- bee.plant.obs %>%
  distinct()

# remove the NA row
bee.plant.obs <- bee.plant.obs[-1,]

# How many observations are there?
nrow(bee.plant.obs)








# Save the bee.plant.obs file
  # This file takes a while to generate - so in the future - we can just import it into this space
#save(bee.plant.obs, file= "./Data/bee_plant_obs_2022_04_05 - short plant list - no apis.rds")






# 14. Compare the interaction matrix and the observation matrix -------------------------------------------------------



# Observation matrix:
  # bee.plant.obs

# Interaction matrix
  # bee.plant.inter



 head(bee.plant.inter)
 
 head(bee.plant.obs)
 
 
 # Add a new column
 bee.plant.obs$y <- NA
 
 for(i in 1:nrow(bee.plant.obs)){
   
   bee.plant.obs$y[i] <-  bee.plant.date.cite[bee.plant.obs$beeID[i],
                                              bee.plant.obs$plantID[i],
                                              bee.plant.obs$monthID[i],
                                              bee.plant.obs$sourceID[i]]
   
 }
 
 
 
 
 
# Save the file
write.csv(bee.plant.obs, file = "./Data/bee-plant-obs-long-format 2022 04 11 - short plant list - no apis.csv")













# Are any of the rows have NA?
length(which(is.na(bee.plant.obs$y) == TRUE))

# Look at which ones have an non-detection
bee.plant.obs %>%
  filter(bee.plant.obs$y == 0)

nrow(bee.plant.obs %>%
       filter(bee.plant.obs$y == 0))

# Look at which ones have a detection
bee.plant.obs %>%
  filter(bee.plant.obs$y == 1)

nrow(bee.plant.obs %>%
       filter(bee.plant.obs$y == 1))






# Are there any combinations from the observations (bee.plant.date.cite) that aren't in the possible interactions (bee.plant.inter)?
# To do this, first we start by collapsing observations across 4th dimension (souce citations)
y.obs <- apply(bee.plant.date.cite, c(1, 2, 3), max, na.rm = TRUE) 
y.obs[y.obs == "-Inf"] <- NA

# # Create a new dataframe where we will store the observations in long format # (similar to the possible interactions)
 y.obs.long <- data.frame(beeID = NA,
                          plantID = NA,
                          monthID = NA,
                          obs = NA)
 
 # Start a counter
 a <- 1
 
 
 # Using a for loop - we will loop through each dimension of the y.obs array
   # If there is no NA, we will create a row with the observation in the y.obs.long # ataframe
   # This loop takes ~42 min
 start.time <- Sys.time()
 
 for(i in 1:dim(y.obs)[1]){
   for(j in 1:dim(y.obs)[2]){
     for(k in 1:dim(y.obs)[3]){
       
       if(is.na(y.obs[i,j,k]) == FALSE){
         
         y.obs.long[a, 1] <- i
         y.obs.long[a, 2] <- j
         y.obs.long[a, 3] <- k
         y.obs.long[a, 4] <- y.obs[i,j,k]
         
         a <- a + 1
       } 
       
     }
   }
 }
 
 end.time <- Sys.time()
 beepr::beep(3)
 
 # How long did the loop take?
 end.time - start.time
 
 
# # Save the output
# write.csv(y.obs.long, file = "./Data/observations-documented-but-not-possible-2022 02 18.csv")


#y.obs.long <- read.csv("./Data/observations-documented-but-not-possible-2022 02 18.csv")


 
 
 
 
 
 
 
 

# Add a column to the possible interactions 
bee.plant.inter$inter <- 1

# Merge the new long observations dataframe (y.obs.long) with the possible interactions data frame (bee.plant.inter)
y2 <- merge(y.obs.long, 
            bee.plant.inter, 
            all.x = TRUE) # keep all rows in the new long observations dataframe (y.obs.long) 

# Identify which bee-plant-month observations were detected BUT are not possible
observed.but.not.possible <- y2[which(is.na(y2$inter) == TRUE),]
nrow(observed.but.not.possible)






# Look at the first row
observed.but.not.possible[2,]

# Look at the possible bee-plant by month interactions
bee.plant.inter[bee.plant.inter$beeID == observed.but.not.possible[2,]$beeID & 
                bee.plant.inter$plantID == observed.but.not.possible[2,]$plantID , ]
  # Note that the monthID is not listed in the possible times they interact


# View(bee.plant.inter[bee.plant.inter$beeID == observed.but.not.possible[1,]$beeID,])

# Pull out the information from Globi that corresponds to this information
# Add a column with the bee name and the plant name

# Add empty columns to fill in
observed.but.not.possible$beeName <- NA
observed.but.not.possible$plantName <- NA

# Loop through each row of the dataframe
for(i in 1:nrow(observed.but.not.possible)){
  # Pull out the bee name
  observed.but.not.possible$beeName[i] <- 
            bee.species[observed.but.not.possible$beeID[i]]
  
  # Pull out the plant name
  observed.but.not.possible$plantName[i] <- 
            plant.species[observed.but.not.possible$plantID[i]]
}

# Now, we have to determine which row is associated with each interaction in the Globi database
obs.not.possible.globi <- data.frame()

for(i in 1:nrow(observed.but.not.possible)){

  # Determine which rows match with the criteria
  globi.row <- which( dat7$resolvedBeeNames == observed.but.not.possible$beeName[i]  &
                      dat7$resolvedPlantNames == observed.but.not.possible$plantName[i] & 
                      dat7$month == observed.but.not.possible$monthID[i] )
  
  glob.sub <- dat7[globi.row,]
  
  # Then add the info to the new dataframe
  obs.not.possible.globi <- rbind(obs.not.possible.globi, glob.sub)
  
  print(i)
  
}


# Number of rows:
nrow(obs.not.possible.globi)


obs.not.possible.globi$resolvedBeeNames


# Append the phenology information

# Add columns for bee phenology
phen <- data.frame(Bee.Jan = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Feb = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Mar = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Apr = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.May = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Jun = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Jul = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Aug = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Sep = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Oct = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Nov = rep(NA, times = nrow(obs.not.possible.globi)),
                      Bee.Dec = rep(NA, times = nrow(obs.not.possible.globi)),
                      
                      Plant.Jan = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Feb = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Mar = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Apr = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.May = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Jun = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Jul = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Aug = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Sep = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Oct = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Nov = rep(NA, times = nrow(obs.not.possible.globi)),
                      Plant.Dec = rep(NA, times = nrow(obs.not.possible.globi)))

obs.not.possible.globi$resolvedBeeNames

obs.not.possible.globi <- data.frame(obs.not.possible.globi, phen)

obs.not.possible.globi$resolvedBeeNames

# Pull out the column number that identifies the bee phenology
bee.phen.cols <- grep("Bee.", colnames(obs.not.possible.globi))
# Only take out months
bee.phen.cols <- bee.phen.cols[2:13]
colnames(obs.not.possible.globi)[bee.phen.cols]

# Pull out the column number that identifies the plant phenology
plant.phen.cols <- grep("Plant.", colnames(obs.not.possible.globi))
# Only take out months
plant.phen.cols <- plant.phen.cols[2:13]
colnames(obs.not.possible.globi)[plant.phen.cols]


# Loop through each row
for(i in 1:nrow(obs.not.possible.globi)){
  
  # Determine which row matches between obs.not.possible.globi and bee.list
  bee.list.row <- which(obs.not.possible.globi$resolvedBeeNames[i] == bee.list$scientificName)

  # Determine which row matches between obs.not.possible.globi and plant.phenology
  plant.list.row <- which(obs.not.possible.globi$resolvedPlantNames[i] == plant.phenology$scientificName)
  
    
  # Add the bee phenology info 
  obs.not.possible.globi[i, bee.phen.cols] <-  bee.list[bee.list.row, 7:(7+11)]
  
  # Add the plant phenology info
  obs.not.possible.globi[i, plant.phen.cols] <-  plant.phenology[plant.list.row, 27:(27+11)]

  print(obs.not.possible.globi$resolvedBeeNames[i])
  
}


obs.not.possible.globi[1,]

obs.not.possible.globi$resolvedBeeNames

# ## # Write the file
# write.csv(obs.not.possible.globi,
#           file = "./Data/globi-obs-not-possible 2022 04 11 - no apis.csv")
#




 
 
 
 # I will look at these entries & decide how to proceed


 
 
 


# To proceed, pick 1 of 2 paths:
  # Option 1: Add in the new month information I made up
    # For this option - run code on lines X - X

  # Option 2: Replace all mismatched data with an NA
    # For this option - run code on lines X - X



# In the meantime, to get the model to work, I will assign all of these species non-detections in the observation array


############
# Option 1
############




############

#----------  WITH APIS

# Here is a new file with a new.month column
new.month <- read.csv("./Data/globi-obs-not-possible 2022 04 05 - new month.csv")


#---------- WITHOUT Apis


# Here is a new file with a new.month column
new.month <- read.csv("./Data/globi-obs-not-possible 2022 04 11 - no apis - new month.csv")



############

# First, replace the incorrect presence data with an NA in the bee.plant.date.cite array
for(i in 1:nrow(observed.but.not.possible)){
  
  # Determine which citation made the mistake
  citID <- which(bee.plant.date.cite[observed.but.not.possible$beeID[i], 
                                     observed.but.not.possible$plantID[i], 
                                     observed.but.not.possible$monthID[i], 
  ] == 1)
  
  # Replace the 1 with an NA
  bee.plant.date.cite[observed.but.not.possible$beeID[i], 
                      observed.but.not.possible$plantID[i], 
                      observed.but.not.possible$monthID[i], 
                      citID] <- NA
  
  
}


# Next, check to make sure that everything matches between the observations and the interactions that are possible
y.obs <- apply(bee.plant.date.cite, c(1, 2, 3), max, na.rm = TRUE) 
y.obs[y.obs == "-Inf"] <- NA

# Create a new dataframe where we will store the observations in long format # (similar to the possible interactions)
y.obs.long <- data.frame(beeID = NA,
                         plantID = NA,
                         monthID = NA,
                         obs = NA)

# Start a counter
a <- 1


# Using a for loop - we will loop through each dimension of the y.obs array
# If there is no NA, we will create a row with the observation in the y.obs.long # ataframe
# This loop takes ~42 min
start.time <- Sys.time()

for(i in 1:dim(y.obs)[1]){
  for(j in 1:dim(y.obs)[2]){
    for(k in 1:dim(y.obs)[3]){
      
      if(is.na(y.obs[i,j,k]) == FALSE){
        
        y.obs.long[a, 1] <- i
        y.obs.long[a, 2] <- j
        y.obs.long[a, 3] <- k
        y.obs.long[a, 4] <- y.obs[i,j,k]
        
        a <- a + 1
      } 
      
    }
  }
}

end.time <- Sys.time()
beepr::beep(3)

# How long did the loop take?
end.time - start.time

# Merge the new long observations dataframe (y.obs.long) with the possible interactions data frame (bee.plant.inter)
y2 <- merge(y.obs.long, 
            bee.plant.inter, 
            all.x = TRUE) # keep all rows in the new long observations dataframe (y.obs.long) 

# Identify which bee-plant-month observations were detected BUT are not possible
observed.but.not.possible <- y2[which(is.na(y2$inter) == TRUE),]

# Identify the number of rows that are observed but not possible
nrow(observed.but.not.possible)
  # Great! Now there are no observations that are NOT possible







# Add in a beeID and plantID column to the data frame: new.month

new.month$BeeID <- NA
new.month$PlantID <- NA
new.month$SourceID <- NA

for(i in 1:nrow(new.month)){
  
  beeID <- which(bee.species == new.month$resolvedBeeNames[i])

  plantID <- which(plant.species == new.month$resolvedPlantNames[i])
  
  citID <- which(citations == new.month$resolvedSource[i])
  
  new.month$BeeID[i] <- beeID
  new.month$PlantID[i] <- plantID
  new.month$SourceID[i] <- citID
  
}


# Now we will fill in the bee.plant.date.cite with 
for(i in 1:nrow(new.month)){
  
  # Replace the 0 with a 1
  bee.plant.date.cite[new.month$BeeID[i], 
                      new.month$PlantID[i], 
                      new.month$new.month[i], 
                      new.month$SourceID[i]] <- 1
  
  
}



# Determine if any of the observations are NOT possible
# Next, check to make sure that everything matches between the observations and the interactions that are possible
y.obs <- apply(bee.plant.date.cite, c(1, 2, 3), max, na.rm = TRUE) 
y.obs[y.obs == "-Inf"] <- NA

# Create a new dataframe where we will store the observations in long format # (similar to the possible interactions)
y.obs.long <- data.frame(beeID = NA,
                         plantID = NA,
                         monthID = NA,
                         obs = NA)

# Start a counter
a <- 1


# Using a for loop - we will loop through each dimension of the y.obs array
# If there is no NA, we will create a row with the observation in the y.obs.long # ataframe
# This loop takes ~42 min
start.time <- Sys.time()

for(i in 1:dim(y.obs)[1]){
  for(j in 1:dim(y.obs)[2]){
    for(k in 1:dim(y.obs)[3]){
      
      if(is.na(y.obs[i,j,k]) == FALSE){
        
        y.obs.long[a, 1] <- i
        y.obs.long[a, 2] <- j
        y.obs.long[a, 3] <- k
        y.obs.long[a, 4] <- y.obs[i,j,k]
        
        a <- a + 1
      } 
      
    }
  }
}

end.time <- Sys.time()
beepr::beep(3)

# How long did the loop take?
end.time - start.time

# Merge the new long observations dataframe (y.obs.long) with the possible interactions data frame (bee.plant.inter)
y2 <- merge(y.obs.long, 
            bee.plant.inter, 
            all.x = TRUE) # keep all rows in the new long observations dataframe (y.obs.long) 

# Identify which bee-plant-month observations were detected BUT are not possible
observed.but.not.possible <- y2[which(is.na(y2$inter) == TRUE),]

# Identify the number of rows that are observed but not possible
nrow(observed.but.not.possible)
  # Now there are 0 observations that are not possible (instead of 44)!!!

#      beeID plantID monthID obs inter
# 34297    81     115       8   1    NA


#--------- End option 1



















############
# Option 2
############

# Now we will fill in the 4-D array with the presence data
start.time <- Sys.time()
for(i in 1:nrow(observed.but.not.possible)){
  
  # Determine which citation made the mistake
  citID <- which(bee.plant.date.cite[observed.but.not.possible$beeID[i], 
                      observed.but.not.possible$plantID[i], 
                      observed.but.not.possible$monthID[i], 
                       ] == 1)
  
  # Replace the 1 with an NA
bee.plant.date.cite[observed.but.not.possible$beeID[i], 
                    observed.but.not.possible$plantID[i], 
                    observed.but.not.possible$monthID[i], 
                    citID] <- NA
  
  
}

end.time <- Sys.time()
# beepr::beep(3)

# How long did the loop take?
end.time - start.time

#--------- End option 2





# 15. Save the data -------------------------------------------------------


# Save the 4-D array
save(bee.plant.date.cite, 
     file= "./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_11 - short plant list - no apis.rds")


## Save the subsetted Globi data
#save(dat7, 
#     file= "./Data/data_summary/FINAL - subsetted_globi_data_2022 02 18.csv" )



# End script

