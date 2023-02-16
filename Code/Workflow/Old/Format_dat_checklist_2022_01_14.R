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
  # Subset the data
    # Lat from 31 - 32
    # Long from -125 to -116
  

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
# 3. Determine number of unique bee sp. and studies
# 4. Summarize the total number of parasites on each bee per study
# 5. Save the data



################################## 
################################## 
################################## 




# 1. Load libraries & set working directory -------------------------------------------------------



# Load libraries
library(tidyverse)
library(rglobi)
library(bipartite)
library(igraph)
library(ggplot2)
library(doParallel)
library(ggmap)
library(dplyr)
library(plyr)
library(rgdal)
library(ggsn)
library(sf)
library(reshape2)

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
# dat <- read.csv("~/Desktop/Folder/Data_globi/all_bee_data_unique-2021.csv")
dat <- read.csv("~/Desktop/Folder/Data_globi/resolvedplantnamesglobi_011722.csv")


# Read in bee phenology data
bee.phenology <- read_tsv("~/Dropbox/Globi/Data/All-California-phenologyTable.tsv")

# Read in the bee checklist
bee.list <- read.table("./Data/SCI-list-04-2021.txt", header = T, sep = "\t")

# Read in plant phenology data
  # resolvedplantsci_011722.csv = Final santa cruz island checklist - has phenology & resolved specie snames
plant.phenology <- read.csv("~/Desktop/Folder/Data_globi/resolvedplantsci_011722.csv")

# Read in file with type of citation
citation <- read.csv("./Data/citation-list-type.csv")


# Other bee names = Synonyms
  # Zenodo synoynm list
  # https://zenodo.org/record/5738043#.YddffBPML0o
bee.names <- read.csv("~/Desktop/Folder/Data_globi/discoverlife-Anthophila.csv")

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
bee.names.globi <- read.csv("./Data/Globi-names-not-in-discoverlife - Sheet1.csv")


# Institution codes
institution.codes <- read.csv("./Data/institutioncodes_2021_12_16.csv")




# Look at data structure
str(dat)
  # Look at globi for column definitions

str(bee.list)
  # $ genus : Bee genus
  # $ subgenera : Bee subgenera
  # $ specificEpithet: Bee species name
  # $ infraSpecificEpithet: Bee subspecies name








# 3. Format checklist info a little -------------------------------------------------------





# Make a bee genus species column with infraSpecificEpithet
bee.list$genus_species <- ifelse(bee.list$specificEpithet == "",
                                 bee.list$genus,
                                 paste(bee.list$genus, bee.list$specificEpithet))

bee.list$genus_species_infra <- ifelse(is.na(bee.list$infraSpecificEpithet), 
                                 paste(bee.list$genus, 
                                       bee.list$specificEpithet),
                                 paste(bee.list$genus, 
                                       bee.list$specificEpithet, 
                                       bee.list$infraSpecificEpithet))

# The plant list already has the scientific names together:
  # plant.phenology$scientificName


# Remove species without a species name ( == "sp.")
bee.list <- bee.list[bee.list$specificEpithet != "sp.",]


# Final number of bee species
nrow(bee.list)





# 4. Metrics from Globi data -------------------------------------------------------






# The downloaded Globi dataset has:
  # 300,465 rows
nrow(dat)



# Remove rows without a bee species name and without plant species name
dat1 <- dat[dat$sourceTaxonSpeciesName != "" & dat$targetTaxonSpeciesName != "", ]
dat1 <- dat1[is.na(dat1$sourceTaxonSpeciesName) == FALSE & 
              is.na(dat1$resolvedPlantNames) == FALSE, ]



# Create a list with all of the unique species in Globi - 
# All of the insect related info in the source column
# All of the plant related info in the target column

# Plant & Bee species
globi.sp <- unique(c(as.character(unique(dat1$sourceTaxonSpeciesName)), 
                     as.character(unique(dat1$targetTaxonSpeciesName))))

# Number of unique bee and plant species (not discriminated)
  # 7,407
length(globi.sp)


# Number of unique observations
  # 157,893
nrow(dat1)




# 5. Format the bee synonym data -------------------------------------------------------





# Add
  # bee.names.globi


# Change the column name from Accepted.Name to resolvedBeeNames
colnames(bee.names.globi)[grep("Accepted.Name", colnames(bee.names.globi))] <- "resolvedBeeNames"


# Filter out
  # The no:match issue is a bit more confusing. In the providedName column the values no:match do not exist. It does exist in the resolvedName column. The reason it exists is that those names are homonyms, but I cannot remember exactly why no:match shows up for the resolved name in these cases. I think the solution is to remove all lines in the DiscoverLife name list that are homonyms, or has HOMONYM_OF in the relationName column.


# Look at the structure of the bee synonym list
str(bee.names)
  # resolvedName = latest name
  # providedName = list of all possible synoynms

str(bee.names.globi)
  # resolvedBeeNames = latest name
  # sourceTaxonSpeciesName = list of all possible synoynms


# Filter out homonyms
bee.names <- bee.names[bee.names$relationName != "HOMONYM_OF",]


# Total number of unique species in the synonym list: 
  # 20,461
length(levels(bee.names$resolvedName))

# Do all of the bee species in our checklist appear in the synoynm list?
bee.list$genus_species %in% levels(bee.names$providedName)


# Which species are not included?
bee.list$genus_species[which(bee.list$genus_species %in% levels(bee.names$providedName) == FALSE)]
 # None - all are included




# Make a new column for the resolvedBeeNames
dat1$resolvedBeeNames <- NA

# Use a loop to go through each genus_species and match with the globi species list
  # This function takes ~ 8 minutes to run

#i <- which(dat1$sourceTaxonSpeciesName == "Afranthidium junodi")[1]

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
  
  } else if (is.na(row.num.kat) == FALSE ) {
    dat1$resolvedBeeNames[i] <- as.character(bee.names.globi$resolvedBeeNames[row.num.kat])
    
  } else {
      dat1$resolvedBeeNames[i] <- NA}
  
}
end.time <- Sys.time()
beepr::beep(3)

# Determine amount of time to run for loop
end.time - start.time


# Pull out the rows in the GLobi dataset that were NOT matched with any species names in the synoynm list (from Zenodo)
not.matched.bee.rows <- dat1[is.na(dat1$resolvedBeeNames) == TRUE |
                               dat1$resolvedBeeNames == "no:match", ]
# Are there any no matches?
length(which(dat1$resolvedBeeNames == "no:match"))

# How many have NAs?
length(which(is.na(dat1$resolvedBeeNames) == TRUE))


# Number of rows that did not produce matches
  # 244
nrow(not.matched.bee.rows)

# Drop unused levels
not.matched.bee.rows <- droplevels(not.matched.bee.rows)

# The not.matched list boils down to 4 levels:  
    # Pseudopanugus sp. E1  
    # Megachile cincta      
    # Coelioxys octodentata 
    # Osmia capensis

# Select the columns you want to keep
unique.rows <- not.matched.bee.rows %>%
                  select(32:52) %>%
                  unique()

# Save output
write.csv(unique.rows, "./Data/no-resolved-bee-names-2022 01 21.csv")




# Now we have the Globi dataset with updated bee names






# Next we need to update our checklist with the resolvedNames

# Make a new column for the resolvedBeeNames
bee.list$resolvedBeeNames <- NA

# Use a loop to go through each genus_species and match with the globi species list
for(i in 1:nrow(bee.list)){
  
  # 1. Identify the row in the bee.names4 dataframe that the name matches
  # Zenodo list
  row.num <- which(bee.list$genus_species[i] == bee.names$providedName)[1]
  
  # The previous line needs to come back with a number to do the next command
  if(length(row.num) > 0){
    
    # 2. Replace the Globi name with the current.name
    bee.list$resolvedBeeNames[i] <- as.character(bee.names$resolvedName[row.num])
    
  } else {bee.list$resolvedBeeNames[i] <- NA}
  
}

# Pull out the rows in the bee.list dataset that were NOT matched with any species names in the synoynm list (from Zenodo)
  # All should have matches
bee.list[is.na(bee.list$resolvedBeeNames) == TRUE |
               bee.list$resolvedBeeNames == "no:match", ]





# Last, we need to update the bee names in the bee.phenology object

# Make a new column for the resolvedBeeNames
bee.phenology$resolvedBeeNames <- NA

# Use a loop to go through each scientificName and match with the bee.phenology species list
for(i in 1:nrow(bee.phenology)){
  
  # 1. Identify the row in the bee.names4 dataframe that the name matches
  # Zenodo list
  row.num <- which(bee.phenology$scientificName[i] == bee.names$providedName)[1]
  
  # The previous line needs to come back with a number to do the next command
  if(is.na(row.num)  == FALSE){
    
    # 2. Replace the Globi name with the current.name
    bee.phenology$resolvedBeeNames[i] <- as.character(bee.names$resolvedName[row.num])
    
  } else {bee.phenology$resolvedBeeNames[i] <- NA}
  
}

# Pull out the rows in the bee.phenology dataset that were NOT matched with any species names in the synoynm list (from Zenodo)
  # All should have matches
bee.phenology[is.na(bee.phenology$resolvedBeeNames) == TRUE |
                    bee.phenology$resolvedBeeNames == "no:match", ]



#View(bee.phenology)






# 4. Subset rows to only those with bee-plant interactions that appear in our checklist -------------------------------------------------------





# Unique bee species
  # 139 species
bee.species <- unique(bee.list$resolvedBeeNames)
bee.species <- bee.species[is.na(bee.species) == FALSE]
bee.species <- bee.species[bee.species != "no:match"]

length(bee.species)

# Unique plant species
  # 562 species
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
  # 150,016
remove.rows


# How many rows will be kept?
  # 7,877
length(which(dat1$tot == 2))


# Quick check
  # the number of rows removed + those kept should == the total number of rows in the Globi data
remove.rows + length(which(dat1$tot == 2)) == nrow(dat1)


# Remove rows with < 2 in the tot column
dat2 <- dat1[which(dat1$tot == 2),]

# Drop unused levels
dat2 <- droplevels(dat2)

# Save the rows with a complete bee-plant match
write.csv(dat2, "./Data/matched_rows_2022_01_21.csv")


# look at citations
# View(table(dat2$sourceCitation))

# Number of observations
  #  7,877
nrow(dat2)




# 4. Make a map with the data point -------------------------------------------------------




# Look at the first few rows
head(dat2)


# Determine which rows do not have lat long data
dat2.5 <- dat2[is.na(dat2$decimalLatitude) == TRUE & is.na(dat2$decimalLongitude) == TRUE,]



# Remove rows with NA in Lat long
dat3 <- dat2[is.na(dat2$decimalLatitude) == FALSE & is.na(dat2$decimalLongitude) == FALSE,]

# Number of rows
  # 7,354
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

g.map <- ggmap(get_stamenmap(bbox, zoom = 3, maptype = "terrain"))+
  geom_point(data = dat4, aes(y = decimalLatitude, x = decimalLongitude, size = log10(total)))+
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
        panel.grid.minor = element_blank())

g.map

#ggsave("./Figures/Globi_map_2022_01_14.pdf", height = 12, width = 15)




# Subset the data
  # Lat from 31 - 36
  # Long from -125 to -116

dat5 <- dat4[dat4$decimalLatitude > 30 & dat4$decimalLatitude < 36 &
           dat4$decimalLongitude > -150 & dat4$decimalLongitude < -116, ]

sum(dat5$total)
  # 583


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
                              size = log10(total)))+
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
        panel.grid.minor = element_blank())

g.map2

# ggsave("./Figures/Globi_CA_map_2022_01_14.pdf", height = 12, width = 15)



# Now subset the actual checklist database to the lat/long indicated here
dat6 <- dat3[dat3$decimalLatitude > 30 & dat3$decimalLatitude < 36 &
             dat3$decimalLongitude > -150 & dat3$decimalLongitude < -116, ]

# Number of observations
  # 583
nrow(dat6)







# 5. Add a new column with the institution codes to replace SCAN citation -------------------------------------------------------






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
 View(dat6[,c("resolvedSource", "code", "sourceInstitutionCode", "sourceCitation")])


# Look at the levels of the sourceCitation
 dat6 <- droplevels(dat6)
levels(dat6$sourceCitation)

for(i in 1:nrow(dat6)){
  
  # If the resolvedSource == NA, then do this:
  if(is.na(dat6$resolvedSource[i]) == TRUE){
    
    # Copy over the sourceCitation
    dat6$resolvedSource[i] <- as.character(dat6$sourceCitation[i])
    
  }
  
}


# Write the file with the final globi dataset
write.csv(dat6, "./Data/final-globi-list-clean 2022 01 21.csv")

# Look at the numnber of unique citations
citations <- levels(dat6$resolvedInstitutionName)


write.csv(citations, "./Data/final-globi-citations-unique 2022 01 21.csv")






# 6. Format the date column  -------------------------------------------------------






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







# 7. Determine which bee-plant interactions are possible & which are forbidden -------------------------------------------------------





# Remove the NA rows
bee.phenology <- bee.phenology[is.na(bee.phenology$resolvedBeeNames) == FALSE,]


# Need to remove rows without species name from the bee.phenology dataframe
bee.phenology <- bee.phenology[bee.phenology$scientificName %in% bee.species,]




# I'm not sure why this is coming up with a different number of rows
nrow(bee.phenology)


# Is the bee phenology data (bee.phenology) in the same order as the list of bee species?
# YES - alphabetical order
# View(cbind(bee.phenology$scientificName, rownames(bee.plant.date.cite)))



# Add a new Genus species column to the plant phenology data
plant.phenology$Genus_species <- paste(plant.phenology$Genus,
                                       plant.phenology$Species,
                                       sep = " ")

# Is the plant phenology data (plant.phenology) in the same order as the list of bee species?
# NO - different orders
re.ordered.plant.phenology <- plant.phenology[match(colnames(bee.plant.date.cite),
                                                    plant.phenology$Genus_species),]

# View(cbind(re.ordered.plant.phenology$Genus_species, colnames(bee.plant.date.cite)))



# Next, we will be working on a long dataframe and specify which bee-plant interactions are:
# possible
# AND forbidden

# Create empty dataframe for possible bee-plant interactions
bee.plant.inter <- data.frame(beeID = NA,
                              plantID = NA,
                              monthID = NA)

# Create empty dataframe for FORBIDDEN bee-plant interactions
bee.plant.forbid <- data.frame(beeID = NA,
                               plantID = NA,
                               monthID = NA)

# These objects will serve as counters, it will make sense in the loop
a <- 1
b <- 1

# Start the loop
for(i in 1:nrow(bee.plant.date.cite)){ # For each bee species
  for(j in 1:ncol(bee.plant.date.cite)){ # For each plant species
    for(k in 1:12){ # for each month
      
      # There is a row in the plant.phenology table that is all NA - we need to skip it
      if(is.na(plant.phenology[j, k + 26]) == FALSE){
        
        
        # Possible interactions
        if(# If the bee is active (bee.phenology == 1)
          bee.phenology[i, k+1] == 1 &
          
          # AND if the plant is available (plant.phenology == 1)
          plant.phenology[j, k + 26] == 1){
          
          # Then that bee plant interaction can occur and write it in the file: bee.plant.inter
          bee.plant.inter[a,1] <- i
          bee.plant.inter[a,2] <- j
          bee.plant.inter[a,3] <- k
          
          # Add 1 to the counter
          a <- a + 1
        }
        
        # Forbidden links
        if(# If the bee is NOT active
          bee.phenology[i, k+1] == 0 |
          
          # OR if the plant is not available
          plant.phenology[j, k + 26] == 0){
          
          # This is a forbidden link, and write it here
          bee.plant.forbid[b,1] <- i
          bee.plant.forbid[b,2] <- j
          bee.plant.forbid[b,3] <- k
          
          # Add 1 to the counter
          b <- b + 1
        }
        
      }
      
    }
    
  }}







# 8. Format the data for the model -------------------------------------------------------



# The data will be a 4-D array
# Dim 1 = bee species
# Dim 2 = plant species
# Dim 3 = month
# Dim 4 = citation


# Drop unused levels
dat8 <- droplevels(dat8)

# Create a citation list object
cit.list <- unique(dat8$citation)

# Create the array that will be filled in
bee.plant.date.cite <- array(0, dim = c(length(bee.species), # Number of bee species
                                        length(plant.species), # Number of plant species
                                        12, # 12 months
                                        length(cit.list))) # Number of unique references


# Add row names
rownames(bee.plant.date.cite) <- bee.species

# Add column names
colnames(bee.plant.date.cite) <- plant.species

# Add sheet names - month
dimnames(bee.plant.date.cite)[[3]] <- c(1:12)

# Add 4th dimension names
dimnames(bee.plant.date.cite)[[4]] <- cit.list


# Total number of interactions by citations
# 15,015,504
length(bee.species) *
  length(plant.species)*
  12*
  length(cit.list)




# Fill in the 4-D array with the data

# First we fill in the possible interactions with a 0 - then, we will go back and fill them in with a 1 if they were detected
# To do this, we need to determine which months each citation went out to look for the bee-plant interaction

# For each citation - loop through them
for(j in 1:length(unique(dat8$citation))){
  
  # Subset the data
  dat.sub <- dat8[dat8$citation == dat8$citation[j],]
  
  # Determine what months they were out looking
  citation.months <- as.numeric(unique(dat.sub$month))
  
  # Trim the NA's
  citation.months <- citation.months[which(is.na(citation.months) == FALSE)]
  
  for(i in 1:nrow(bee.plant.inter)){
    
    bee.plant.date.cite[bee.plant.inter[i,1],
                        bee.plant.inter[i,2],
                        bee.plant.inter[i,3],
                        citation.months]     <- 0
    
  }
  
}


# Now we will fill in the 4-D array with the presence data
for(i in 1:nrow(dat8)){
  
  # Determine which citation
  cit.pos <- which(cit.list %in% dat8$citation[i] == TRUE)
  
  # Determine which bee
  bee.pos <- which(bee.species %in% dat8$sourceTaxonSpeciesName[i]  == TRUE |
                     bee.species %in% dat8$sourceTaxonName[i] == TRUE |
                     bee.species %in% dat8$targetTaxonSpeciesName[i] == TRUE |
                     bee.species %in% dat8$targetTaxonName[i] == TRUE )
  
  # Determine which month
  month.pos <- as.numeric(dat8$month[i])
  
  # Determine which plant
  plant.pos <- which(plant.species %in% dat8$sourceTaxonSpeciesName[i]  == TRUE |
                       plant.species %in% dat8$sourceTaxonName[i] == TRUE |
                       plant.species %in% dat8$targetTaxonSpeciesName[i] == TRUE |
                       plant.species %in% dat8$targetTaxonName[i] == TRUE )
  
  # If we have a value for each of the positions, then add a 1
  if(is.na(cit.pos) == FALSE &
     is.na(bee.pos == 0) == FALSE &
     is.na(month.pos == 0) == FALSE &
     is.na(plant.pos == 0) == FALSE) {
    
    # Add a 1
    bee.plant.date.cite[bee.pos, plant.pos, month.pos, cit.pos] <- 1
    
  }
  
  
}




# 9. Save the data -------------------------------------------------------


# Save the 4-D array
save(bee.plant.date.cite, file= "./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2021_12_29.rds")


# Save the subsetted Globi data
save(dat8, file= "./Data/data_summary/FINAL - subsetted_globi_data_2021 12 29.csv" )



# End script

