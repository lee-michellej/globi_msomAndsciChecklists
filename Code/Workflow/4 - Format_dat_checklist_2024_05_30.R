########################################
########################################
# This code was written by: G. V. DiRenzo & M. J. Lee
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code objective ##########
##################################


# To format the globi data [4D array (bee species x plant species x month x citation) & in 2D] for the analysis

# The objective of the analysis is to determine the number of plants that each bee species interacts with while accounting for sampling bias

# We will be using a multi-species occupancy model for the analysis
  # We will account for bee-plant interactions that were not observed
  # We will account for bee-plant interactions that can not occur
    # e.g., bees aren't active when plants are flowering (forbidden links)
  # We will use citations & collections are the "replicate surveys" (rather than temporal or spatial surveys)
  # We will account for the month that citations & collections were searching for bee-plant interactions
  # This model does not take space into account


##################################
######## Output of Code ##########
##################################


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


# The Globi database consists of detection-only data.
  # We used a checklist from a specific place (Santa Cruz Island) to generate a list of bee and plant species that co-occur together
  # We will assign non-detections by compiling a checklist of the bees and plants for Santa Cruz island and assigning a 0 when they are not observed
  # Note - we use ALL observations in Globi, regardless of location, because many observations in globi do not include lat/long info and including the observations made the final dataset more robust
    # Taking this approach assumes that each citation (the unit we are using as the replicate) had the possibility of observing all bees and plants in the checklists (which might not always be true)


# We note that databases are plagued by 3 at least sampling biases:
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




#* NOTE --- all of the file paths will need to be deleted before we add these to the code release




# 1. Load libraries & set working directory -------------------------------------------------------






# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)

# Set working directory
setwd("~/globi_tritrophic_networks/")

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  # This function sets the working directory to wherever this file is located
setwd("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/")
# Check the working directory
getwd()






# 2. Load data -------------------------------------------------------





# Pre-processing steps to Globi data (that take a while to run):
  # Step 1: Standardizing columns where bee & plant species names appear
    # File names: 1-SourceTargetStandardization.Rmd
  # Step 2: Add resolved plant species names (resolvedPlantName)
            # Will be done on the Globi datset
            # And on the plant phenology list
    # File names: 2 - PlantListResolver.R

# Read in data
  # This data was downloaded from globi 
  # resolvedplantnamesglobi_12feb24.csv = This is the whole Globi csv file- with the new resolvedPLant names column
# dat <- read.csv("./Data/resolvedplantnamesglobi_12feb24.csv")
# dat <- read.csv("/Volumes/SanDisk/LARGE_globiDataFiles/resolvedplantnamesglobi_12feb24.csv")
 dat <- read.csv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/resolvedplantnamesglobi_12feb24.csv")



# Read in bee phenology data with the bee checklist names
bee.list <- read.csv("./Data/SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv")


# In this code, we will go through all of the scientific bee names and identify synonyms & update them all to the latest nomenclature
# To do this - we need to load several files, which were compiled from different sources with scientific bee name synonyms 
#----- We need 6 separate files that store bee names 

# Bee name list from Discover Life (published on Zenodo:https://zenodo.org/records/10463762)
#bee.names.jan24 <- read_tsv("/Volumes/SanDisk/LARGE_globiDataFiles/discoverlife-January-05-2024.tsv") %>% 
bee.names.jan24 <- read_tsv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/discoverlife-January-05-2024.tsv") %>% 
  dplyr::select(1:11)
# Add column names
colnames(bee.names.jan24) <- c("providedExternalId",
                               "providedName",
                               "providedAuthorship",
                               "providedRank",
                               "providedPath",
                               "relationName",
                               "resolvedExternalID",
                               "resolvedExternalId",
                               "resolvedAuthorship",
                               "resolvedRank",
                               "resolvedPath"
                               )
# Quick note about column names in this file:
  # providedName (old scientific name) --> relationName (latest scientific name)


# Bee name list from Discover Life that was used in previous script
bee.names.orig <- read.csv("./Data/discoverlife-Anthophila-2022 04 01.csv")
# Quick note about column names in this file:
  # providedName (old scientific name) --> resolvedName (latest scientific name)


# Bee name list that Katja generated for Globi Names not included in Discover Life
bee.names.globi <- read.csv("./Data/Globi-names-not-in-discoverlife - Sheet1 2022 04 01.csv")
# these are names that were manually edited by Katja
  # sourceTaxonSpeciesName (old scientific name) --> resolvedBeeNames (latest scientific name)

# Bee name list from the Big Bee Network that Katja and others.
# This file deals with a lot of the typos we were having issues with.
# bee.names.bigbeenetwork <- read.csv("/Volumes/SanDisk/LARGE_globiDataFiles/names-aligned.csv")
bee.names.bigbeenetwork <- read.csv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/names-aligned.csv")
# Quick note about column names in this file:
  # providedName (old scientific name) --> alignedSpeciesName (latest scientific name)

# Bee name list from Michelle that was a manual edit to match the last few names that were unmatched by above lists.
# bee.names.editmar24 <- read.csv("./Data/globibees_namesmissing_26mar24.csv")
bee.names.editmar24 <- read.csv("/Users/gdirenzo/OneDrive - University of Massachusetts/_My-Projects/GloBi/Data/globibees_namesmissing_26mar24.csv")

# Quick note about column names in this file:
  # name (old scientific name) --> resolvedName (latest scientific name)

#------ End files for bee matching (synonyms)




# Institution codes
  # This file matches the institution codes in the globi database to a name
institution.codes <- read.csv("./Data/institutioncodes_2021_12_16.csv")


# Read in plant phenology data
  # Resolving the plant names (i.e., updating the synonyms to the latest scientific name) was done in a previous file
plant.phenology <- read.csv("./Data/resolvedplantsci_12feb24.csv")


# This is the bee phenology data with the following modification:
  # Remove Apis mellifera
bee.list <- bee.list[bee.list$specificEpithet != "mellifera",]




################
################
################




# Look at data structure
str(dat)
  # Look at globi for column definitions
  # 380597 observations

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
  # 140
nrow(bee.list)

# Remove species without a species name ( == "sp.")
bee.list <- bee.list[bee.list$specificEpithet != "sp.",]


# Final number of bee species
  # Removed rows not identified to species level
  # 137
nrow(bee.list)




# Confirm that Apis mellifera DOES or DOES NOT appear
which(bee.list$scientificName == "Apis mellifera")
  # interger(0) == does not appear






# 4. General metrics from Globi data -------------------------------------------------------






# The downloaded Globi dataset has:
  # 380 597 rows
nrow(dat)



# Remove rows without a bee species name and without plant species name
dat1 <- dat[dat$sourceTaxonSpeciesName != "" & dat$targetTaxonSpeciesName != "", ]
dat1 <- dat1[is.na(dat1$sourceTaxonSpeciesName) == FALSE & 
              is.na(dat1$resolvedPlantNames) == FALSE, ]

# Number of unique observations after removing rows without a bee species name and without plant species name
  # 293 443
nrow(dat1)

# Create a list with all of the unique species in Globi - 
# All of the insect related info in the source column
# All of the plant related info in the target column

# Plant & Bee species
globi.sp <- unique(c(as.character(unique(dat1$sourceTaxonSpeciesName)), 
                     as.character(unique(dat1$targetTaxonSpeciesName))))

# Number of unique bee and plant species (not discriminated)
  # 12 889
length(globi.sp)







# 5. Format the bee synonym data -------------------------------------------------------







# Change the column name from Accepted.Name to resolvedBeeNames
colnames(bee.names.globi)[grep("Accepted.Name", colnames(bee.names.globi))] <- "resolvedBeeNames"


str(bee.names.globi)
   # resolvedBeeNames = latest name
   # sourceTaxonSpeciesName = list of all possible synoynms


# Make a new column for the resolvedBeeNames
dat1$resolvedBeeNames <- NA
dat1$sourceTaxonSpeciesName <- trimws(dat1$sourceTaxonSpeciesName, "both")




# loop to replace bee synonym names =========


# Use a loop to go through each globi bee species list and try to find a match in the synonym list - if there is a match, then assign the resolvedName
  # This function takes ~ 8 minutes to run
start.time <- Sys.time()
for(i in 1:nrow(dat1)){

  #------- 1. Identify the row in the bee.names dataframe that the name matches
  
  # discover life list: providedName --> resolvedName
  row.num.zenodo <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.orig$providedName)[1]

  # big bee network name resolver: providedName --> alignedSpeciesName
  row.num.bigbee <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.bigbeenetwork$providedName)[1]
  # using another column from the big bee list
  row.num.bigbee2 <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.bigbeenetwork$parsedName)[1]
  
  
  # Check the Katja list: sourceTaxonSpeciesName --> resolvedBeeNames
  row.num.kat <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.globi$sourceTaxonSpeciesName)[1]

  # michelle's manual edit march 24: name --> resolvedName
  row.num.mic <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.editmar24$name)[1]
  
  # Discover life download from the start of January 2024
  row.num.jan <- which(dat1$sourceTaxonSpeciesName[i] == bee.names.jan24$providedName)[1]
  
  # The previous line needs to come back with a number to do the next command

  #------  2. Replace the Globi name with the current.name
  
  if(is.na(row.num.zenodo) == FALSE){
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.orig$resolvedName[row.num.zenodo])

  }

  if (is.na(row.num.kat) == FALSE ) {
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.globi$resolvedBeeNames[row.num.kat])

  }

  if (is.na(row.num.mic) == FALSE ) {
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.editmar24$resolvedName[row.num.mic])
    
  }
  
  if (is.na(row.num.bigbee) == FALSE ) {
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.bigbeenetwork$alignedName[row.num.bigbee])
    
  }
  
  if (is.na(row.num.bigbee2) == FALSE ) {
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.bigbeenetwork$alignedName[row.num.bigbee2])
    
  }
  
  if (is.na(row.num.jan) == FALSE ) {
    
    dat1$resolvedBeeNames[i] <- as.character(bee.names.jan24$relationName[row.num.jan])
    
  }
  
}
end.time <- Sys.time()
beepr::beep(1)
# Determine amount of time to run for loop =======
end.time - start.time 




# Pull out the rows in the GLobi dataset that were NOT matched with any species names in the synoynm list (from Zenodo) - i.e., produced an NA, a "no:match" or a blank
not.matched.bee.rows <- dat1[is.na(dat1$resolvedBeeNames) == TRUE |
                               dat1$resolvedBeeNames == "no:match" |
                               dat1$resolvedBeeNames == "", ]

# Calculate the total number of rows that do not match
nrow(not.matched.bee.rows)

# Pull out the names that did not match into a single list - this condenses it to a list of unique entries
not.matched <- not.matched.bee.rows %>% 
  distinct() %>% 
  filter(str_detect(str_trim(sourceTaxonSpeciesName), "\\s+")) %>% 
  filter(str_count(sourceTaxonSpeciesName, fixed(".")) == 0) %>% 
  filter(str_count(sourceTaxonSpeciesName, fixed("?")) == 0) %>% 
  filter(str_count(sourceTaxonSpeciesName, fixed("/")) == 0) %>% 
  filter(str_count(sourceTaxonSpeciesName, fixed(",")) == 0) %>% 
  dplyr::select(sourceTaxonSpeciesName) %>% 
  distinct()

#write.csv(not.matched, "~/Downloads/globibees_namesmissing_01apr24.csv", row.names = F)
write.csv(not.matched, "~/Downloads/globibees_namesmissing_30may24.csv", row.names = F)


# Are there any no matches?
  # May 2024 = 26
length(which(dat1$resolvedBeeNames == "no:match"))

# How many have NAs?
  # May 2024 = 743
length(which(is.na(dat1$resolvedBeeNames) == TRUE))

# Are there any blanks?
  # May 2024 = 201
length(which(dat1$resolvedBeeNames == ""))


# Number of rows that did not produce matches
  # May 2024 = 970
nrow(not.matched.bee.rows)

# Drop unused levels
not.matched.bee.rows <- droplevels(not.matched.bee.rows)

# Most of the rows that aren't matching is because they are "sp." species
  # May 2024 = 169
unique(not.matched.bee.rows$sourceTaxonSpeciesName)

# Select the columns you want to keep
unique.rows <- not.matched.bee.rows %>%
                  select(32:52) %>%
                  unique()

# Save output
# write.csv(unique.rows, "./Data/no-resolved-bee-names-2022 02 18.csv")



# After reviewing the names that didn't match,
# they were mostly sp. names or morphospecies descriptions. 
# We remove the species names that were not resolved.
dat1 <- dat1[-c(which(is.na(dat1$resolvedBeeNames) == TRUE |
                            dat1$resolvedBeeNames == "no:match" |
                            dat1$resolvedBeeNames == "")), ]
# file now is 292473 rows




# Now we have the Globi dataset with updated bee names




# Next, we need to update our bee checklist/phenology object with the resolvedNames

# Make a new column for the resolvedBeeNames
bee.list$resolvedBeeNames <- NA

# Use a loop to go through each scientificName and match with the globi species list
for(i in 1:nrow(bee.list)){
  
  # 1. Identify the row in the bee.names4 dataframe that the name matches
  # Zenodo list
  row.num.zen <- which(bee.list$scientificName[i] == bee.names.orig$providedName)[1]
  
  row.num.kat <- which(bee.list$scientificName[i] == bee.names.globi$sourceTaxonSpeciesName)[1]
  
  
  # The previous line needs to come back with a number to do the next command
  if(is.na(row.num.zen) == FALSE){
    
    # 2. Replace the Globi name with the current.name
    bee.list$resolvedBeeNames[i] <- as.character(bee.names.orig$resolvedName[row.num.zen])
    
  } 
  
  if(is.na(row.num.kat) == FALSE){
    
    bee.list$resolvedBeeNames[i] <- as.character(bee.names.globi$resolvedBeeNames[row.num.kat])
    
  }
  
}

# Pull out the rows in the bee.list dataset that were NOT matched with any species names in the synoynm list (from Zenodo)
  # All should have matches
  # This should return a vector of length 0
bee.list[is.na(bee.list$resolvedBeeNames) == TRUE |
           bee.list$resolvedBeeNames == "no:match", ]













# 6. Subset Globi to rows with the bee-plant interactions that appear in our checklist -------------------------------------------------------








# Unique bee species
  # 137 species
bee.species <- unique(bee.list$resolvedBeeNames)
bee.species <- bee.species[is.na(bee.species) == FALSE]
bee.species <- bee.species[bee.species != "no:match"]

length(bee.species)


grep("Apis mellifera", bee.species)

length(bee.species)

# Unique plant species
  # 516 species = complete list
  # 126 species = shortened list WITHOUT Apis
plant.species <- unique(plant.phenology$scientificName)
plant.phenology2 <- filter(plant.phenology, plant.phenology$resolvedPlantNames %in% dat1$resolvedPlantNames)
plant.species2 <- unique(plant.phenology2$scientificName)

length(plant.species) # 566 plants
length(plant.species2) # 302 plants as of 2024


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
  
  if(dat1$resolvedPlantNames[i] %in% plant.phenology2$resolvedPlantNames){
    dat1$Plant[i] <- 1
  } else {dat1$Plant[i] <- 0}

}
end.time <- Sys.time()

# How long did it take to run the loop?
end.time - start.time # 3 min
beepr::beep(1)


# Sum the plant & insect columns
  # if it = 1; then, there was only 1 match (either bee or plant)
  # if it = 2; then, both bee and plant matched
  # if it = 0; then, there were no matches (bee and plant did NOT match)
dat1$tot <- dat1$Plant + dat1$Insect


# Determine the number of records that will be discarded
remove.rows <- length(which(dat1$tot < 2))

# How many rows will be removed?
  # 28,2918 --> 28 May 2024
remove.rows


# How many rows will be kept?
  # 11068 rows will be kept (of 292 473 records)
length(which(dat1$tot == 2))


# Quick check
  # the number of rows removed + those kept should == the total number of rows in the Globi data
remove.rows + length(which(dat1$tot == 2)) == nrow(dat1)


# Remove rows with < 2 in the tot column
dat2 <- dat1[which(dat1$tot == 2),]

# Drop unused levels
dat2 <- droplevels(dat2)

# Save the rows with a complete bee-plant match
## write.csv(dat2, "./Data/matched_rows_2024_04_06.csv")


# look at citations
##View(table(dat2$sourceCitation))

# Number of observations
  # 11068
nrow(dat2)

nrow(dat1)










# 7. Add a new column to globi data with the institution codes to replace SCAN citation -------------------------------------------------------



# Our objective here is to add a new column to globi data with the institution codes to replace SCAN citation because some institutions have multiple SCAN citations and should be combined

# We will be working with the following 2 objects in this section:
 # dat2 = the subsetted species
 # institution.codes = the name of the institutions

# The institution codes can be found in this column of the Globi database:
  # sourceInstitutionCode
  # the previous dataset used sourceCatalogNumber to split into institution code segments, 
  # but this column has become much less succinct in the latest upload of data

# Here, we subset the data to those that have a SCAN sourceCitation
# after looking at scan-specific data, it looks like there is only one source that should be split: ESSIG-UTB

# The column sourceInstitutionCode does not need to be modified

# institution.codes object
  # 3 columns
  # fullcode
  # firstcode
  # secondcode

# Next we will match 2 columns globi dataset (code, sourceInstitutionCode) with the 3 columns in the institution.codes object


# Make a new column for the resolvedInstitutionName
dat2$resolvedSource <- NA



# Use a loop to go through each row of dat2 and try to match with any of the 3 columns in institution.codes object
for(i in 1:nrow(dat2)){
  
  # Using sourceInstitutionCode, look to see if it matches any of the first 3 columns in the institution.codes object
  first.col <- which(dat2$sourceInstitutionCode[i] == institution.codes$fullcode)[1]
  sec.col   <- which(dat2$sourceInstitutionCode[i] == institution.codes$firstcode)[1]
  third.col <- which(dat2$sourceInstitutionCode[i] == institution.codes$secondcode)[1]
  
  
  if(is.na(first.col) == FALSE){
    
    # 2. Add in a resolvedSource
    dat2$resolvedSource[i] <- as.character(institution.codes$institution[first.col])
    
  } 
  
  if (is.na(sec.col) == FALSE){
    
    dat2$resolvedSource[i] <- as.character(institution.codes$institution[sec.col])
    
  }
  
  if (is.na(third.col) == FALSE){
    dat2$resolvedSource[i] <- as.character(institution.codes$institution[third.col])
    
  } 
  
  if (is.na(first.col) == TRUE &
      is.na(sec.col) == TRUE &
      is.na(third.col) == TRUE ) {
    
      dat2$resolvedSource[i] <- NA
    
    }
  
  
}

# Looks good!
# View(dat2[,c("resolvedSource", "sourceInstitutionCode", "sourceCitation")])

# Determine how many rows have an institution
length(which(is.na(dat2$resolvedSource)== FALSE))


# Look at the levels of the sourceCitation
dat2 <- droplevels(dat2)

dat2$sourceCitation <- as.factor(dat2$sourceCitation)

levels(dat2$sourceCitation)

# Now we need to merge the resolvedSource column (which replaces the SCAN names) and the sourceCitation column
for(i in 1:nrow(dat2)){
  
  # If the resolvedSource == NA, then do this:
  if(is.na(dat2$resolvedSource[i]) == TRUE){
    
    # Copy over the sourceCitation
    dat2$resolvedSource[i] <- as.character(dat2$sourceCitation[i])
    
  }
  
}


# Write the file with the final globi dataset
#write.csv(dat2, "./Data/final-globi-list-clean 2024 04 07.csv")

# Look at the numnber of unique citations
citations <- levels(as.factor(dat2$resolvedSource))
  # 50 unique citations

# Write the file with the final list of source names
# write.csv(citations, "./Data/final-globi-citations-unique 2024 04 07.csv")






# 9. Format the date column in globi data -------------------------------------------------------






# Pull apart the info in the event date
  # Year
  # Month
  # Day - this column also includes time - but I won't finish formatting this because we don't need this detail
dat3 <- as_tibble(dat2) %>%
  mutate(year = year(eventDate),
         month= month(eventDate),
         day = day(eventDate))


# Drop unused levels
dat3 <- droplevels(dat3)


# Remove rows with no month info
dat3 <- dat3[dat3$month != "",]

# Save month as numeric
dat3$month <- as.numeric(dat3$month)
dat3 <- dat3[is.na(dat3$month) == FALSE,] # this brings down to 9956 observations


# Remove repeat observations
dat3 <- dat3 %>%
          distinct()


# remove any NA rows in dat3
dat3 <- dat3[is.na(dat3$resolvedSource) == FALSE,]

nrow(dat3)
  # this brings down to 9605 observations



# Before we move on to determining which bee-plant interactions are possible or formatting the data, we should check the number of unique bee-plant-citation-month observations
# Subset the data
dat4 <- dat3[, c("resolvedBeeNames", "resolvedPlantNames", "resolvedSource", "month")]

# Remove duplicates
dat5 <- dat4 %>%
  distinct()

# Number of unique combinations of bee-plant-citation-month observations
nrow(dat5)
  # 2 746





# 10. Determine which bee-plant interactions are possible based on phenology -------------------------------------------------------







# Next, we will be working on a long dataframe and specify which bee-plant interactions are:
  # possible

# Create empty dataframe for possible bee-plant interactions
bee.plant.inter <- data.frame(beeID = NA,
                              plantID = NA,
                              monthID = NA)

# Start the loop
  # This takes 1.6 mins to run
start.time <- Sys.time()

 for(i in 1:nrow(bee.list)){ # For each bee species
  for(j in 1:nrow(plant.phenology2)){ # For each plant species

      if(length(which(plant.phenology2[j, 27:(26+12)] + bee.list[i, 7:(6+12)] == 2)) > 0){
        
        new.dat <- data.frame(beeID = i,
                              plantID = j,
                              monthID = which(plant.phenology2[j, 27:(26+12)] + bee.list[i, 7:(6+12)] == 2))
        
        bee.plant.inter <- rbind(bee.plant.inter, new.dat)
        
        
      }

    }

   print(paste0("Done with Bee ID # ", i, "..."))
   
  }

end.time <- Sys.time()
beepr::beep(3)

# How long did the loop take?
end.time - start.time

# How many possible interactions are there?
  # 129, 945
nrow(bee.plant.inter)

# Save the bee.plant.inter file
 # This file takes a while to generate - so in the future - we can just import it into this space
# save(bee.plant.inter, file= "./Data/bee_plant_inter_2024_05_30 - short plant - no apis.rds")




 
 # 11. Create the 4-D array we will fill in -------------------------------------------------------
 
 
 
 
 
 # The data will be a 4-D array
 # Dim 1 = bee species
 # Dim 2 = plant species
 # Dim 3 = month
 # Dim 4 = citation
 
 # Create the array that will be filled in
 bee.plant.date.cite <- array(NA, dim = c(length(bee.species), # Number of bee species
                                          length(plant.phenology2$resolvedPlantNames), # Number of plant species
                                          12, # 12 months
                                          length(citations))) # Number of unique references
 

 # Total number of interactions by citations
 length(bee.species) *
   length(plant.phenology2$resolvedPlantNames)* 
   12*
   length(citations)
 # May 2024 = 24 824 400

 
 
 
# 12. Fill in the 4-D array for the model -------------------------------------------------------


 
 

 
 
# First- we fill in the possible interactions with a 0 (non-detection) - then, we will go back and fill them in with a 1 if they were detected
 
# To do this, we need to determine which months each citation went out to look for the bee-plant interaction and fill in the 0's

# For each citation - loop through them
start.time <- Sys.time()
for(j in 1:length(citations)){
  
  # Subset the data by each citation - sometimes this generates NA rows
  dat.sub <- dat5[dat5$resolvedSource == citations[j],]
  
  # Remove rows where months == NA
  dat.sub <- dat.sub[is.na(dat.sub$month) == FALSE,]
  
  # Determine what months each citation was going out looking
  citation.months <- as.numeric(unique(dat.sub$month))
  
  # Here - we subset the bee.plant.inter object (which lists all of the possible bee-plant interactions) to only the months that that citation was searching/looking 
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
# 2 278 441
length(which(bee.plant.date.cite == 0))




# Now we will fill in the 4-D array with the detection data
start.time <- Sys.time()
for(i in 1:nrow(dat3)){
  
  # Determine which citation
  cit.pos <- which(citations %in% dat3$resolvedSource[i] == TRUE)
  
  # Determine which bee
  bee.pos <- which(bee.species %in% dat3$resolvedBeeNames[i]  == TRUE)
  
  # Determine which month
  month.pos <- as.numeric(dat3$month[i])
  
  # Make sure that each observation is accounted for
  if(length(cit.pos) == 0 |
     length(bee.pos) == 0 |
     length(month.pos) == 0) break
  
  # Determine which plant
  plant.pos <- which(plant.phenology2$resolvedPlantNames %in% dat3$resolvedPlantNames[i]  == TRUE)
  
  print(paste("Working on row ", i, "; cit = ", cit.pos, "; bee = ", bee.pos, "; plant = ", plant.pos, "; month = ", month.pos, "\r.."))
    
    # Add a 1
    bee.plant.date.cite[bee.pos, 
                        plant.pos, 
                        month.pos, 
                        cit.pos] <- 1
  
}

end.time <- Sys.time()
beepr::beep(3)

# How long did the loop take?
end.time - start.time

# How many detections are in the dataframe?
  # Note that in some cases the same citation documents the same bee and plant interactions during the same month
length(which(bee.plant.date.cite == 1))
  # 2 746 detection







# 13. Create a matrix with the observed bee-plant-month by citation elements -------------------------------------------------------





# Next, we will be working on a long dataframe and specify which bee-plant interactions were observed

# May 2024 --- we can easily do this with reshape2 instead of this long loop --- see code below

# Convert the 4-D array into a 2-D array using the melt() function in reshape2
bee.plant.obs <- reshape2::melt(bee.plant.date.cite)

# Change the column names
colnames(bee.plant.obs) <- c("beeID", "plantID", "monthID", "sourceID", "y")

# Look at the first few rows
head(bee.plant.obs)

# Make sure you have the same TOTAL number of detections (i.e., sum up all of the y's)
sum(bee.plant.obs$y, na.rm = TRUE)
  # NA = not possible interaction OR the citation didn't go out that month
  # 0 = non-detection (citation went out and did not detect the interaction)
  # 1 = detection     (citation went out and did detect the interaction)

nrow(bee.plant.obs)
  # 24 824 400




# 14. Compare the possible bee-plant interaction matrix (bee.plant.inter) and the observation bee-plant matrix (bee.plant.obs) -------------------------------------------------------




# Number of rows - 
# Observation matrix:
  # bee.plant.obs 954 270
    # May 2024 = 24 824 400

# Interaction matrix
  # bee.plant.inter 88 665
    # May 2024 = 129 945




# Are there any combinations from the observations (bee.plant.date.cite) that aren't in the possible interactions (bee.plant.inter)?
  # To do this, first we start by collapsing observations across 4th dimension (source citations)
bee.plant.date <- apply(bee.plant.date.cite, c(1, 2, 3), max, na.rm = TRUE) 
bee.plant.date[bee.plant.date == "-Inf"] <- NA


# Convert the 4-D array into a 2-D array using the melt() function in reshape2
bee.plant.obs.long <- reshape2::melt(bee.plant.date)
# Change the column names
colnames(bee.plant.obs.long) <- c("beeID", "plantID", "monthID", "y")

# Look at the first few rows
head(bee.plant.obs.long)
  # Reminder in the y column = 
    # NA = not possible interaction OR the citation didn't go out that month
    # 0 = non-detection (citation went out and did not detect the interaction)
    # 1 = detection     (citation went out and did detect the interaction)
 


# Add a column to the possible interactions 
bee.plant.inter$inter <- 1

nrow(bee.plant.obs.long)
  # 496 488
nrow(bee.plant.inter)
  # 129 945

# Merge the new long observations dataframe (bee.plant.obs.long) with the possible interactions data frame (bee.plant.inter)
y2 <- merge(bee.plant.obs.long, 
            bee.plant.inter, 
            all.x = TRUE) # keep all rows in the new long observations dataframe (bee.plant.obs.long) 

nrow(y2)
  # 496 488

head(y2)

# Identify which bee-plant-month observations were detected BUT are not possible
  # An impossible interaction would be the observation (y == 1) but the interaction is not possible (inter == NA)
observed.but.not.possible <- y2[which(y2$y == 1 & is.na(y2$inter) == TRUE),]

nrow(observed.but.not.possible) 
  # 834



# Look at the first row
observed.but.not.possible[1,]



# Look at the possible bee-plant by month interactions
bee.plant.inter[bee.plant.inter$beeID == observed.but.not.possible[1,]$beeID & 
                bee.plant.inter$plantID == observed.but.not.possible[1,]$plantID , ]
  # Note that the monthID is not listed in the possible times they interact


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
            plant.phenology2[observed.but.not.possible$plantID[i],]$resolvedPlantNames
  
}


# Append the phenology information to the observed.but.not.possible object

# Add columns for bee phenology
phen <- data.frame(Bee.Jan = rep(NA,    times = nrow(observed.but.not.possible)),
                      Bee.Feb = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Mar = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Apr = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.May = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Jun = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Jul = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Aug = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Sep = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Oct = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Nov = rep(NA, times = nrow(observed.but.not.possible)),
                      Bee.Dec = rep(NA, times = nrow(observed.but.not.possible)),
                      
                      Plant.Jan = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Feb = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Mar = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Apr = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.May = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Jun = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Jul = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Aug = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Sep = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Oct = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Nov = rep(NA, times = nrow(observed.but.not.possible)),
                      Plant.Dec = rep(NA, times = nrow(observed.but.not.possible)))

observed.but.not.possible <- data.frame(observed.but.not.possible, phen)


# Pull out the column number that identifies the bee phenology
bee.phen.cols <- grep("Bee.", colnames(observed.but.not.possible))
# Only take out months
colnames(observed.but.not.possible)[bee.phen.cols]

# Pull out the column number that identifies the plant phenology
plant.phen.cols <- grep("Plant.", colnames(observed.but.not.possible))
# Only take out months
colnames(observed.but.not.possible)[plant.phen.cols]


# Loop through each row
for(i in 1:nrow(observed.but.not.possible)){
  
  # Determine which row matches between observed.but.not.possible and bee.list
  bee.list.row <- which(observed.but.not.possible$beeName[i] == bee.list$scientificName)

  # Determine which row matches between observed.but.not.possible and plant.phenology2
  plant.list.row <- which(observed.but.not.possible$plantName[i] == plant.phenology2$resolvedPlantNames)
  
    
  # Add the bee phenology info 
  observed.but.not.possible[i, bee.phen.cols] <-  bee.list[bee.list.row, 7:(7+11)]
  
  # Add the plant phenology info
  observed.but.not.possible[i, plant.phen.cols] <-  plant.phenology2[plant.list.row, 27:(27+11)]

}



nrow(observed.but.not.possible)
  # 834

# ## # Write the file
write.csv(observed.but.not.possible,
          file = "./Data/globi-obs-not-possible 2024 05 30 - no apis.csv")



#------ GVD ended here

 
 
 
 # I will look at these entries & decide how to proceed


 
 
 


# To proceed, pick 1 of 2 paths: ==================
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

  plantID <- which(plant.phenology2$resolvedPlantNames == new.month$resolvedPlantNames[i])
  
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
# save(dat3,
#     file= "./Data/data_summary/FINAL - subsetted_globi_data_2024 04 08.csv" )



# End script

