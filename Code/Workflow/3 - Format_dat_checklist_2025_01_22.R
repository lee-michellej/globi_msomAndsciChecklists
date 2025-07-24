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
  # We will use citations & collections are the "replicate surveys" (rather than temporal or spatial surveys)
  # This model does not take time or space into account


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
  # 4. Temporal sampling bias
      # There are some interactions that occur during particular times of the year



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

# Update in January 2025: Remove a few bee species that are dubious for actually being on SCI. They are only in the current checklist because there is a specimen record, but it is thought to be a misidentification.
bee.sp.remove <- c("Colletes kincaidii", "Bombus occidentalis", "Halictus harmonius", "Eucera lunata")

bee.list <- bee.list[!bee.list$scientificName %in% bee.sp.remove, ]


# Final number of bee species
  # Removed rows not identified to species level
  # 133
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
  # 970

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

# write.csv(not.matched, "~/Downloads/globibees_namesmissing_01apr24.csv", row.names = F)
# write.csv(not.matched, "~/Downloads/globibees_namesmissing_30may24.csv", row.names = F)


# Are there any no matches?
  # 26
length(which(dat1$resolvedBeeNames == "no:match"))

# How many have NAs?
  # 743
length(which(is.na(dat1$resolvedBeeNames) == TRUE))

# Are there any blanks?
  # 201
length(which(dat1$resolvedBeeNames == ""))


# Number of rows that did not produce matches
  # 970
nrow(not.matched.bee.rows)

# Drop unused levels
not.matched.bee.rows <- droplevels(not.matched.bee.rows)

# Most of the rows that aren't matching is because they are "sp." species
  # 169
length(unique(not.matched.bee.rows$sourceTaxonSpeciesName))
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
# file now is 292 473 rows
nrow(dat1)



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
  # 133 species
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
length(plant.species2) # 302 plants


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
  # 281 637
remove.rows


# How many rows will be kept?
  # 10 836 rows will be kept (of 281,405 records)
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
nrow(dat2)
  # 10 836

nrow(dat1)
  # 292 473









# 7. Add a new column to globi data with the institution codes to replace SCAN citation -------------------------------------------------------



# Our objective here is to add a new column to globi data with the institution codes to replace SCAN citation because some institutions have multiple SCAN citations and should be combined

# We will be working with the following 2 objects in this section:
 # dat2 = the subsetted species from the original globi data pull
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
  # 5 341

# Look at the levels of the sourceCitation
dat2 <- droplevels(dat2)

dat2$sourceCitation <- as.factor(dat2$sourceCitation)

levels(dat2$sourceCitation)
length(levels(dat2$sourceCitation))
  # 33 sources

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

# Look at the number of samples per source citation
citation.sampl.size <- data.frame(table(dat2$resolvedSource))
citation.sampl.size

# Add row names
rownames(citation.sampl.size) <- names(table(dat2$resolvedSource))

# View(citation.sampl.size)

colnames(citation.sampl.size) <- c("citation", "total-obs")


# Look at the numnber of unique citations
citations <- levels(as.factor(dat2$resolvedSource))
length(levels(as.factor(dat2$resolvedSource)))
  # 50 unique citations

# Write the file with the final list of source names
# write.csv(citations, "./Data/final-globi-citations-unique 2024 04 07.csv")







 
 # 11. Create the 3-D array we will fill in -------------------------------------------------------
 
 
 
 
 
 # The data will be a 4-D array
 # Dim 1 = bee species
 # Dim 2 = plant species
 # Dim 3 = citation
 
 # Create the array that will be filled in
 bee.plant.cite <- array(0, dim = c(length(bee.species), # Number of bee species
                                          length(plant.phenology2$resolvedPlantNames), # Number of plant species
                                          length(citations))) # Number of unique references
 

 # Total number of interactions by citations
 length(bee.species) *
   length(plant.phenology2$resolvedPlantNames)* 
   length(citations)
 # = 2 008 300

 
 
 
# 12. Fill in the 3-D array for the model -------------------------------------------------------


 



# Now we will fill in the 3-D array with the detection data
start.time <- Sys.time()
for(i in 1:nrow(dat2)){
  
  if(is.na(dat2$resolvedSource[i]) == FALSE){
  
  # Determine which citation
  cit.pos <- which(citations %in% dat2$resolvedSource[i] == TRUE)
  
  # Determine which bee
  bee.pos <- which(bee.species %in% dat2$resolvedBeeNames[i]  == TRUE)
  
  # Make sure that each observation is accounted for
  if(length(cit.pos) == 0 |
     length(bee.pos) == 0 ) break
  
  # Determine which plant
  plant.pos <- which(plant.phenology2$resolvedPlantNames %in% dat2$resolvedPlantNames[i]  == TRUE)
    
    # Add a 1
    bee.plant.cite[bee.pos, 
                   plant.pos, 
                   cit.pos] <- 1
  }
}

end.time <- Sys.time()
#beepr::beep(3)

# How long did the loop take?
end.time - start.time

# How many detections are in the dataframe?
  # Note that in some cases the same citation documents the same bee and plant interactions during the same month
length(which(bee.plant.cite == 1))
  # 1 881 unique bee-plant-citation detection

unique_obs_per_source <- data.frame(citation = citations, 
                                    unique_obs = apply(bee.plant.cite, 3, sum))

table_S1 <- merge(unique_obs_per_source, citation.sampl.size, by = "citation")


# Write a csv file
# write.csv(table_S1, "./Tables/2025 01 26/Table-S1-citations.csv")





# 15. Determine the number of unique bee-plant-citation detections -------------------------------------------------------------- 




# Determine the number of unique bee-plant detections per citation
tot.bee.plant <- apply(bee.plant.cite, c(3), sum, na.rm = TRUE)

# hist(tot.bee.plant, breaks = 100)

sort(tot.bee.plant)

# We will remove the sourceCitations with < 10 unique bee-plant interactions
# sourceCit.10.plus <- which(tot.bee.plant > 9)

# We will remove the sourceCitations with < 100 unique bee-plant interactions
sourceCit.100.plus <- which(tot.bee.plant > 99)


# Keeping the dimensions with tot.bee.plant 100 +
bee.plant.cite <- bee.plant.cite[ , , sourceCit.100.plus]


# List of soures we kept
  # 6 total
citations[sourceCit.100.plus]

# Write csv file
# write.csv(citations[sourceCit.10.plus],"./Data/sources.kept.2024.07.01.csv")

# Look at the number of dimensions
dim(bee.plant.cite)

# Total number of unique bee-plant observations across all source citations
sum(bee.plant.cite)
  # 1 315

# Number of unique combinations
dim(bee.plant.cite)[1]*
  dim(bee.plant.cite)[2]*
  dim(bee.plant.cite)[3]
  # 240 996




# Create a list with other info about the data and the species order
dat_info <- list()

dat_info[[1]] <- bee.species

dat_info[[2]] <- plant.species2

dat_info[[3]] <- citations[sourceCit.100.plus]


names(dat_info) <- c("bee.species", "plant.species", "citations")





# 15. Subset the dat2 object -------------------------------------------------------




# Subset the dat2 object to only the final sourceCitations
dat3 <- dat2 %>% 
  filter(resolvedSource %in% citations[sourceCit.100.plus])

# Determine number of observations in filtered data versus previous data
nrow(dat3)
  # 8 052
nrow(dat2)
  # 10 836


# Determine how many entries do not have date/month
length(which(is.na(dat3$eventDate) == TRUE))
length(which(dat3$eventDate == ""))

# Number of observations per bee species with < 10 and < 15 observations
length(which(count(dat3$resolvedBeeNames)$freq < 10))
length(which(count(dat3$resolvedBeeNames)$freq < 15))

# Number of observations per plant species with < 10 and < 15 observations
length(which(count(dat3$resolvedPlantNames)$freq < 10))
length(which(count(dat3$resolvedPlantNames)$freq < 15))





# 16. Save the data -------------------------------------------------------


# Save subsetted globi data
save(dat3,
     file = "./Data/final_globi_data.csv")


# Save the 3-D array
save(bee.plant.cite, 
     file= "./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")


# Save the dat_info
save(dat_info,
     file = "./Data/dat_info_2025_01_22.rds")


## Save the subsetted Globi data
# save(dat3,
#     file= "./Data/data_summary/FINAL - subsetted_globi_data_2024 04 08.csv" )



# End script

