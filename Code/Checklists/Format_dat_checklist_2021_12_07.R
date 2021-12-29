########################################
########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code objective ##########
##################################


# To format the globi data [3D array (bee species x plant species x citation)] for the analysis

# The objective of the analysis is to determine the number of plants that each bee species interacts with while accounting for sampling bias

# We will be using a multi-species occupancy model for the analysis


##################################
######## Output of Code ##########
##################################


# This code generates 1 file: 
  # "./Data/globi_data_formatted_bee_plant_2021_04_05.rds"
  # Object names: globi.dat


##################################
######## Data info ###############
##################################


# The data were downloaded from the Globi database.
  # Observations are obtained from museum collections, citizen science obervers, and research studies worldwide.
  # Studies vary in terms of objectives, study design, etc.


# The Globi database consists of presence-only data.
  # We subsetted the data to only include bee genera and instances of bees interacting with plants (either as the "source" or "target").
  
# We do not explicitly consider space in the model, but we subset the records to only include specific localities
  # Subset the data
    # Lat from 31 - 32
    # Long from -125 to -116
  # In doing this, we are liklely removing the ecological studies from the list
  
# Databases are plagued by 2 problems:
  # 1. taxonomic sampling bias
      # Particular species may be sampled more frequently than others because more is known about them or inference is desired on that species
  # 2. spatial sampling bias
      # Particular areas are easier to reach or sample
  # 2. detection bias
      # Species detectability changes over time and space as a result of observers or number of surveys
      # Number of observers, quality of observers, length of survey, survey conditions

# As a result, patterns may be masked (or there are false patterns) because of observation effort





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





# Read in data
  # Note that this is not in the github repo because the file size is too big
  # This data was downloaded from globi
dat <- read.csv("~/Desktop/Folder/Data_globi/all_bee_data_unique.csv")


# Read in bee phenology data
bee.phenology <- read_tsv("~/Dropbox/Globi/Data/All-California-phenologyTable.tsv")

# Read in the bee checklist
bee.list <- read.table("./Data/SCI-list-04-2021.txt", header = T, sep = "\t")


# Read in plant phenology data
plant.phenology <- read.table("~/Dropbox/Globi/Data/plants-SCI_traitsfull_ml_2021_09_14.txt", header = T, sep = "\t")


# Read in the bee checklist
plant.list <- read_tsv("~/Desktop/Folder/Data_globi/plants-SCI_2021_09_02.tsv")


# Read in file with type of citation
citation <- read.csv("./Data/citation-list-type.csv")


# Other bee names = Synonyms
  # https://docs.google.com/spreadsheets/d/140eMiLBjG7ySc5oviCXJL8A5OT1VTM5rMwEvDTJmd8U/edit#gid=0
bee.names <- read.csv("~/Dropbox/Globi/Data/Checklist-bee-names.csv")


# Institution codes
institution.codes <- read.csv("./Data/institutioncodes_oct21.csv")





# Look at data structure
str(dat)
  # Look at globi for column definitions

str(bee.list)
  # $ genus : Bee genus
  # $ specificEpithet: Bee species name
  # $ infraSpecificEpithet: Bee subspecies name


str(plant.list)
  # $ family: Plant family 
  # $ genus : Plant genus
  # $ specificEpithet: Plant species name
  # $ infraSpecificEpithet: Plant subspecies name
  # $ ScientificName : Plant scientific name









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

# Plant list already has a column with full scientific name

# Make a bee genus species column with infraSpecificEpithet
plant.list$genus_species <- paste(plant.list$Genus, plant.list$Species)

# Remove species without a species name ( == "sp.") - bee list
bee.list <- bee.list[bee.list$specificEpithet != "sp.", ]

nrow(bee.list)




# 4. Subset globi data to rows that have plant records only -------------------------------------------------------





# Next step would be to create new a dataset that also only contain records that also reference plants (Plantae). 
# This step is done independent of the interaction type as authors may use many interactions type names


# The downloaded dataset has:
# 304,795 rows
nrow(dat)



# Pull out if the source or target are bees
plant.source <- grep("Plantae", dat$targetTaxonPathNames)
plant.target <- grep("Plantae", dat$sourceTaxonPathNames)


# Only take unique observations
plant.rows <- unique(c(plant.source, plant.target))

# The number of rows with plant interactions:
# 259,135 rows
length(plant.rows)

# Keep only plant rows
dat1 <- dat[plant.rows,]
nrow(dat1)

# Create a list with all of the unique species in Globi - 
# Plant & Bee
globi.sp <- unique(unique(dat1$sourceTaxonSpeciesName), 
                   unique(dat1$sourceTaxonName),
                   unique(dat1$targetTaxonSpeciesName),
                   unique(dat1$targetTaxonName))

# Number of unique species
  # 3,035
length(globi.sp)



# 5. Format the bee synonym data -------------------------------------------------------




# As a reminder the object: bee.names - contains the bee synonym names
  # We will be working to format this list

# Remove bee names without a species ID
bee.names <- bee.names[as.character(bee.names[,1]) %in% bee.list$genus_species_infra,]

# Pull apart the bee names in 1 column by the ; symbol, and bind it to the original names
bee.names2 <- cbind(as.character(bee.names[,1]), 
                    str_split(bee.names$Synonyms.from.DL, ";", simplify = TRUE)
)

# View the new data frame
# View(bee.names2)

# Remove the ", YEAR" at the end of each entry
for(i in 1:ncol(bee.names2)){
  bee.names2[,i] <- gsub(", [0-9][0-9][0-9][0-9]",'',bee.names2[,i])
}

# Remove the space at the beginning of each name
for(i in 1:nrow(bee.names2)){
  for(j in 1:ncol(bee.names2)){
    bee.names2[i,j] <- trimws(bee.names2[i,j])
  }
}

# Copy over bee.names2
bee.names3 <- bee.names2

# Remove the name of the person that described the species
for(i in 1:nrow(bee.names2)){
  for(j in 1:ncol(bee.names2)){
    
    if(length(str_split(bee.names2[i,j], " ", simplify = TRUE)) > 2){
      
      bee.names3[i,j] <- gsub("\\s*\\w*$", "", bee.names2[i,j])
    
      } else {bee.names3[i,j] <- bee.names2[i,j]}
  }
  
} 

# View the new data frame
# View(bee.names3)

# Remove anything with a ", ...."
for(i in 1:ncol(bee.names3)){
  bee.names3[,i] <- gsub(", .*",'', bee.names3[,i])
}

# Remove anything with a "_ ...."
for(i in 1:ncol(bee.names3)){
  bee.names3[,i] <- gsub("_.*",'', bee.names3[,i])
}

# Remove anything with a "var ...."
for(i in 1:ncol(bee.names3)){
  bee.names3[,i] <- gsub("var .*",'', bee.names3[,i])
}

# View(bee.names3)


# Keep the ( ) names because some Globi entries have the subspecies names

# There is 1 entry [32, 2] that still has the year - remove it
for(i in 1:ncol(bee.names3)){
  bee.names3[,i] <- gsub("[0-9][0-9][0-9][0-9]$",'',bee.names3[,i])
}


# There are some entries with the names of the person descrbing the species at the end
  # e.g., (Cresson), (Robertson), (Timberlake), (Fowler)
for(i in 1:ncol(bee.names3)){
  bee.names3[,i] <- gsub("\\s*\\([^\\)]+\\)$",'', bee.names3[,i])
}

# View(bee.names3)


# It looks like there are STILL author names throughout:
  # Cockerell
  # Radoszkowski
  # etc/
# I need someone who knows what they are doing to look at this


# write.csv(bee.names3, "./Data/bee-list-cleaned 2021 12 28.csv")


# How many of the bee.names3 species are not in the Globi database?
  # We will test with matching & regular expressions
bee_present_match_syn <- bee_present_grep_syn <- matrix(NA, nrow = nrow(bee.names3), ncol = ncol(bee.names3))


# Use a loop to go through each genus_species and match with the globi species list
for(i in 1:nrow(bee.names3)){
  for(j in 1:ncol(bee.names3)){
    
    # Perform if statement if bee.names2 does not equal ""
    if(bee.names3[i,j] != ""){
      # Matching
      bee_present_match_syn[i,j] <- bee.names3[i,j] %in% globi.sp
      
      # Regular expressions
      bee_present_grep_syn[i,j] <- length(grep(bee.names3[i,j], globi.sp))
    }
    
    
  }
}

# Determine how many speces (i.e., rows) were found
# The max should be 1
row_sums2 <- apply(bee_present_match_syn, 1, max, na.rm = TRUE)

sum(row_sums2)

# 93 total species were detected of the 140

# Which species were not detected?
bee.names3[which(row_sums2 == 0), 1]

# write.csv(bee.names3[which(row_sums2 == 0), 1], 
#           "./Data/data_summary/bee-species-not-detected-with-synon- 2021 12 28.csv")







# 4. Adjust species names in the Globi database so that all names are uniform (i.e., replace synoynms with the current name) -------------------------------------------------------



# As a reminder, we are working with the Globi dataset that has this many rows:
nrow(dat1)
  # 259135


# We need to adjust the names in the Globi database with the synonym names - so that we know which species it is suppose to be- rather than having 10+ names for a single species

# Turn the object bee.names3 into a vector
bee.vector <-  c(bee.names3)

# Remove empty cells
bee.vector <- unique(bee.vector[bee.vector != ""])


##########
# Step 1: Determine if the Globi name matches any of the synonym names
##########

# Determine the number of matches between dat1$sourceTaxonSpeciesName and the original bee names checklist
length(which(dat1$sourceTaxonSpeciesName %in% bee.names3[,1] == TRUE))
  # 29,257

# Determine the number of matches between dat1$sourceTaxonSpeciesName and bee.vector
length(which(dat1$sourceTaxonSpeciesName %in% bee.vector == TRUE))
  # 33,139

# We are increasing our number of observations by ~ 4,000 (just by looking at the source column - we need to also check the targe columns)

# Determine the number of matches between dat1$targetTaxonSpeciesName and the original bee names checklist
length(which(dat1$targetTaxonSpeciesName %in% bee.names3[,1] == TRUE))
  # 2,773

# Determine the number of matches between dat1$sourceTaxonSpeciesName and bee.vector
length(which(dat1$targetTaxonSpeciesName %in% bee.vector == TRUE))
  # 2,779

# Not as big of an increase


##########
# Step 2: Convert bee.names to long format - and just search in the synonym column
##########

# Convert bee.names3 to a dataframe
bee.names3 <- as.data.frame(bee.names3)

# Remove the first column
colnames(bee.names3)[1] <- "current.name"

# Convert to long format
bee.names4 <- melt(bee.names3, id = "current.name")[,-2]

# Replace column names
colnames(bee.names4) <- c("current.name", "synonym")

## Make sure there are no duplicate synonyms
as_tibble(bee.names4) %>%
  distinct(synonym) %>%
  group_by(synonym) %>%
  tally() %>%
  filter(n > 1)

bee.names4 <- as_tibble(bee.names4) %>%
                distinct(synonym, .keep_all = TRUE)

# Remove any empty cells
bee.names4 <- bee.names4[bee.names4$synonym != "", ]

View(bee.names4)

##########
# Step 3: Replace the species name in the Globi database with the current species name (column 1 of the bee.names4 dataframe)
##########


# j = 67480


# Convert dat1$sourceTaxonSpeciesName into a character
dat1$sourceTaxonSpeciesName <- as.character(dat1$sourceTaxonSpeciesName)
dat1$sourceTaxonName <- as.character(dat1$sourceTaxonName)

# Convert targetTaxonSpeciesName column into character
dat1$targetTaxonSpeciesName <- as.character(dat1$targetTaxonSpeciesName)
dat1$targetTaxonName <- as.character(dat1$targetTaxonName)



# For each entry in the dat1 dataframe
for(j in 1:length(dat1$sourceTaxonSpeciesName)){
  
  
  # sourceTaxonSpeciesName column
  
  # Skip if cell is NA
  if(is.na(dat1$sourceTaxonSpeciesName[j]) == FALSE){
    
  # Skip if cell is blank
  if(dat1$sourceTaxonSpeciesName[j] != ""){
    

      # Identify if the sourceTaxonSpeciesName matches any of the names in the bee.vector
      if(dat1$sourceTaxonSpeciesName[j] %in% bee.names4$synonym){
        
        # If the species in Globi database does match any of the names in the bee.vector, then do this:
        
        # 1. Identify the row in the bee.names4 dataframe that the name matches
        row.num <- which(dat1$sourceTaxonSpeciesName[j] == bee.names4$synonym)
        
        # 2. Replace the Globi name with the current.name
        dat1$sourceTaxonSpeciesName[j] <- bee.names4$current.name[row.num]
        
      }
 
    }
  
  }
  
  
  # sourceTaxonName column
  
  # Skip if cell is NA
  if(is.na(dat1$sourceTaxonName[j]) == FALSE){
    
    # Skip if cell is blank
    if(dat1$sourceTaxonName[j] != ""){
      
      
      # Identify if the sourceTaxonName matches any of the names in the bee.vector
      if(dat1$sourceTaxonName[j] %in% bee.names4$synonym){
        
        # If the species in Globi database does match any of the names in the bee.vector, then do this:
        
        # 1. Identify the row in the bee.names4 dataframe that the name matches
        row.num <- which(dat1$sourceTaxonName[j] == bee.names4$synonym)
        
        # 2. Replace the Globi name with the current.name
        dat1$sourceTaxonName[j] <- bee.names4$current.name[row.num]
        
      }
      
    }
    
  }
  
  
  

  # targetTaxonSpeciesName column
  
  # Skip if cell is NA
  if(is.na(dat1$targetTaxonSpeciesName[j]) == FALSE){
    
    # Skip if cell is blank
    if(dat1$targetTaxonSpeciesName[j] != ""){
      
      
      # Identify if the targetTaxonSpeciesName matches any of the names in the bee.vector
      if(dat1$targetTaxonSpeciesName[j] %in% bee.names4$synonym){
        
        # If the species in Globi database does match any of the names in the bee.vector, then do this:
        
        # 1. Identify the row in the bee.names4 dataframe that the name matches
        row.num <- which(dat1$targetTaxonSpeciesName[j] == bee.names4$synonym)
        
        # 2. Replace the Globi name with the current.name
        dat1$targetTaxonSpeciesName[j] <- bee.names4$current.name[row.num]
        
      }
      
    }
    
  }
    
    
    # targetTaxonName column
    
    # Skip if cell is NA
    if(is.na(dat1$targetTaxonName[j]) == FALSE){
      
      # Skip if cell is blank
      if(dat1$targetTaxonName[j] != ""){
        
        
        # Identify if the targetTaxonName matches any of the names in the bee.vector
        if(dat1$targetTaxonName[j] %in% bee.names4$synonym){
          
          # If the species in Globi database does match any of the names in the bee.vector, then do this
          
          # 1. Identify the row in the bee.names4 dataframe that the name matches
          row.num <- which(dat1$targetTaxonName[j] == bee.names4$synonym)
          
          # 2. Replace the Globi name with the current.name
          dat1$targetTaxonName[j] <- bee.names4$current.name[row.num]
          
        }
        
      }
    
    
  }
  
}









# 4. Make Insect & Plant columns -------------------------------------------------------






# Unique bee species
  # 140 species
bee.species <- bee.names3[,1]



# Unique plant species
  # 667 species
plant.species <- unique(plant.list$genus_species)


# Determine the number of cores available
no_cores <- detectCores() - 1 

# Register the cores
registerDoParallel(cores=no_cores)  

# Make the cores available for use
cl <- makeCluster(no_cores, type="FORK")  




# Create a function to check checklist names against globi names
check.names <- function(list.sp.names, input1, input2, input3, input4){
  
  # Check if any of the bee species are in the source or target positions
  check.source <- which(list.sp.names %in% input1  == TRUE |
                        list.sp.names %in% input2 == TRUE)
  check.target <- which(list.sp.names %in% input3 == TRUE |
                        list.sp.names %in% input4 == TRUE)
  
 return( 
   # If the check.source or check.target objects contain a value - then their length will be > 0; and it will return a 1
   if(length(check.source) + length(check.target) > 0){
    1
  } else { # Else it will return a 0
    0
  }
  )
  
}

# use foreach and %dopar% to parrallelize the computation

# We are going through each row in the Globi dataset (dat1) and determining if the BEE species names in Globi (in either the source or target positions) appears in our bee checklist
  # This takes a few minutes to run
bee.check <- foreach(i=1:nrow(dat1)) %dopar% check.names(
              list.sp.names = bee.species,
              input1 = dat1$sourceTaxonSpeciesName[i],
              input2 = dat1$sourceTaxonName[i],
              input3 = dat1$targetTaxonSpeciesName[i],
              input4 = dat1$targetTaxonName[i]) 

# Add the bee.check object as a column to dat1
# Create an empty column
dat1$Insect <- NA


# Then add the bee.check object to the new column
dat1[1:length(unlist(bee.check)),]$Insect <- c(unlist(bee.check))


# We are going through each row in the Globi dataset (dat1) and determining if the PLANT species names in Globi (in either the source or target positions) appears in our bee checklist
  # This takes a few minutes to run
plant.check <- foreach(i=1:nrow(dat1)) %dopar% check.names(list.sp.names = plant.species,
                                                         input1 = dat1$sourceTaxonSpeciesName[i],
                                                         input2 = dat1$sourceTaxonName[i],
                                                         input3 = dat1$targetTaxonSpeciesName[i],
                                                         input4 = dat1$targetTaxonName[i]) 

# Add the bee.check object as a column to dat1
# Create an empty column
dat1$Plant <- NA

# Then add the bee.check object to the new column
dat1[1:length(unlist(plant.check)),]$Plant <- c(unlist(plant.check))


# Sum the plant & insect columns
  # if it = 1; then, there was only 1 match (either bee or plant)
  # if it = 2; then, both bee and plant matched
  # if it = 0; then, there were no matches (bee and plant did NOT match)
dat1$tot <- dat1$Plant + dat1$Insect


# Determine the number of records that will be discarded
remove.rows <- length(which(dat1$tot < 2))

# How many rows will be kept?
  # 5,538
length(which(dat1$tot == 2))

# Save discarded rows as a csv file
write.csv(dat1[remove.rows,], "./Data/data_summary/discarded_rows_2021_12_28.csv")

# View(dat[,53:ncol(dat)])



# Remove rows with < 2 in the tot column
dat2 <- dat1[which(dat1$tot == 2),]

# Drop unused levels
dat2 <- droplevels(dat2)

# Save the rows with a complete bee-plant match
write.csv(dat2, "./Data/data_summary/globi_database_trimmed_KEEP_rows_2021_12_28.csv")


# look at citations
# View(table(dat2$sourceCitation))

# Number of observations
  #  5538
nrow(dat2)


head(dat2)





# 4. Make a map with the data point -------------------------------------------------------






# Look at the first few rows
head(dat2)


# Determine which rows do not have lat long data
dat2.5 <- dat2[is.na(dat2$decimalLatitude) == TRUE & is.na(dat2$decimalLongitude) == TRUE,]


# Write csv file
#write.csv(unique(dat2.5$sourceCitation), "./Data/entries_missing_lat_long_2021_05_14.csv")


# Remove rows with NA in Lat long
dat3 <- dat2[is.na(dat2$decimalLatitude) == FALSE & is.na(dat2$decimalLongitude) == FALSE,]

# Number of rows
  # 4,462
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

ggsave("./Data/data_summary/Globi_map_2021_12_28.pdf", height = 12, width = 15)


# Create a list of unique species names
# write.csv(sort(c(as.character(unique(dat3$sourceTaxonName)), as.character(unique(dat3$targetTaxonName)))), file = "./Data/species_names_Globi_map.csv")



# Subset the data
  # Lat from 31 - 36
  # Long from -125 to -116



dat5 <- dat4[dat4$decimalLatitude > 30 & dat4$decimalLatitude < 36 &
           dat4$decimalLongitude > -150 & dat4$decimalLongitude < -116, ]

sum(dat5$total)
  # 511


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

ggsave("./Data/data_summary/Globi_CA_map_2021_12_28.pdf", height = 12, width = 15)



# Now subset the actual checklist database to the lat/long indicated here
dat6 <- dat3[dat3$decimalLatitude > 30 & dat3$decimalLatitude < 36 &
               dat3$decimalLongitude > -150 & dat3$decimalLongitude < -116, ]

# Number of observations
nrow(dat6)

# Create a list of unique species names
# write.csv(sort(c(as.character(unique(dat6$sourceTaxonName)), as.character(unique(dat6$targetTaxonName)))), file = "./Data/species_names_Globi_CA_map.csv")







# 5. Add a new column with the institution codes to replace SCAN citation -------------------------------------------------------



# We will be working with the following 2 dataframes in this section:
  # dat6 = the subsetted species & CA dataset
  # institution.codes = the name of the institutions

# To look at the data frames, use these functions:
  # View(institution.codes)
  # View(dat6)


# The institution codes can be found in this column of the Globi database:
  # sourceCatalogNumber

# We need to pull apart the alphabetical characters and the numeric characters
dat7 <- dat6 %>% 
  mutate(., fullcode = sub(" ","", sourceCatalogNumber)) %>% 
  mutate(., withdashcode = sub("_","", fullcode)) %>% 
  mutate(., no_characters = sub("-","", withdashcode)) %>% 
  separate(no_characters, 
           into = c("code", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )

# Remove dashes from the names in institution.codes object
inst.code <- institution.codes %>% 
              mutate(., nodash = sub("-","", code)) 

# Look to see how many names match between our code column and the inst.code$nodash
unique(dat7$code) %in% inst.code$nodash


# Write a file with the institution codes
# write.csv(unique(dat7$code), file = "./Data/data_summary/subset_data_inst_codes_2021 12 29.csv")





# Next, we need to create 1 column with either the institution code or the sourceCitation

# Copy over the sourceCitation column
dat7$citation <- as.character(dat7$sourceCitation)

# levels(dat7$sourceCitation)

for(i in 1:nrow(dat7)){
  
  # If the source citation == "Symbiota Collections of Arthropods Network (SCAN)", then do this:
  if(dat7$sourceCitation[i] == "Symbiota Collections of Arthropods Network (SCAN)"){
    
    # Copy over the institution code into the citation column
    dat7$citation[i] <- dat7$code[i]
    
  }
  
}


compare_cols <- data.frame(original_sourceCitation = as.character(dat7$sourceCitation),
           inst.code = as.character(dat7$code),
           new_column = as.character(dat7$citation)
)

# Write the compare_col data frame
# write.csv(compare_cols, "./Data/data_summary/compare-citation-columns 2021 12 29.csv")

# Look at the dataframe
# View(compare_cols)

# Write the unique citations
# write.csv(unique(dat7$citation), "./Data/data_summary/unique-citations- 2021 12 29.csv")






# 6. Format the date column  -------------------------------------------------------





# Format the date of observation
# eventDateUnixEpoch
dat7$eventDate <- as.POSIXct(dat7$eventDateUnixEpoch/1000, origin = "1970-01-01")


# Pull apart the info in the event date
# Year
# Month
# Day - this column also includes time - but I won't finish formatting this because we don't need this detail
dat8 <- as_tibble(dat7) %>% 
  mutate(year = str_split(eventDate, "-", n = 3, simplify = TRUE)[,1],
         month= str_split(eventDate, "-", n = 3, simplify = TRUE)[,2],
         day = str_split(eventDate, "-", n = 3, simplify = TRUE)[,3])




# 7. Determine which bee-plant interactions are possible & which are forbidden -------------------------------------------------------






# Need to remove rows without species name from the bee.phenology dataframe
bee.phenology <- bee.phenology[bee.phenology$scientificName %in% bee.list$genus_species_infra,]

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
