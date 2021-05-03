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
# Observations are obtained from museum collections and research studies worldwide.
# Studies vary in terms of objectives, study design, etc.
# The Globi database consists of presence-only data.
  # We subsetted the data to only include bee genera and instances of bees interacting with plants (either as the "source" or "target").
  # We aggregate the presence-only data to count data = the total number of plants that each bee genera is interacting with
    # Problem - if 1 study documents 1 plant-bee interaction, and another study also documents 1 plant-bee interaction; you don't know if it is the same plant-bee interaction that was documented
  # By doing this, we loss the identity of the plant species that the bee is interacting with.

# We do not explicitly consider space in the model 
  # Data are collected worldwide across time, and these are records of any possible plant-bee interaction anywhere

# Databases are plagued by 2 problems:
  # 1. sampling bias
      # Particular species may be sampled more frequently than others because more is known about them or inference is desired on that species
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

# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Load data -------------------------------------------------------




# Read in data
  # Note that this is not in the github repo because the file size is too big
  # This data was downloaded from globi
dat <- read.csv("~/Desktop/Data_globi/all_bee_data_unique.csv")



# Read in the bee checklist
bee.list <- read_tsv("./Data/Checklist_SCI_bees.tsv")

# Read in the bee checklist
plant.list <- read_tsv("./Data/Checklist_SCI_plants.tsv")


# Read in file with type of citation
citation <- read.csv("./Data/citation-list-type.csv")


# Look at data structure
str(dat)
  # Look at globi for column definitions

str(bee.list)
  # $ family: Bee family 
  # $ genus : Bee genus
  # $ specificEpithet: Bee species name
  # $ infraSpecificEpithet
  # $ ScientificName : Bee scientific name


str(plant.list)
  # $ family: Plant family 
  # $ genus : Plant genus
  # $ specificEpithet: Plant species name
  # $ infraSpecificEpithet
  # $ ScientificName : Plant scientific name






# 3. Format checklist info a little -------------------------------------------------------




# Replace NA's with sp. in bee list
bee.list$specificEpithet <- ifelse(is.na(bee.list$specificEpithet), 
                                   "",
                                   bee.list$specificEpithet)


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



# Replace NA's with sp. in plant list
plant.list$specificEpithet <- ifelse(is.na(plant.list$specificEpithet), 
                                   "",
                                   plant.list$specificEpithet)


# Make a plant genus species column with infraSpecificEpithet
plant.list$genus_species <- ifelse(plant.list$specificEpithet == "",
                                   plant.list$genus,
                                  paste(plant.list$genus, 
                                        plant.list$specificEpithet))

plant.list$genus_species_infra <- ifelse(is.na(plant.list$infraSpecificEpithet), 
                                 paste(plant.list$genus, 
                                       plant.list$specificEpithet),
                                 paste(plant.list$genus, 
                                       plant.list$specificEpithet, 
                                       plant.list$infraSpecificEpithet))






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


# Create a list with all of the unique species in Globi - 
  # Plant & Bee
globi.sp <- unique(unique(dat1$sourceTaxonSpeciesName), 
                     unique(dat1$sourceTaxonName),
                     unique(dat1$targetTaxonSpeciesName),
                     unique(dat1$targetTaxonName))

# Number of unique species
  # 3,035
length(globi.sp)

# How many of the bee.list species are not in the Globi database?
  # We will test with matching & regular expressions
bee_present_match <- bee_present_grep <- numeric(nrow(bee.list))
plant_present_match <- plant_present_grep <- numeric(nrow(plant.list))

# Use a loop to go through each genus_species and match with the globi species list
for(i in 1:nrow(bee.list)){
  
  # Matching
  bee_present_match[i] <- bee.list$genus_species[i] %in% globi.sp
  
  # Regular expressions
  bee_present_grep[i] <- length(grep(bee.list$genus_species[i], globi.sp))
  
}

for(i in 1:nrow(plant.list)){
  
  # Matching
  plant_present_match[i] <- plant.list$genus_species[i] %in% globi.sp
  
  # Regular expressions
  plant_present_grep[i] <- length(grep(plant.list$genus_species[i], globi.sp))
  
}


# Replace anything with a > 1 value with a 1
bee_present_grep[bee_present_grep > 1] <- 1
plant_present_grep[plant_present_grep > 1] <- 1


# How many species matched using regular expressions?
  # 82
sum(bee_present_grep)
  # 437
sum(plant_present_grep)


# How many species matched using matching?
  # 79
sum(bee_present_match)
  # 129
sum(plant_present_match)


# As a reminder there are:
  # 120 bee species in our checklist
  # 1,869 plant species

# Which species were not found with matching?
  # The ones without a species name
    # Calliopsis
    # Sphecodes
    # Stelis

# Which species did not match anything in Globi?
bee.finder <- data.frame(genus_species = bee.list$genus_species,
               match = bee_present_match, 
               grep = bee_present_grep)


plant.finder <- data.frame(genus_species = plant.list$genus_species,
                match = plant_present_match, 
                grep = plant_present_grep)

# remove NA rows
plant.finder <- plant.finder[is.na(plant.finder$genus_species) == FALSE,]

# write.csv(bee.finder,
#           file = "./Data/globi-bee-matches.csv")
#
# write.csv(plant.finder,
#           file = "./Data/globi-plant-matches.csv")
# 
# globi.sp <- sort(globi.sp)
#
# write.csv(globi.sp,
#           file = "./Data/globi-species-list.csv")
# 

# Other code to explore

# write.csv(data.frame(sp.names = sort(unique(dat1$targetTaxonSpeciesName))),
#                     file = "./Data/globi-targetTaxonSpeciesNames.csv")
# 
# bee.list$genus_species[2] %in% dat1$targetTaxonSpeciesName
# 
# grep(bee.list$genus_species[4], dat1$targetTaxonSpeciesName)
# 
# View(dat1[grep("vandykei", dat1$targetTaxonName),])
# 
# View(data.frame(spNames = dat1$sourceTaxonSpeciesName,
#                 taxonNames = dat1$sourceTaxonName))
# 
# "Pseudopanugus" %in% bee.list$genus




# 4. Make Insect & Plant columns -------------------------------------------------------



# Unique bee species
  # 120 species
bee.species <- unique(bee.list$genus_species)


# Unique plant species
  # 1,186 species
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

# We are going through each row and determining if the bee species names appears in the source or target positions
  # This takes a few minutes to run
bee.check <- foreach(i=1:nrow(dat1)) %dopar% check.names(
              list.sp.names = bee.species,
              input1 = dat1$sourceTaxonSpeciesName[i],
              input2 = dat1$sourceTaxonName[i],
              input3 = dat1$targetTaxonSpeciesName[i],
              input4 = dat1$targetTaxonName[i]) 

# Add the bee.check object as a column to dat1
dat1$Insect <- NA
dat1[1:length(unlist(bee.check)),]$Insect <- c(unlist(bee.check))


# We are going through each row and determining if the plant species names appears in the source or target positions
  # This takes a few minutes to run
plant.check <- foreach(i=1:nrow(dat1)) %dopar% check.names(list.sp.names = plant.species,
                                                         input1 = dat1$sourceTaxonSpeciesName[i],
                                                         input2 = dat1$sourceTaxonName[i],
                                                         input3 = dat1$targetTaxonSpeciesName[i],
                                                         input4 = dat1$targetTaxonName[i]) 

# Add the bee.check object as a column to dat1
dat1$Plant <- NA
dat1[1:length(unlist(plant.check)),]$Plant <- c(unlist(plant.check))


# Sum the plant & insect columns
  # if it = 1; then, there was only 1 match (either bee or plant)
  # if it = 2; then, both bee and plant matched
  # if it = 0; then, there were no matches (bee and plant did NOT match)
dat1$tot <- dat1$Plant + dat1$Insect


# Determine the number of records that will be discarded
remove.rows <- length(which(dat1$tot < 2))

# How many rows will be kept
length(which(dat1$tot == 2))

# Save discarded rows as a csv file
# write.csv(dat1[remove.rows,], "./Data/discarded_rows_2021_04_05.csv")




# Remove rows with < 2 in the tot column
dat2 <- dat1[which(dat1$tot == 2),]

# Drop unused levels
dat2 <- droplevels(dat2)

# Save the rows with a complete bee-plant match
write.csv(dat2, "./Data/matched_rows_2021_05_03.csv")


# look at citations
# View(table(dat2$sourceCitation))

# Number of observations
  # 8,768
nrow(dat2)


head(dat2)



# 4. Make a map with the data point -------------------------------------------------------




# Look at the first few rows
head(dat2)


# Remove rows with NA in Lat long
dat3 <- dat2[is.na(dat2$decimalLatitude) == FALSE & is.na(dat2$decimalLongitude) == FALSE,]

# Number of rows
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

ggsave("./Figures/Globi_map_2021_05_03.pdf", height = 12, width = 15)






# 5. Summarize the total number of plants each bee interacts with in each study -------------------------------------------------------



# Now, we need to make the wide formatted data
  # Along the rows = each bee genus
  # Along the columns = each study
  # In each cell = the number of unique plant-bee interactions

# Create a citation list object
cit.list <- unique(dat2$sourceCitation)


# Create the array that will be filled in
bee.plant.cite <- array(0, dim = c(length(bee.species),
                                   length(plant.species),
                                   length(cit.list)))

# Add row names
rownames(bee.plant.cite) <- bee.species

# Add column names
colnames(bee.plant.cite) <- plant.species

# Add sheet names
dimnames(bee.plant.cite)[[3]] <- cit.list

# Total number of interactions by citations
  # 3,273,360
length(bee.species) *
length(plant.species)* 
length(cit.list)

# Now we will fill in the 3D array
for(i in 1:nrow(dat2)){
  
# Determine which citation
cit.pos <- which(cit.list %in% dat2$sourceCitation[i] == TRUE)

# Determine which bee
bee.pos <- which(bee.species %in% dat2$sourceTaxonSpeciesName[i]  == TRUE |
                 bee.species %in% dat2$sourceTaxonName[i] == TRUE |
                 bee.species %in% dat2$targetTaxonSpeciesName[i] == TRUE |
                 bee.species %in% dat2$targetTaxonName[i] == TRUE )

# Determine which plant
plant.pos <- which(plant.species %in% dat2$sourceTaxonSpeciesName[i]  == TRUE |
                   plant.species %in% dat2$sourceTaxonName[i] == TRUE |
                   plant.species %in% dat2$targetTaxonSpeciesName[i] == TRUE |
                   plant.species %in% dat2$targetTaxonName[i] == TRUE )

# Add a 1
bee.plant.cite[bee.pos, plant.pos, cit.pos] <- 1

}


# Look at the dimensions
dim(bee.plant.cite)



# 6. Save the data -------------------------------------------------------


save(bee.plant.cite, file= "./Data/globi_data_formatted_bee_plant_2021_04_05.rds")

write.csv(bee.plant.cite, file= "./Data/globi_data_formatted_bee_plant_2021_04_05.csv")



# End script
