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
  
# Databases are plagued by 3 problems:
  # 1. taxonomic sampling bias
      # Particular species may be sampled more frequently than others because more is known about them or inference is desired on that species
  # 2. spatial sampling bias
      # Particular areas are easier to reach or sample
  # 2. detection bias
      # Species detectability changes over time and space as a result of observers or number of surveys
      # Number of observers, quality of observers, length of survey, survey conditions

# As a result, patterns may be masked (or there are false patterns) because of observation effort

# In an effort to safeguard against some sources of biases, we use bee and plant phenology data to code "forbidden links"
  # For example, if a bee is not active or a plant is not flowering while the other is active/flowering - then this interaction could never occur



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
library(lubridate)

# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Load data -------------------------------------------------------




# Read in data
  # Note that this is not in the github repo because the file size is too big
  # This data was downloaded from globi
dat <- read.csv("~/Desktop/Folder/Data_globi/all_bee_data_unique.csv")
  # Not updated

# Read in the bee checklist
bee.list <- read_tsv("~/Dropbox/Globi/Data/bees-SCI_2021_09_14.tsv")
  # Updated 27 Sept 2021

# Read in bee phenology data
bee.phenology <- read_tsv("~/Dropbox/Globi/Data/All-California-phenologyTable.tsv")

# Read in the plant checklist
plant.list <- read.table("~/Dropbox/Globi/Data/plants-SCI_traits_ml_2021_09_14.txt", header = T, sep = "\t")
  # Updated 27 Sept 2021

# Read in plant phenology data
plant.phenology <- read.table("~/Dropbox/Globi/Data/plants-SCI_traitsfull_ml_2021_09_14.txt", header = T, sep = "\t")

# Read in file with type of citation
citation <- read.csv("./Data/citation-list-type.csv")


# Other bee names
bee.names <- read.csv("./Data/Checklist names and synonyms - Author-less.csv")


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
# colnames(bee.list)[4] <- "genus_species"
# We are creating a new column to remove any of the extrenuous info
bee.list$Genus_sp <- paste(bee.list$Genus, bee.list$Species, sep = " ")

# Plant list already has a column with full scientific name
# colnames(plant.list)[4] <- "genus_species"
plant.list$Genus_sp <- paste(plant.list$Genus, plant.list$Species, sep = " ")





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
  bee_present_match[i] <- bee.list$Genus_sp[i] %in% globi.sp
  
  # Regular expressions
  bee_present_grep[i] <- length(grep(bee.list$Genus_sp[i], globi.sp))
  
}

for(i in 1:nrow(plant.list)){
  
  # Matching
  plant_present_match[i] <- plant.list$Genus_sp[i] %in% globi.sp
  
  # Regular expressions
  plant_present_grep[i] <- length(grep(plant.list$Genus_sp[i], globi.sp))
  
}


# Replace anything with a > 1 value with a 1
bee_present_grep[bee_present_grep > 1] <- 1
plant_present_grep[plant_present_grep > 1] <- 1


# How many species matched using regular expressions?
  # 91
sum(bee_present_grep)
  # 77
sum(plant_present_grep)


# How many species matched using matching?
  # 91
sum(bee_present_match)
  # 77
sum(plant_present_match)


# As a reminder there are:
  # 142 bee species in our checklist
  # 584 plant species

# Which species were not found with matching?
  # The ones without a species name
    # Calliopsis
    # Sphecodes
    # Stelis


# Now, we will add species synonymns to the list
View(bee.names)


# Pull apart the names ;
View(str_split(bee.names$Synonyms.from.DL, ";", simplify = TRUE))

# Then convert from wide to long format

BN <- bee.names %>%
  mutate(syn.names.1 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,1],
         syn.names.2 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,2],
         syn.names.3 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,3],
         syn.names.4 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,4],
         syn.names.5 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,5],
         syn.names.6 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,6],
         syn.names.7 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,7],
         syn.names.8 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,8],
         syn.names.9 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,9],
         syn.names.10 = str_split(Synonyms.from.DL, "; ", simplify = TRUE)[,10]) %>% 
  pivot_longer(cols = syn.names.1:syn.names.10,
               names_to = "syn.names") %>%
  mutate(syn.number = str_split(syn.names, "syn.names.", simplify = TRUE)[,2]) %>%
  select(Checklist.names, syn.number, value)%>%
  filter(value != "") %>%
  mutate(value = str_trim(value))



View(BN)




# How many of the bee.list species are not in the Globi database?
# We will test with matching & regular expressions
bee_present_match_syn <- bee_present_grep_syn <- numeric(nrow(bee.names))


# Use a loop to go through each genus_species and match with the globi species list
for(i in 1:nrow(BN)){
  
  # Matching
  bee_present_match_syn[i] <- BN$value[i] %in% globi.sp
  
  # Regular expressions
  bee_present_grep_syn[i] <- length(grep(BN$value[i], globi.sp))
  
}


# Replace anything with a > 1 value with a 1
bee_present_grep_syn[bee_present_grep_syn > 1] <- 1


# How many species matched?
sum(bee_present_grep_syn)
  # 9
sum(bee_present_match_syn)
  # 8



# Which species did not match anything in Globi?
bee.finder <- data.frame(genus_species = bee.list$Genus_sp,
               match = bee_present_match, 
               grep = bee_present_grep)


plant.finder <- data.frame(genus_species = plant.list$Genus_sp,
                match = plant_present_match, 
                grep = plant_present_grep)

# Write the files 

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




# 4. Make Insect & Plant columns -------------------------------------------------------



# Unique bee species
  # 142 species
bee.species <- unique(bee.list$Genus_sp)


# Unique plant species
  # 584 species
plant.species <- unique(plant.list$Genus_sp)


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

# Add the plant.check object as a column to dat1
# Create an empty column
dat1$Plant <- NA

# Then add the plant.check object to the new column
dat1[1:length(unlist(plant.check)),]$Plant <- c(unlist(plant.check))


# Sum the plant & insect columns
  # if it = 1; then, there was only 1 match (either bee or plant)
  # if it = 2; then, both bee and plant matched
  # if it = 0; then, there were no matches (bee and plant did NOT match)
dat1$tot <- dat1$Plant + dat1$Insect

# How many rows had 1 match?
length(which(dat1$tot == 1))
  # 38,666

# How many rows had 0 matches?
length(which(dat1$tot == 0))
  # 214,886

# How many rows had 2 matches?
length(which(dat1$tot == 2))
  # 5,583



# Determine the number of records that will be discarded
remove.rows <- length(which(dat1$tot < 2))

# Save discarded rows as a csv file
# write.csv(dat1[remove.rows,], "./Data/discarded_rows_2021_05_14.csv")

# Remove rows with < 2 in the tot column
dat2 <- dat1[which(dat1$tot == 2),]

# Drop unused levels
dat2 <- droplevels(dat2)

# Save the rows with a complete bee-plant match
# write.csv(dat2, "./Data/matched_rows_2021_09_13.csv")



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

# ggsave("./Figures/Globi_map_2021_09_13.pdf", height = 12, width = 15)


# Create a list of unique species names
# write.csv(sort(c(as.character(unique(dat3$sourceTaxonName)), as.character(unique(dat3$targetTaxonName)))), file = "./Data/species_names_Globi_map.csv")



# Subset the data
  # Lat from 31 - 36
  # Long from -125 to -116
dat5 <- dat4[dat4$decimalLatitude > 30 & dat4$decimalLatitude < 36 &
           dat4$decimalLongitude > -150 & dat4$decimalLongitude < -116, ]

sum(dat5$total)
  # 518


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

# ggsave("./Figures/Globi_CA_map_2021_09_13.pdf", height = 12, width = 15)



# Now subset the actual checklist database to the lat/long indicated here
dat6 <- dat3[dat3$decimalLatitude > 30 & dat3$decimalLatitude < 36 &
               dat3$decimalLongitude > -150 & dat3$decimalLongitude < -116, ]

# Number of observations
nrow(dat6)
  # 517

# Create a list of unique species names
# write.csv(sort(c(as.character(unique(dat6$sourceTaxonName)), as.character(unique(dat6$targetTaxonName)))), file = "./Data/species_names_Globi_CA_map.csv")





# 5. Summarize the total number of plants each bee interacts with in each study -------------------------------------------------------




# Now, we need to make the wide formatted data
  # Along the rows = each bee genus
  # Along the columns = each study
  # In each cell = the number of unique plant-bee interactions

# Drop unused levels
dat6 <- droplevels(dat6)

# For the paper by Seltman et al., we will be using the info in the referenceCitation column. For all others, we will use the sourceCitation column
dat6$lit <- ifelse(dat6$sourceCitation ==  unique(dat6$sourceCitation)[4], 
                   as.character(dat6$referenceCitation),
                   as.character(dat6$sourceCitation))

# For the "Symbiota Collections of Arthropods Network (SCAN)" citation, we need to replace with the insitiuion codes
# But first, we need to pull apart the column with the institution codes, and then replace them with the actual names



  #######  ********* Check to make sure you are using the right column for the instution codes   #######  *********   #######  ********* 



dat6 <- dat6 %>%
  mutate(inst.code = str_split(sourceTaxonId, ":", simplify = TRUE)[,1])


# Now, with the institution codes seperated, we can replace the "SCAN" sourceCitation in the lit column with the instituion code
dat6$lit <- ifelse(dat6$lit == unique(dat6$lit)[2] ,
                    as.character(dat6$inst.code),
                    as.character(dat6$lit))

# Create a citation list object
cit.list <- unique(dat6$lit)

# Number of unique collections/citations
length(cit.list)

# Write csv file
# write.csv(cit.list, "./Data/final_literature_cited_list_2021_05_14.csv")



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
  # 7,783,872
length(bee.species) *
length(plant.species)* 
  12 * 
length(cit.list)


# Is the bee phenology data (bee.phenology) in the same order as the list of bee species?
  # YES - alphabetical order
# View(cbind(bee.phenology$scientificName, rownames(bee.plant.date.cite)))

# Add a new Genus species column to the plant phenology data
plant.phenology$Genus_species <- paste(plant.phenology$Genus, 
                                       plant.phenology$Species, 
                                       sep = " ")

# Is the bee phenology data (bee.phenology) in the same order as the list of bee species?
  # NO - different orders
 View(cbind(re.prdered$Genus_species, colnames(bee.plant.date.cite)))

re.prdered <- plant.phenology[match(colnames(bee.plant.date.cite), plant.phenology$Genus_species),]
 
# Try reording the plants according to how they appear in your bee.plant.date.cite
 
 
 
# Month counter
bee.plant.inter <- data.frame(beeID = NA,
                              plantID = NA,
                              monthID = NA)
 
bee.plant.forbid <- data.frame(beeID = NA,
                              plantID = NA,
                              monthID = NA)
a <- 1
b <- 1

for(i in 1:nrow(bee.plant.date.cite)){ # For each bee species
  for(j in 1:ncol(bee.plant.date.cite)){ # For each plant species
   
    for(k in 1:12){ # for each month
      
      # Work through each species and month
        # If the value = 0, then the species doesn't interact with ANY plants that month
 #   if(bee.phenology[i, k+1] == 0){
 #     bee.plant.date.cite[i, , k, ] <- -9999
 #     
 #   }
    
      # Possible interactions
      if(is.na(plant.phenology[j, k + 26]) == FALSE){
        if(bee.phenology[i, k+1] == 1 & 
           plant.phenology[j, k + 26] == 1){
          
          bee.plant.inter[a,1] <- i
          bee.plant.inter[a,2] <- j
          bee.plant.inter[a,3] <- k
          
          a <- a + 1
        }
      }
      
      # Forbidden links
      if(is.na(plant.phenology[j, k + 26]) == FALSE){
        if(bee.phenology[i, k+1] == 0 |
           plant.phenology[j, k + 26] == 0){
          
          bee.plant.forbid[b,1] <- i
          bee.plant.forbid[b,2] <- j
          bee.plant.forbid[b,3] <- k
          
          b <- b + 1
        }
      }
      
    }
  }
}

# Plant species 71 = all NA 
plant.phenology[j, 1:12 + 26]



# Now we will fill in the 3D array
for(i in 1:nrow(dat6)){
  
# Determine which citation
cit.pos <- which(cit.list %in% dat6$sourceCitation[i] == TRUE)

# Determine which bee
bee.pos <- which(bee.species %in% dat6$sourceTaxonSpeciesName[i]  == TRUE |
                 bee.species %in% dat6$sourceTaxonName[i] == TRUE |
                 bee.species %in% dat6$targetTaxonSpeciesName[i] == TRUE |
                 bee.species %in% dat6$targetTaxonName[i] == TRUE )

# Determine which plant
plant.pos <- which(plant.species %in% dat6$sourceTaxonSpeciesName[i]  == TRUE |
                   plant.species %in% dat6$sourceTaxonName[i] == TRUE |
                   plant.species %in% dat6$targetTaxonSpeciesName[i] == TRUE |
                   plant.species %in% dat6$targetTaxonName[i] == TRUE )

# Add a 1
bee.plant.cite[bee.pos, plant.pos, cit.pos] <- 1

}


# Look at the dimensions
dim(bee.plant.cite)
  # 142 bee species
  # 667 plant species
  # 6 citation sources


# Remove columns (which correspond to plants) that do not 
# Determine which columns have at least 1 entry and match those plant names with the column names in bee.plant.cite
cols.keep <- colnames(bee.plant.cite) %in%
  names(which(apply(bee.plant.cite, 2, sum) > 0)) 

# Keep these columns
bee.plant.cite2 <- bee.plant.cite[, cols.keep, ]

# List of plant names removed
write.csv(colnames(bee.plant.cite)[cols.keep == FALSE],
          "./Data/plant_species_removed_no_interaction_2021_09_13.csv")

# Look at the dimensions
dim(bee.plant.cite2)
  # 142, 101, 6



# 6. Save the data -------------------------------------------------------


save(bee.plant.cite2, file= "./Data/globi_data_formatted_bee_plant_2021_09_13.rds")

write.csv(dat6, file= "./Data/globi_data_subset_bee_plant_2021_09_13.csv")




# Still to do
  # Determine which column has month/time info
  # Add NA to bee-plant interaction when the collector went in the wrong month


# End script
