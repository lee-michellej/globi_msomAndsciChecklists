########################################
########################################
# This code was written by: G. V. DiRenzo & M. J. Lee
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code objective ##########
##################################


# To format the covariates of the Globi model




################################## 
########  Table of Contents ######
################################## 


# 1. Load libraries & set working directory
# 2. Load data
# 3. Make sure all species are in the same order
# 4. Summarize the bee covariates
# 5. Save the file


################################## 
################################## 
################################## 




# 1. Load libraries -------------------------------------------------------

# Load libraries
library(tidyverse)
library(plyr)





# 2. Load data -------------------------------------------------------






# Bee size data
size <- read.csv("./Data/Bee traits for checklist species - size.csv")

# Bee coloration data
color <- read.csv("./Data/Bee traits for checklist species - coloration_NEW.csv")[-1,]
  # First row = EXAMPLE = remove

# Bee sociality data
sociality <- read.csv("./Data/Bee traits for checklist species - sociality.csv")[-1,]
  # First row = EXAMPLE = remove


# Read in file with type of citation
citation.type <- read.csv("./Data/citation-list-type.csv")
citation.list <- read.csv("./Data/final-globi-citations-unique 2022 01 21.csv")








#--------------- This analysis was run WITHOUT Apis mellifora - here we are removing the species


# Remove Apis entry for each file
size <- size[size$Species != "Apis mellifera",]
color <- color[color$Species != "Apis mellifera",]
sociality <- sociality[sociality$Species != "Apis mellifera",]



# Upload the observed bee-plant data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_11 - short plant list - no apis.rds")
  # object name = bee.plant.date.cite
  # 4-D array
  

# Flower color
plant.covariates <- read.csv("./Data/short_noapis_resolvedplantsci_041122.csv")




################
################
################








# 3. Make sure all species are in the same order -------------------------------------------------------




# Save the order in which the bee species are listed
bee.sp.names <- rownames(bee.plant.date.cite)



# Save the order in which the bee species are listed
plant.sp.names <- colnames(bee.plant.date.cite)




# Reorder the bee species in the size dataframe
size <- size[which(size$Species %in% bee.sp.names),]

# Quick visual check to make sure they are reordered & paired correctly
#data.frame(sizeSP = size$Species,
#           order = bee.sp.names)


# Reorder the bee species in the color dataframe
color <- color[which(color$Species %in% bee.sp.names),]

# Quick visual check to make sure they are reordered & paired correctly
#data.frame(colSP = color$Species,
#           order = bee.sp.names)


# Reorder the bee species in the sociality dataframe
sociality <- sociality[which(sociality$Species %in% bee.sp.names),]

# Quick visual check to make sure they are reordered & paired correctly
#data.frame(socialitySP = sociality$Species,
#           order = bee.sp.names)


# Reorder the plant species in the plant.covariates dataframe
plant.covariates <- plant.covariates[which(plant.covariates$scientificName %in% plant.sp.names),]

# # Quick visual check to make sure they are reordered & paired correctly
# data.frame(plantSP = plant.covariates$scientificName,
#            order = plant.sp.names)
#









# 4. Summarize the bee covariates -------------------------------------------------------






# Summarize the bee covariates that you need into 1 file
bee.covariates <- data.frame(species = size[,1],
                             size = size[,2],
                             striped = apply(color[,c(7, 15)], 1, function(x)ifelse(sum(x) > 0, 1, 0)),
                             notStriped = apply(color[,c(7, 15)], 1, function(x)ifelse(sum(x) > 0, 0, 1)),
                             solitary = apply(sociality[,c(6, 7)], 1, function(x)ifelse(sum(x) > 0, 1, 0)),
                             notSolitary = apply(sociality[,c(6, 7)], 1, function(x)ifelse(sum(x) > 0, 0, 1))
                             )

# Replace value for ""
bee.covariates[bee.covariates$species == "Pseudopanurgus californicus", ]$size <- 6.35


## Replace N/A with the average size in each genus
bee.covariates <- bee.covariates %>%
                    separate(col = species,
                            sep = " ",
                            c("Genus", "Species"))

# Replace N/A with NA
bee.covariates$size <- ifelse(bee.covariates$size == "N/A", "NA", bee.covariates$size)
bee.covariates$size <- as.numeric(as.character(bee.covariates$size))

# Convert Genus to a factor
bee.covariates$Genus <- as.factor(bee.covariates$Genus)

# Calculate the average size by genus
mean.bee.size.genus <- ddply(.data = bee.covariates,
                        .variables = "Genus",
                        .fun = summarize,
                        mean.size = mean(size,na.rm = TRUE))


# Replace NA values with the average bee size per genus
for(i in 1:nrow(bee.covariates)){
  
  if(is.na(bee.covariates$size[i]) == TRUE){
    
    # Match the genus name between bee.covariate and mean.bee.size.genus
    genus.row <- which(bee.covariates$Genus[i] == mean.bee.size.genus$Genus)
    
    # Replace with average mean bee size by genus
    bee.covariates$size[i] <- mean.bee.size.genus$mean.size[genus.row]
    
    
  }
  
  
}



# Standardize size data & replace NAs
bee.covariates$size_std <- (bee.covariates$size - mean(bee.covariates$size, na.rm = TRUE))/sd (bee.covariates$size, na.rm = TRUE)


min(bee.covariates$size, na.rm = TRUE)

max(bee.covariates$size, na.rm = TRUE)


length(which(bee.covariates$size > 10))

length(bee.covariates$size)






# 5. Citation covariates -------------------------------------------------------





# 4 types of citations:
  # [1] "Aggregated Data"     
  # [2] "Collection Specimen" 
  # [3] "Literature"          
  # [4] "Observation"   


# Out list of citations
citation.list$x


# Vector with citation type
citation.type <- c("Collection Specimen", #[1] "American Museum of Natural History Invertebrate Zoology Collection"                                                                         
              "Collection Specimen", #[2] "Bee Biology and Systematics Laboratory"                                                                                                     
              "Observation" , #[3] "http://iNaturalist.org is a place where you can record what you see in nature, meet other nature lovers, and learn about the natural world."
              
              "Collection Specimen", #[4] "Museum of Southwestern Biology"                                                                                                             
              "Collection Specimen", #[5] "R. M. Bohart Museum of Entomology"                                                                                                          
              "Collection Specimen", #[6] "Santa Barbara Museum of Natural History Entomology Collection"                                                                              
              "Collection Specimen" #[7] "University of California Santa Barbara Invertebrate Zoology Collection"  
              )

citation.code <- ifelse(citation.type == "Observation", 1, 0)


citation.covariates <- data.frame(citation.name = c("American Museum of Natural History Invertebrate Zoology Collection"    ,                                                                     "Bee Biology and Systematics Laboratory",                                                                                                     "http://iNaturalist.org is a place where you can record what you see in nature, meet other nature lovers, and learn about the natural world.",
                                                    "Museum of Southwestern Biology",                                                                                                          "R. M. Bohart Museum of Entomology",                                                                             "Santa Barbara Museum of Natural History Entomology Collection",                                                                            "University of California Santa Barbara Invertebrate Zoology Collection"),
                                  citation.type = citation.type,
                                  citation.code = citation.code
)


citation.covariates






# 6. Plant covariates -------------------------------------------------------




# # Determine if plant family is confounded with being yellow or being a bowl
#plant.sum <- ddply(.data = plant.covariates,
#      .fun= summarize,
#      .variables = "Family",
#      sum.yellow = sum(yellow),
#      sum.bowl = sum(bowl))
#
#ggplot(data = plant.sum, aes(x = Family, y = sum.yellow))+
#  geom_point()
#





plant.covariates$aster <- ifelse(plant.covariates$Family == "Asteraceae", 1, 0)






# 5. Save the file -------------------------------------------------------



# Save all covaritates in a single object
covariates <- list(bee.covariates = bee.covariates,
                   plant.covariates = plant.covariates,
                   citation.covariates = citation.covariates)


save(covariates, file = "./Data/model_covariates - 2022 04 21 - no apis.rds")






# 6. Make some plots (to check for correlations) -------------------------------------------------------





ggplot(data = covariates$plant.covariates, aes(x = yellow, y = bowl))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Flower shape (1 = bowl)")+
  xlab("Flower color (1 = yellow)")

ggsave("./Figures/Covariate-relationship-1.pdf",
       height = 6, 
       width = 8)


ggplot(data = covariates$bee.covariates, aes(x = size, y = solitary))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Bee solitary (1 = yes)")+
  xlab("Bee size")

ggsave("./Figures/Covariate-relationship-2.pdf",
       height = 6, 
       width = 8)


ggplot(data = covariates$bee.covariates, aes(x = size, y = striped))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Bee strippiness (1 = yes)")+
  xlab("Bee size")

ggsave("./Figures/Covariate-relationship-3.pdf",
       height = 6, 
       width = 8)

ggplot(data = covariates$bee.covariates, aes(x = striped, y =  solitary))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Bee solitary  (1 = yes)")+
  xlab("Bee strippiness (1 = yes)")

ggsave("./Figures/Covariate-relationship-4.pdf",
       height = 6, 
       width = 8)


# End script

