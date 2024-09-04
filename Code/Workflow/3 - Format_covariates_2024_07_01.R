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



# Set working directory
setwd("Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/")



# Bee size data
size <- read.csv("./Data/Bee traits for checklist species - size.csv")

# Bee coloration data
color <- read.csv("./Data/Bee traits for checklist species - coloration_NEW.csv")[-1,]
  # First row = EXAMPLE = remove

# Bee sociality data
sociality <- read.csv("./Data/Bee traits for checklist species - sociality.csv")[-1,]
  # First row = EXAMPLE = remove


# Read in file with type of citation
citation.list <- read.csv("./Data/KS-sources.kept.2024.07.01.csv")


#View(citation.list)





#--------------- This analysis was run WITHOUT Apis mellifora - here we are removing the species


# Remove Apis entry for each file
size <- size[size$Species != "Apis mellifera",]
color <- color[color$Species != "Apis mellifera",]
sociality <- sociality[sociality$Species != "Apis mellifera",]



# Upload the observed bee-plant data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2024_07_11 - short plant list - no apis.rds")
  # object name = bee.plant.cite
  # 3-D array
  



# Read in the dat_info
load("./Data/dat_info_2024_07_11.rds")
  # object name = dat_info


# Flower color
plant.covariates <- read.csv("./Data/plant.phenology2.csv")




################
################
################








# 3. Make sure all species are in the same order -------------------------------------------------------




# Save the order in which the bee species are listed
bee.sp.names <- dat_info$bee.species
bee.species <- dat_info$bee.species



# Save the order in which the bee species are listed
plant.sp.names <- dat_info$plant.species




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

nrow(size)
nrow(color)
nrow(sociality)
nrow(plant.covariates)







# 4. Summarize the bee covariates -------------------------------------------------------



# Summarize the bee covariates that you need into 1 file
bee.covariates <- data.frame(species = size[,1],
                             size = size[,2],
                             
                             # Look at columns "StripeAbdomen" and "PatternedAbdomen" - if these > 0, then assign a 1; else = 0
                             striped = apply(color[,c(7, 15)], 1, function(x)ifelse(sum(x) > 0, 1, 0)),
                             notStriped = apply(color[,c(7, 15)], 1, function(x)ifelse(sum(x) > 0, 0, 1)),
                             
                             # Look at columns "Solitary" and "Parasitic" - if these > 0, then assign a 1; else = 0
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


# Now - we have to determine the type of reference it is in our sourceCitation list
citation.list$aggregated <- ifelse(citation.list$citationType == "Aggregated", 1, 0)
citation.list$literature <- ifelse(citation.list$citationType == "Literature", 1, 0)
citation.list$collection <- ifelse(citation.list$citationType == "Collection", 1, 0)
citation.list$observation <- ifelse(citation.list$citationType == "Observation", 1, 0)





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





# 5. Save the file -------------------------------------------------------



# Save all covariates in a single object
covariates <- list(bee.covariates = bee.covariates,
                   plant.covariates = plant.covariates,
                   citation.covariates = citation.list)


save(covariates, file = "./Data/model_covariates - 2024 07 11 - no apis.rds")







# 6. Make some plots (to check for correlations) -------------------------------------------------------




ggplot(data = covariates$plant.covariates, aes(x = yellow, y = bowl))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Flower shape (1 = bowl)")+
  xlab("Flower color (1 = yellow)")

#ggsave("./Figures/Covariate-relationship-1.pdf",
#       height = 6, 
#       width = 8)


ggplot(data = covariates$bee.covariates, aes(x = size, y = solitary))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Bee solitary (1 = yes)")+
  xlab("Bee size")

#ggsave("./Figures/Covariate-relationship-2.pdf",
#       height = 6, 
#       width = 8)


ggplot(data = covariates$bee.covariates, aes(x = size, y = striped))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Bee strippiness (1 = yes)")+
  xlab("Bee size")

# ggsave("./Figures/Covariate-relationship-3.pdf",
#        height = 6, 
#        width = 8)

ggplot(data = covariates$bee.covariates, aes(x = striped, y =  solitary))+
  geom_point()+
  geom_smooth()+
  theme_bw(17)+
  ylab("Bee solitary  (1 = yes)")+
  xlab("Bee strippiness (1 = yes)")

# ggsave("./Figures/Covariate-relationship-4.pdf",
#        height = 6, 
#        width = 8)



# 7. Check some sample sizes -------------------------------------------------------



# -- How many total observations are there for each bee species? Are some bees/genus over-represented in the dataset?

bee.samp <- data.frame(bee.species = bee.species,
           number_obs_per_bee = apply(bee.plant.cite, 1, sum)
           )

bee.samp$genus <- str_split_fixed(bee.samp$bee.species, n = 2, pattern = " ")[,1]


ggplot(bee.samp, aes(y = bee.species, x = number_obs_per_bee)) +
  geom_bar(stat = "identity")+ 
  theme_bw(6)+
  scale_x_continuous(trans = 'log10')+
  xlab("Total number of observations across plants + sourceCitations")+
  ylab("Bee species")

ggsave("./Figures/2024 07 11 - Bee-sample-size.png",
       height = 12, 
       width = 8)


#--- Make the plot by genus


ggplot(bee.samp, aes(y = bee.species, x = number_obs_per_bee)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~genus, scale = "free") +
  theme_bw(10)+
  scale_x_continuous(trans = 'log10')+
  xlab("Total number of observations across plants + sourceCitations")+
  ylab("Bee species")

ggsave("./Figures/2024 07 11 - Bee-sample-size-genus.png",
       height = 12, 
       width = 18)




#--- Remove sourceCitations - which bees have the most interactions with plants?

# Take the max across sourceCitations for each bee plant (i.e., if it was ever observed interacting, it receives a 1, if not then a 0)
bee_plant = apply(bee.plant.cite, c(1, 2), max)

bee_plant_sum <- data.frame(bee.species = bee.species,
                            number_plants_per_bee = apply(bee_plant, 1, sum)
)

bee_plant_sum$genus <- str_split_fixed(bee_plant_sum$bee.species, n = 2, pattern = " ")[,1]

ggplot(bee_plant_sum, aes(y = bee.species, x = number_plants_per_bee)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~genus, scale = "free") +
  theme_bw(10)+
  scale_x_continuous(trans = 'log10')+
  xlab("Total number of plant interactions")+
  ylab("Bee species")


ggsave("./Figures/2024 07 11 - Bee-plant-interactions.png",
       height = 12, 
       width = 18)






#----- Are bigger bees seen more often?


bee.samp$size <- bee.covariates$size


ggplot(data = bee.samp, aes(x = size, y = number_obs_per_bee))+
  geom_point()+
  geom_smooth(se = F)+
  theme_bw(17)+
  #scale_y_continuous(trans = "log10")+
  ylab("Total number of observations across plants + sourceCitations")+
  xlab("Bee size")

ggsave("./Figures/2024 07 11 - Bee-size_vs_obs.png",
       height = 8, 
       width = 10)



bee_plant_sum$size <- bee.covariates$size

ggplot(data = bee_plant_sum, aes(x = size, y = number_plants_per_bee))+
  geom_point()+
  geom_smooth(se = F)+
  theme_bw(17)+
  #scale_y_continuous(trans = "log10")+
  ylab("Total number of plant interactions")+
  xlab("Bee size")

ggsave("./Figures/2024 07 11 - Bee-size_vs_plants.png",
       height = 8, 
       width = 10)

#---- Are social bees seen more often?


bee.samp$solitary <- bee.covariates$solitary


ggplot(data = bee.samp, aes(x = as.factor(solitary), y = number_obs_per_bee))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw(17)+
  scale_y_continuous(trans = "log10")+
  ylab("Total number of observations across plants + sourceCitations")+
  xlab("Bee sociality")+
  scale_x_discrete(labels = c("Social", "Solitary"))

ggsave("./Figures/2024 07 11 - Bee-solitary_vs_obs.png",
       height = 8, 
       width = 10)



bee_plant_sum$solitary <- bee.covariates$solitary

ggplot(data = bee_plant_sum, aes(x = as.factor(solitary), y = number_plants_per_bee))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw(17)+
  scale_y_continuous(trans = "log10")+
  ylab("Total number of plant interactions")+
  xlab("Bee sociality")+
  scale_x_discrete(labels = c("Social", "Solitary"))

ggsave("./Figures/2024 07 11 - Bee-solitary_vs_plants.png",
       height = 8, 
       width = 10)




#------- Number of observations per bee-plant combination

bee.plant.total <- apply(bee.plant.cite, c(1,2), sum)

rownames(bee.plant.total) <- bee.sp.names

colnames(bee.plant.total) <- plant.sp.names

bee.plant.tot.long <- reshape2::melt(bee.plant.total)

colnames(bee.plant.tot.long) <- c("bee.name", "plant.name", "total")

ggplot(data = bee.plant.tot.long, aes(x = reorder(bee.name, total, decreasing = T), y = reorder(plant.name, total, decreasing = T), fill = total))+
  geom_tile()+
  viridis::scale_fill_viridis(discrete = FALSE)+
  theme_bw(4)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Bee species")+
  ylab("Plant species")

ggsave("./Figures/2024 07 11 - Bee-plant-heat-map.png",
       height = 15, 
       width = 10)




##------- Bee strippness


bee.samp$strips <- bee.covariates$striped


ggplot(data = bee.samp, aes(x = as.factor(strips), y = number_obs_per_bee))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw(17)+
  scale_y_continuous(trans = "log10")+
  ylab("Total number of observations across plants + sourceCitations")+
  xlab("Bee strippiness")+
  scale_x_discrete(labels = c("No strips", "Yes strips"))

ggsave("./Figures/2024 07 11 - Bee-strips_vs_obs.png",
       height = 8, 
       width = 10)



bee_plant_sum$strips <- bee.covariates$striped

ggplot(data = bee_plant_sum, aes(x = as.factor(strips), y = number_plants_per_bee))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw(17)+
  scale_y_continuous(trans = "log10")+
  ylab("Total number of plant interactions")+
  xlab("Bee strippiness")+
  scale_x_discrete(labels = c("No strips", "Yes strips"))

ggsave("./Figures/2024 07 11 - Bee-strips_vs_plants.png",
       height = 8, 
       width = 10)




#---- Exploring plant color

plant.samp <- data.frame(plant.species = plant.sp.names,
                         number_obs_per_plant = apply(bee.plant.cite, 2, sum)
)

bee_plant_sum <- data.frame(plant.sp.names = plant.sp.names,
                            number_bees_per_plant = apply(bee_plant, 2, sum)
)

# Add color covariate
plant.samp$color <- ifelse(plant.covariates$yellow == 1, "yellow", 
                           ifelse(plant.covariates$blue == 1, "blue", 
                                  ifelse(plant.covariates$white == 1, "white",
                                         "other")))


bee_plant_sum$color <- ifelse(plant.covariates$yellow == 1, "yellow", 
                           ifelse(plant.covariates$blue == 1, "blue", 
                                  ifelse(plant.covariates$white == 1, "white",
                                         "other")))


# These are the sample sizes recorded in the results section for the number of total bee observations across all sources for each flower color type
plant.samp[plant.samp$color == "blue",]

nrow(plant.samp[plant.samp$color == "yellow",])
  mean(plant.samp[plant.samp$color == "yellow",]$number_obs_per_plant)
  sd(plant.samp[plant.samp$color == "yellow",]$number_obs_per_plant)
  min(plant.samp[plant.samp$color == "yellow",]$number_obs_per_plant)
  max(plant.samp[plant.samp$color == "yellow",]$number_obs_per_plant)
  

nrow(plant.samp[plant.samp$color == "white",])
  mean(plant.samp[plant.samp$color == "white",]$number_obs_per_plant)
  sd(plant.samp[plant.samp$color == "white",]$number_obs_per_plant)
  min(plant.samp[plant.samp$color == "white",]$number_obs_per_plant)
  max(plant.samp[plant.samp$color == "white",]$number_obs_per_plant)


nrow(plant.samp[plant.samp$color == "other",])
  mean(plant.samp[plant.samp$color == "other",]$number_obs_per_plant)
  sd(plant.samp[plant.samp$color == "other",]$number_obs_per_plant)
  min(plant.samp[plant.samp$color == "other",]$number_obs_per_plant)
  max(plant.samp[plant.samp$color == "other",]$number_obs_per_plant)


ggplot(data = plant.samp, aes(x = as.factor(color), y = number_obs_per_plant))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw(17)+
  scale_y_continuous(trans = "log10")+
  ylab("Total number of observations across bees + sourceCitations")+
  xlab("Plant color")

ggsave("./Figures/2024 07 11 - Plant color_vs_obs.png",
       height = 8, 
       width = 10)


ggplot(data = bee_plant_sum, aes(x = as.factor(color), y = number_bees_per_plant))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw(17)+
  scale_y_continuous(trans = "log10")+
  ylab("Total number of bee interactions")+
  xlab("Plant color")

ggsave("./Figures/2024 07 11 - Plant color_vs_bees.png",
       height = 8, 
       width = 10)

# 





# 7. Calculate some species richness metrics -------------------------------------------------------


# collapse across plants to calculate bee species richness by source citation type
bee_cite <- as.data.frame(apply(bee.plant.cite, c(1, 3), max))

# Total number of species by source citation
colSums(bee_cite)

colnames(bee_cite) <- dat_info$citations

bee_cite$bee.species <- dat_info$bee.species

# add solitary bee info
bee_cite$solitary <- c(unlist(bee.covariates$solitary))

bee_cite_long <- reshape2::melt(bee_cite, id.vars = c("solitary"))

View(bee_cite_long)

sp_rich_solitary_cite <- bee_cite_long %>% 
  group_by(variable, solitary) %>% 
  dplyr::summarize(total_sp = sum(as.numeric(value), na.rm = TRUE))


ggplot(data = sp_rich_solitary_cite, aes(y = total_sp, 
                                         x = variable,
                                         fill = as.factor(solitary)))+
  geom_bar(stat = "identity", position="dodge")+
  ylab("Total bee species richness")+
  xlab("Citation") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  

ggsave("./Figures/2024 07 11 - Citation - solitary - richness.png",
       height = 8, 
       width = 10)




# 7. Save the sample sizes you've calculated -------------------------------------------------------



length(which(obs_dat$bee_plant_sum$number_plants_per_bee == 0))



# Save the number of observation into a list and then as a R object

obs_dat <- list(bee.samp = bee.samp,
                bee_plant_sum = bee_plant_sum)


save(obs_dat, file = "./Data/obs_dat.rds")

# End script

