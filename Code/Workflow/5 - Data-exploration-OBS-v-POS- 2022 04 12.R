#######################################
#######################################
## Author: Dr. Graziella DiRenzo
##
## Date Created: 2022-04-11
##
## Copyright (c) Graziella DiRenzo, 2022
## Email: gdirenzo@umass.edu
#######################################
#######################################


#######################################
## Code objectives:
#######################################


# To visulalize the raw Globi data
  # What is possible vs what was observed



#######################################
## Output of code:
#######################################




#######################################
############ Table of Contents ########
#######################################


# 1. Load libraries & set working directory
# 2. Load data


#######################################
#######################################
#######################################




# 1. Load libraries & set working directory -------------------------------------------------------



# Load libraries
library(reshape2)
library(plyr)
library(ggplot2)
library(PerformanceAnalytics)


# Set working directory
setwd("~/")





# 2. Load data -------------------------------------------------------






# Load data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2023_02_23 - short plant list - no apis.rds")
  # object = bee.plant.date.cite




# Load covariate data
load("./Data/model_covariates - 2022 04 21 - no apis.rds")
  # object = covariates






# 3. Summarize the data & visualize -------------------------------------------------------





# -	How to visualize the relationship
    # o	Source by bee size by number of detections


dim(bee.plant.date.cite)
  # collapse across months


# Take the max (0 or 1) across months - if the bee/plant combination was EVER observed by a source
bee.plant.cite <- apply(bee.plant.date.cite, c(1, 2, 4), max, na.rm = TRUE) 
bee.plant.cite[bee.plant.cite == "-Inf"] <- NA


# Convert to long format
bee.plant.cite.long <- melt(bee.plant.cite)

# Add column names
colnames(bee.plant.cite.long) <- c("beeID", "plantID", "sourceID", "detection")

# Remove the NA rows - those aren't possible
bee.plant.cite.long <- bee.plant.cite.long[is.na(bee.plant.cite.long$detection) == FALSE,]


# Merge bee.plant.cite.long with covariates

# First format the bee.covariate data
covariates$bee.covariates$beeID <- paste(covariates$bee.covariates$Genus, covariates$bee.covariates$Species, sep = " ")


# Merge
# Bee covariates
bee.plant.cite.long.bee.cov <- merge(bee.plant.cite.long, covariates$bee.covariates, by = "beeID")

# Plant covariates
colnames(covariates$plant.covariates)[which(colnames(covariates$plant.covariates) == "resolvedPlantNames")] <- "plantID"
bee.plant.cite.long.cov <- merge(bee.plant.cite.long.bee.cov, covariates$plant.covariates, by = "plantID")


# Look at first 6 rows
head(bee.plant.cite.long.cov)

# Add a column to acknowledge the possible bee-plant interactions
bee.plant.cite.long.cov$possible <- 1


# Add up the number of bee-plant observations by source by bee size
det.source.size <- ddply(.data = bee.plant.cite.long.cov,
                         .variables = c("sourceID", "size_std"),
                         .fun = summarize,
                         sum.det = sum(detection),
                         sum.pos = sum(possible)
                         )


det.source.yellow <- ddply(.data = bee.plant.cite.long.cov,
                         .variables = c("sourceID", "yellow"),
                         .fun = summarize,
                         sum.det = sum(detection),
                         sum.pos = sum(possible)
)


det.source.bowl <- ddply(.data = bee.plant.cite.long.cov,
                           .variables = c("sourceID", "bowl"),
                           .fun = summarize,
                           sum.det = sum(detection),
                           sum.pos = sum(possible)
)

# Change the names of sourceID
levels(det.source.size$sourceID)

det.source.size$sourceID <- as.character(det.source.size$sourceID)


det.source.size$sourceID <- ifelse(det.source.size$sourceID == "American Museum of Natural History Invertebrate Zoology Collection", "AMNH InvertZoo Coll", 
                                   ifelse(det.source.size$sourceID == "Bee Biology and Systematics Laboratory", "BeeBio & SysLab", 
                                          ifelse(det.source.size$sourceID == "http://iNaturalist.org is a place where you can record what you see in nature, meet other nature lovers, and learn about the natural world.", "iNaturalist", 
                                                 ifelse(det.source.size$sourceID == "Museum of Southwestern Biology", "Mus of SW Bio", 
                                                        ifelse(det.source.size$sourceID == "Santa Barbara Museum of Natural History Entomology Collection", "SB Mus of NHEntColl", 
                                                               ifelse(det.source.size$sourceID == "University of California Santa Barbara Invertebrate Zoology Collection", "UCSB InvertZoo Coll",  
                                                                      ifelse(det.source.size$sourceID == "R. M. Bohart Museum of Entomology", "RMBohart Mus", NA)))))))


det.source.size$sourceID  <- factor(det.source.size$sourceID )


levels(det.source.size$sourceID)






det.source.yellow$sourceID <- as.character(det.source.yellow$sourceID)


det.source.yellow$sourceID <- ifelse(det.source.yellow$sourceID == "American Museum of Natural History Invertebrate Zoology Collection", "AMNH InvertZoo Coll", 
                                   ifelse(det.source.yellow$sourceID == "Bee Biology and Systematics Laboratory", "BeeBio & SysLab", 
                                          ifelse(det.source.yellow$sourceID == "http://iNaturalist.org is a place where you can record what you see in nature, meet other nature lovers, and learn about the natural world.", "iNaturalist", 
                                                 ifelse(det.source.yellow$sourceID == "Museum of Southwestern Biology", "Mus of SW Bio", 
                                                        ifelse(det.source.yellow$sourceID == "Santa Barbara Museum of Natural History Entomology Collection", "SB Mus of NHEntColl", 
                                                               ifelse(det.source.yellow$sourceID == "University of California Santa Barbara Invertebrate Zoology Collection", "UCSB InvertZoo Coll",  
                                                                      ifelse(det.source.yellow$sourceID == "R. M. Bohart Museum of Entomology", "RMBohart Mus", NA)))))))


det.source.yellow$sourceID  <- factor(det.source.yellow$sourceID )







det.source.bowl$sourceID <- as.character(det.source.bowl$sourceID)


det.source.bowl$sourceID <- ifelse(det.source.bowl$sourceID == "American Museum of Natural History Invertebrate Zoology Collection", "AMNH InvertZoo Coll", 
                                     ifelse(det.source.bowl$sourceID == "Bee Biology and Systematics Laboratory", "BeeBio & SysLab", 
                                            ifelse(det.source.bowl$sourceID == "http://iNaturalist.org is a place where you can record what you see in nature, meet other nature lovers, and learn about the natural world.", "iNaturalist", 
                                                   ifelse(det.source.bowl$sourceID == "Museum of Southwestern Biology", "Mus of SW Bio", 
                                                          ifelse(det.source.bowl$sourceID == "Santa Barbara Museum of Natural History Entomology Collection", "SB Mus of NHEntColl", 
                                                                 ifelse(det.source.bowl$sourceID == "University of California Santa Barbara Invertebrate Zoology Collection", "UCSB InvertZoo Coll",  
                                                                        ifelse(det.source.bowl$sourceID == "R. M. Bohart Museum of Entomology", "RMBohart Mus", NA)))))))


det.source.bowl$sourceID  <- factor(det.source.bowl$sourceID )






#--------------- Source by bee size




# create plot
ggplot(data = det.source.size, aes(x = size_std, y = sum.det))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~sourceID, scales = "free")+
  ylim(c(0, 25))+
  theme_bw(25)+
  ylab("Number of OBSERVED bee-plant interactions")+
  xlab("Bee size")



ggsave("Figures/DETECTIONS-by-Bee-size-by-Source.pdf",
       height = 12, 
       width = 13)



# create plot
ggplot(data = det.source.size, aes(x = size_std, y = sum.pos))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~sourceID, scales = "free")+
  theme_bw(25)+
  ylab("Number of POSSIBLE bee-plant interactions")+
  xlab("Bee size")



ggsave("Figures/POSSIBLE-by-Bee-size-by-Source.pdf",
       height = 12, 
       width = 13)









#--------------- Source by yellow




# create plot
ggplot(data = det.source.yellow, aes(x = yellow, y = sum.det))+
  geom_point(size = 8)+
  geom_smooth()+
  facet_wrap(~sourceID, scales = "free")+
  theme_bw(25)+
  ylab("Number of OBSERVED bee-plant interactions")+
  xlab("Yellow (1 = yellow)")



ggsave("Figures/DETECTIONS-by-Plant-yellow-by-Source.pdf",
       height = 12, 
       width = 13)



# create plot
ggplot(data = det.source.yellow, aes(x = yellow, y = sum.pos))+
  geom_point(size = 8)+
  geom_smooth()+
  facet_wrap(~sourceID, scales = "free")+
  theme_bw(25)+
  ylab("Number of POSSIBLE bee-plant interactions")+
  xlab("Yellow (1 = yellow)")




ggsave("Figures/POSSIBLE-by-Plant-yellow-by-Source.pdf",
       height = 12, 
       width = 13)









#--------------- Source by bowl




# create plot
ggplot(data = det.source.bowl, aes(x = bowl, y = sum.det))+
  geom_point(size = 8)+
  geom_smooth()+
  facet_wrap(~sourceID, scales = "free")+
  theme_bw(25)+
  ylab("Number of OBSERVED bee-plant interactions")+
  xlab("Bowl (1 = bowl)")



ggsave("Figures/DETECTIONS-by-Plant-bowl-by-Source.pdf",
       height = 12, 
       width = 13)



# create plot
ggplot(data = det.source.bowl, aes(x = bowl, y = sum.pos))+
  geom_point(size = 8)+
  geom_smooth()+
  facet_wrap(~sourceID, scales = "free")+
  theme_bw(25)+
  ylab("Number of POSSIBLE bee-plant interactions")+
  xlab("Bowl (1 = bowl)")




ggsave("Figures/POSSIBLE-by-Plant-bowl-by-Source.pdf",
       height = 12, 
       width = 13)





# 4. Look for correlations among covaraites -------------------------------------------------------


# # Covariates
# stripped      = covariates$bee.covariates$striped,
# size          = covariates$bee.covariates$size_std,
# solitary     = covariates$bee.covariates$solitary,

# citation.code = covariates$citation.covariates$citation.code,

# flower_color  = 
# flower_shape  = 
# plant_family  = 

pdf("./Figures/2023_02_24/Fig. S2 - bee covariate correlation.pdf")
chart.Correlation(data.frame(striped = covariates$bee.covariates$striped,
                             size   = covariates$bee.covariates$size_std,
                             solitary = covariates$bee.covariates$solitary), histogram = TRUE, method = "pearson")

dev.off()

pdf("./Figures/2023_02_24/Fig. S3 - plant covariate correlation.pdf")
chart.Correlation(data.frame(yellow = covariates$plant.covariates$yellow,
                             bowl = covariates$plant.covariates$bowl,
                             aster = covariates$plant.covariates$aster), histogram = TRUE, method = "pearson")

dev.off()

# End script