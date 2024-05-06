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
# citation.type <- read.csv("./Data/citation-list-type.csv")
citation.list <- read.csv("./Data/final-globi-citations-unique 2024 04 07.csv")








#--------------- This analysis was run WITHOUT Apis mellifora - here we are removing the species


# Remove Apis entry for each file
size <- size[size$Species != "Apis mellifera",]
color <- color[color$Species != "Apis mellifera",]
sociality <- sociality[sociality$Species != "Apis mellifera",]



# Upload the observed bee-plant data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2023_02_23 - short plant list - no apis.rds")
  # object name = bee.plant.date.cite
  # 4-D array
  

# Flower color
plant.covariates <- plant.phenology2 # this file was created in 4 - Format_dat_checklist_2024_02_24.R




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


# Out list of citations
citation.list$x


# Vector with citation type
# citation.type <- c("Collection Specimen", #[1] "American Museum of Natural History Invertebrate Zoology Collection"                                                                         
#               "Collection Specimen", #[2] "Bee Biology and Systematics Laboratory"                                                                                                     
#               "Observation" , #[3] "http://iNaturalist.org is a place where you can record what you see in nature, meet other nature lovers, and learn about the natural world."
#               
#               "Collection Specimen", #[4] "Museum of Southwestern Biology"                                                                                                             
#               "Collection Specimen", #[5] "R. M. Bohart Museum of Entomology"                                                                                                          
#               "Collection Specimen", #[6] "Santa Barbara Museum of Natural History Entomology Collection"                                                                              
#               "Collection Specimen" #[7] "University of California Santa Barbara Invertebrate Zoology Collection"  
#               )
citation.type <- c("Aggregated Data", # [1] "A. Thessen. 2014. Species associations extracted from EOL text data objects via text mining."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Collection Specimen", # [2] "American Museum of Natural History Invertebrate Zoology Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                   "Collection Specimen", # [3] "Archbold Biological Station Arthropod Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                   "Collection Specimen", # [4] "Arizona State University"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                   "Collection Specimen", # [5] "Bee Biology and Systematics Laboratory"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                   "Collection Specimen", # [6] "BLM Mother Lode Field Office: The Bees of Pine Hill Preserve"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Collection Specimen", # [7] "C.P. Gillette Museum of Arthropod Diversity"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
                   "Literature", # [8] "Carril OM, Griswold T, Haefner J, Wilson JS. (2018) Wild bees of Grand Staircase-Escalante National Monument: richness, abundance, and spatio-temporal beta-diversity. PeerJ 6:e5867 https://doi.org/10.7717/peerj.5867"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
                   "Collection Specimen", # [9] "Connecticut Agricultural Experiment Station Arthropod Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                   "Collection Specimen", # [10] "Cornell University Insect Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                   "Collection Specimen", # [11] "Essig Museum of Entomology"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                   "Collection Specimen", # [12] "Florida State Collection of Arthropods"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                   "Literature", # [13] "Giselle Muschett & Francisco E. Fontúrbel. 2021. A comprehensive catalogue of plant – pollinator interactions for Chile"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
                   "Literature", # [14] "Guzman, Laura Melissa; Kelly, Tyler; Elle, Elizabeth, 2022, \"\"A dataset for pollinator diversity and their interactions with plants in the Pacific NorthWest\"\", https://doi.org/10.5683/SP3/WTEZNH, Borealis, V1"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                   "Aggregated Data", # [15] "https://mangal.io - the ecological interaction database."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                   "Observation", # [16] "iNaturalist Research-grade Observations"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
                   "Literature",  # [17] "IPBES. (2016). The assessment report of the Intergovernmental Science-Policy Platform on Biodiversity and Ecosystem Services on pollinators, pollination and food production. Table 2.4.3 p88 Zenodo. https://doi.org/10.5281/zenodo.3402857"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Collection Specimen", # [18] "Kenneth S. Norris Center for Natural History, University of California Santa Cruz, Insect Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                   "Collection Specimen", # [19] "KWP Lepidoptera Collection (Arctos)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                   "Literature", # [20] "LaManna, JA, Burkle, LA, Belote, RT, Myers, JA. Biotic and abiotic drivers of plant–pollinator community assembly across wildfire gradients. J Ecol. 2020; 00: 1– 14. https://doi.org/10.1111/1365-2745.13530 ."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                   "Collection Specimen", # [21] "Mississippi Entomological Museum"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                   "Collection Specimen", # [22] "Museum of Comparative Zoology, Harvard University"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                   "Collection Specimen", # [23] "Museum of Southwestern Biology"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                   "Aggregated Data", # [24] "National Database Plant Pollinators. Center for Plant Conservation at San Diego Zoo Global. Accessed via https://saveplants.org/national-collection/pollinator-search/ on 2020-06-05."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                   "Collection Specimen", # [25] "Natural History Museum of Los Angeles County"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Literature", # [26] "Nick Balfour, Maria Clara Castellanos, Chris Johnson, Dave Goulson, Andrew Philippides. 2023. The Database of Pollinator Interactions (DoPI). Accessed at https://www.sussex.ac.uk/lifesci/ebe/dopi/ on 2023-12-01."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                   "Literature", # [27] "Ollerton, J., Trunschke, J. ., Havens, K. ., Landaverde-González, P. ., Keller, A. ., Gilpin, A.-M. ., Rodrigo Rech, A. ., Baronio, G. J. ., Phillips, B. J., Mackin, C. ., Stanley, D. A., Treanore, E. ., Baker, E. ., Rotheray, E. L., Erickson, E. ., Fornoff, F. ., Brearley, F. Q. ., Ballantyne, G. ., Iossa, G. ., Stone, G. N., Bartomeus, I. ., Stockan, J. A., Leguizamón, J., Prendergast, K. ., Rowley, L., Giovanetti, M., de Oliveira Bueno, R., Wesselingh, R. A., Mallinger, R., Edmondson, S., Howard, S. R., Leonhardt, S. D., Rojas-Nossa, S. V., Brett, M., Joaqui, T., Antoniazzi, R., Burton, V. J., Feng, H.-H., Tian, Z.-X., Xu, Q., Zhang, C., Shi, C.-L., Huang, S.-Q., Cole, L. J., Bendifallah, L., Ellis, E. E., Hegland, S. J., Straffon Díaz, S., Lander, T. A. ., Mayr, A. V., Dawson, R. ., Eeraerts, M. ., Armbruster, W. S. ., Walton, B. ., Adjlane, N. ., Falk, S. ., Mata, L. ., Goncalves Geiger, A. ., Carvell, C. ., Wallace, C. ., Ratto, F. ., Barberis, M. ., Kahane, F. ., Connop, S. ., Stip, A. ., Sigrist, M. R. ., Vereecken, N. J. ., Klein, A.-M., Baldock, K. ., & Arnold, S. E. J. . (2022). Pollinator-flower interactions in gardens during the COVID-19 pandemic lockdown of 2020. Journal of Pollination Ecology, 31, 87–96. https://doi.org/10.26786/1920-7603(2022)695"
                   "Collection Specimen", # [28] "Orthoptera DNA Tissue Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                   "Literature", # [29] "Pardee et al. 2023: Plant-Pollinator Networks across an Urban Corridor"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                   "Literature", # [30] "Plant\xd0pollinator community assembly across wildfire gradients"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                   "Literature", # [31] "Pollinator interaction flexibility across scales affects patch colonization and occupancy"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                   "Collection Specimen", # [32] "R. M. Bohart Museum of Entomology"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                   "Literature", # [33] "Redhead, J.W.; Coombes, C.F.; Dean, H.J.; Dyer, R.; Oliver, T.H.; Pocock, M.J.O.; Rorke, S.L.; Vanbergen, A.J.; Woodcock, B.A.; Pywell, R.F. (2018). Plant-pollinator interactions database for construction of potential networks. NERC Environmental Information Data Centre. https://doi.org/10.5285/6d8d5cb5-bd54-4da7-903a-15bd4bbd531b"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Collection Specimen", # [34] "RL Minckley Insect and Plant Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
                   "Collection Specimen", # [35] "Robert L. Minckley San Bernardino Valley from the year 2000 to 2011."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                   "Collection Specimen",  # [36] "Santa Barbara Museum of Natural History Entomology Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                   "Literature", # [37] "Sarah E Miller. 12/13/2016. Species associations manually extracted from Onstad, D.W. EDWIP: Ecological Database of the World's Insect Pathogens. Champaign, Illinois: Illinois Natural History Survey, [23/11/2016]. http://insectweb.inhs.uiuc.edu/Pathogens/EDWIP."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                   "Literature", # [38] "Schwarz, Benjamin et al. (2021). Data from: Temporal scale-dependence of plant-pollinator networks [Dataset]. Dryad. https://doi.org/10.5061/dryad.qz612jmbp"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Literature", # [39] "Seltmann, K., Van Wagner, J., Behm, R., Brown, Z., Tan, E., & Liu, K. (2020). BID: A project to share biotic interaction and ecological trait data about bees (Hymenoptera: Anthophila). UC Santa Barbara: Cheadle Center for Biodiversity and Ecological Restoration. Retrieved from https://escholarship.org/uc/item/1g21k7bf"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                   "Collection Specimen", # [40] "The Purdue Entomological Research Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                   "Collection Specimen", # [41] "United States Geological Survey (USGS) Pollinator Library. https://www.npwrc.usgs.gov/pollinator."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                   "Collection Specimen",  # [42] "United States National Museum, Entomology Collections"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                   "Collection Specimen", # [43] "University of California Santa Barbara Invertebrate Zoology Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                   "Collection Specimen", # [44] "University of Colorado Museum of Natural History Entomology Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                   "Collection Specimen", # [45] "University of Connecticut Museum"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                   "Collection Specimen", # [46] "University of Kansas Natural History Museum Entomology Division"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                   "Collection Specimen",  # [47] "University of Kentucky Insect Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                   "Collection Specimen", # [48] "University of Michigan Museum of Zoology, Division of Insects"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                   "Collection Specimen", # [49] "University of Northern Iowa - Wen Research Collection"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                   "Aggregated Data" # [50] "Web of Life. http://www.web-of-life.es ."
                   )



citation.code <- ifelse(citation.type == "Observation", 1, 0)


citation.covariates <- data.frame(citation.name = citation.list$x,
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



# Save all covariates in a single object
covariates <- list(bee.covariates = bee.covariates,
                   plant.covariates = plant.covariates,
                   citation.covariates = citation.covariates)


save(covariates, file = "./Data/model_covariates - 2024 05 01 - no apis.rds")






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

