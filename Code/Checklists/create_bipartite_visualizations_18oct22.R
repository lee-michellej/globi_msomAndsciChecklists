# ### Create example network visualizations for GloBI manuscript ###
# created by M. Lee 26 Sep 2022
# updated by M. Lee 12 Oct 2022





# This code does the following ---------------------------------------
# download cleaned interaction and network metric data
# look at spread of network metrics to choose networks to print
# print networks for a spread of islets (maybe ~4)



# Load libraries and set working directory ------------------------------


library(tidyverse)
library(ggplot2)
library(bipartite)
library(vegan)
library(igraph)
setwd("~/Desktop/palmyra_Rproject/Network_Analyses/11_createnetworks_code/")
source("../11_createnetworks_code/interaction_matrix_function.R")


# Main csv files for manipulation ------------------------
setwd("~/Downloads/")
dat <- read_csv("bee-plant-mod-probabilities.csv") %>% 
  mutate(prob = max_prob * 10000,
         log_prob = log(prob),
         corrected_log_prob = ifelse(log_prob > 0, log_prob, 0),
         prob_100 = max_prob * 100,
         prob_10 = max_prob * 10)

#hist(dat$prob)
#hist(dat$log_prob)
#hist(dat$prob_100)
#hist(dat$max_prob)
#hist(dat$prob_10)

# what does probability of an interaction actually mean?
# and what measure would be most equivalent to actual counts from the globi dataset?
# max probability ranges from 0-1
# I would either use:
# prob_10, multiplying max_prob*10
# prob_100, multiplying max_prob*100
# will try visualizations with both sets and see what happens.

setwd("~/Desktop/globi/globi_tritrophic_networks/Data/")
globi_dat <- read_csv("final-globi-list-clean 2022 02 01.csv") %>% 
  select(resolvedPlantNames, resolvedBeeNames, sourceTaxonFamilyName, targetTaxonOrderName) %>% 
  mutate(plant_order = ifelse(is.na(targetTaxonOrderName),
                              "Boraginales",
                              targetTaxonOrderName))




# Modeled network bipartite ------

# cut off here of probability of 0.03%
cut_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 3)

# make interaction matrix
cut_matdat <- as.data.frame(df2intmatrix(as.data.frame(cut_df), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))


# Make list of cut-off species -----

# using cut-off above, make unique list of bee species
bee.names <- as.data.frame(unique(cut_df$bee.names))
colnames(bee.names)[1] <- "resolvedBeeNames"

# using cut-off above, make uniqeu list of plant species
plant.names <- as.data.frame(unique(cut_df$plant.names))
colnames(plant.names)[1] <- "resolvedPlantNames"

# will need to merge these with other plant list and bee list
# will need family names etc


# +++++ ------


# GloBI network bipartite ------

# filter by cut off species lists above
unmatched.bees <- anti_join(globi_dat, bee.names, by = "resolvedBeeNames")
list(unique(unmatched.bees$resolvedBeeNames)) # 7 species
# 193 observations should drop
# "Anthidium collectum"     "Hylaeus polifolii"       "Apis mellifera"          "Epeolus minimus"        
#"Xeromelecta californica" "Anthidium illustre"      "Hylaeus mesillae"  

unmatched.plants <- anti_join(globi_dat, plant.names, by = "resolvedPlantNames")
list(unique(unmatched.plants$resolvedPlantNames)) # 9 species
# might be overlapping with above, but should be about 22 dropped
#"Tamarix ramosissima"   "Datura wrightii"       "Salix lasiolepis"      "Artemisia californica"
#"Melilotus albus"       "Phyla nodiflora"       "Quercus lobata"        "Erodium cicutarium"   
#"Myoporum laetum"


# in theory could drop as low as 351
globi_filtered1 <- filter(globi_dat, globi_dat$resolvedBeeNames %in% bee.names$resolvedBeeNames)
# first filter drops to 373
globi_filtered <- filter(globi_filtered1, globi_filtered1$resolvedPlantNames %in% plant.names$resolvedPlantNames)
# second filter doesn't drop any interactions


# make interaction matrix
globi_matdat <- as.data.frame(df2intmatrix(as.data.frame(globi_filtered), 
                                         varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                                         type.out = "array",
                                         emptylist = TRUE))


# +++++ ------

# Order of species list by phylogenetic order ----
# not completely ordered correctly:
# ordered by family for bees: https://www.researchgate.net/figure/Family-and-subfamily-level-phylogeny-for-bees-based-on-Danforth-et-al-2013_fig1_332959316
# ordered to order for plants: https://www.researchgate.net/publication/279592674_An_ordinal_classification_for_the_families_of_flowering_plants
setwd("~/Desktop/globi/globi_tritrophic_networks/Data/")


# ///// for GloBI data -------

plant_phylog <- read_csv("plant_phylog.csv")
bee_phylog <- read_csv("bee_phylog.csv")

# make phylogeny list for globi plant list
glob_plant_order <- plant_phylog %>% 
  right_join(globi_filtered, by = "plant_order") %>% 
  arrange(plant_phylog, resolvedPlantNames)

# make phylogeny list for globi bee list
glob_bee_order <- bee_phylog %>% 
  right_join(globi_filtered, by = c("bee_family" = "sourceTaxonFamilyName")) %>% 
  arrange(bee_phylog, resolvedBeeNames)

# make list to feed to network code
glob_order <- list(
  seq.high = unique(glob_bee_order$resolvedBeeNames),
  seq.low = unique(glob_plant_order$resolvedPlantNames)
)




# ///// for modelled data -------

# SOMETHING HAPPENING HERE WHERE LISTS AREN'T MATCHING UP

# make phylogeny list for modeled plant list
mod_plant_list <- as.data.frame(read_csv("plant_phylog_modellist.csv")) %>% 
  filter(!is.na(scientificName))

# check for missing species names
# mod_plant_missing <- anti_join(cut_df,
#                               mod_plant_list, 
#                               by = c("plant.names" = "scientificName"))

mod_plant_order <- left_join(cut_df, mod_plant_list, 
                             by = c("plant.names" = "scientificName")) %>% 
  left_join(plant_phylog, by  = c("Order" = "plant_order")) %>% 
  arrange(plant_phylog, resolvedPlantNames)
#resolvedPlantNames
#want Family order


# make phylogeny list for modeled bee list
mod_bee_list <- read_csv("bee_phylog_modellist.csv")
# 23 bee species -- the same as the unique bees included in model cut off

mod_bee_phylog <- left_join(cut_df, mod_bee_list, by = "bee.names") %>% 
  left_join(bee_phylog, by = c("bee.family" = "bee_family")) %>% 
  arrange(bee_phylog, bee.names)


# make list to feed to network code
mod_order <- list(
  seq.high = unique(mod_bee_phylog$bee.names),
  seq.low = unique(mod_plant_order$plant.names)
)



# +++++ ------

# Plot modeled network -------

plotweb(cut_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        sequence = mod_order
        #plot.axes = TRUE
)



# +++++ ------

# Plot GloBI network -------

# without phylogenetic order
plotweb(globi_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        #plot.axes = TRUE
)


# with phylogenetic order


plotweb(globi_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        #plot.axes = TRUE
        sequence = glob_order
)

#adjustcolor("lightgray", alpha.f = 0.75)











# +++++ ------
# Bipartite for individual bee species -------------------------------

matrix <- frame2webs(as.data.frame(dat), 
                     varnames = c("plant.names", "bee.names", "bee.names", "corrected_log_prob"), 
                     type.out = "array", 
                     emptylist = TRUE)

matdat <- df2intmatrix(as.data.frame(dat), 
                       varnames = c("plant.names", "bee.names", "prob_10"),
                       type.out = "array",
                       emptylist = TRUE)

matdf <- as.data.frame(matdat)

matrix_agopost <- matdf %>% 
  select(`Agapostemon texanus`)

plotweb(matrix_agopost, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("pink", alpha.f = 0.75),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = adjustcolor("lightgray", alpha.f = 0.75),
        bor.col.high = NA,
        bor.col.low = adjustcolor("lightgray", alpha.f = 0.75),
        text.rot = 90,
        y.lim = c(-5,1.5),
        x.lim = c(0, 1),
        text.high.col = FALSE,
        high.plot = FALSE
)



# ////with interaction cut off ----------------

cut_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 3)


cut_matdat <- as.data.frame(df2intmatrix(as.data.frame(cut_df), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))

cut_mat_oneSpec <- cut_matdat %>% 
  select(`Agapostemon texanus`)

plotweb(cut_mat_oneSpec, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("pink", alpha.f = 0.75),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = adjustcolor("lightgray", alpha.f = 0.75),
        bor.col.high = NA,
        bor.col.low = adjustcolor("lightgray", alpha.f = 0.75),
        text.rot = 90,
        y.lim = c(-5,1.5),
        x.lim = c(0, 1),
        text.high.col = FALSE,
        high.plot = FALSE
)



plotweb(cut_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("pink", alpha.f = 0.75),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = adjustcolor("lightgray", alpha.f = 0.75),
        bor.col.high = adjustcolor("lightgray", alpha.f = 0.75),
        bor.col.low = adjustcolor("lightgray", alpha.f = 0.75),
        text.rot = 90,
        y.lim = c(-1,1),
        #x.lim = c(0, 1),
)







# ////try igraph -------

igraph_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) #%>% 
#dplyr::filter(prob_10 >= 2)

igraph_df_oneSpec <- igraph_df %>% 
  filter(bee.names == "Agapostemon texanus")

g <- graph.data.frame(igraph_df_oneSpec, directed = F)
E(g)$weight <- igraph_df_oneSpec[,3]
V(g)$type <- bipartite_mapping(g)$type

print(g)

plot(g, layout=layout.bipartite, edge.width=unlist(E(g)$weight), vertex.size=7,vertex.label.cex=0.6)






igraph_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 3)

igraph_df_oneSpec <- igraph_df %>% 
  filter(bee.names == "Agapostemon texanus")

g <- graph.data.frame(igraph_df_oneSpec, directed = F)
E(g)$weight <- igraph_df_oneSpec[,3]
V(g)$type <- bipartite_mapping(g)$type

print(g)

plot(g, layout=layout.bipartite, edge.width=unlist(E(g)$weight), vertex.size=7,vertex.label.cex=0.6)







# ////now with "Andrena auricoma" ------------------



# bipartite

cut_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 2)


cut_matdat <- as.data.frame(df2intmatrix(as.data.frame(cut_df), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))

cut_mat_oneSpec <- cut_matdat %>% 
  select(`Andrena auricoma`)

plotweb(cut_mat_oneSpec, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("pink", alpha.f = 0.75),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = adjustcolor("lightgray", alpha.f = 0.75),
        bor.col.high = NA,
        bor.col.low = adjustcolor("lightgray", alpha.f = 0.75),
        text.rot = 90,
        y.lim = c(-5,1.5),
        x.lim = c(0, 1),
        text.high.col = FALSE,
        high.plot = FALSE
)






# igraph
igraph_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 2)

igraph_df_oneSpec <- igraph_df %>% 
  filter(bee.names == "Andrena auricoma")

g <- graph.data.frame(igraph_df_oneSpec, directed = F)
E(g)$weight <- igraph_df_oneSpec[,3]
V(g)$type <- bipartite_mapping(g)$type

print(g)

plot(g, layout=layout.bipartite, edge.width=unlist(E(g)$weight), vertex.size=7,vertex.label.cex=0.6)





