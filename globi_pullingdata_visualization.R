### USING GLOBI DATA FOR TRI-TROPHIC INTERACTION NETWORK ANALYSES ###
# 14 september 2020
# michelle j lee
# This is a dataset pulled from the Global Biotic Interactions Database (GloBi). The following script pulls data using rglobi and visualizes these interaction networks.

##### Libraries used #####
library(tidyverse)
library(rglobi)
library(bipartite)
library(igraph)
library(ggplot2)

##### APIS DATA FROM GLOBI #####

# finding all interaction types can be difficult with the rglobi CRAN publication. use this command to look at all interaction types
interactions_types <- get_interaction_types()
View(interactions_types)

# PULLING INTERACTIONS FOR APIS MELLIFERA
# GloBi has limits on how much data you can pull at a time. these lines of code help you grab data and pagenate your searches

### PULL APIS POLLINATION INTERACTIONS
otherkeys = list("limit"=1000, "skip"=0)
first_page_apis_poll <- get_interactions(taxon = "Apis mellifera", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=1000)
second_page_apis_poll <- get_interactions(taxon = "Apis mellifera", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=2000)
third_page_apis_poll <- get_interactions(taxon = "Apis mellifera", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=3000)
fourth_page_apis_poll <- get_interactions(taxon = "Apis mellifera", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
# combine pages into one df
Apis_mell <- rbind(first_page_apis_poll, second_page_apis_poll, third_page_apis_poll, fourth_page_apis_poll) %>% 
  filter(source_taxon_name == "Apis mellifera")
# check for duplicates
check_Apis_dups <- duplicated(Apis_mell)
# create a unique list
Apis_mell_unique <- unique(Apis_mell)

### EXPLORE AND VISUALIZE INTERACTIONS

# make a df to count occurences
Apis_mell_count <- Apis_mell %>% 
  group_by(target_taxon_name) %>% 
  summarize(count = length(target_taxon_name))
Apis_mell_count
# make a histogram with counts
Apis_mell_hist <- ggplot(Apis_mell_count, aes(x = count)) +
  geom_histogram(bins = 7) +
  xlab("A. mellifera Interactions in Dataset") +
  ylab("Frequency")
Apis_mell_hist

# make a df to count unique occurences
u_Apis_mell_count <- Apis_mell_unique %>% 
  group_by(target_taxon_name) %>% 
  summarize(count = length(target_taxon_name))
u_Apis_mell_count
# make a histogram with unique counts
u_Apis_mell_hist <- ggplot(u_Apis_mell_count, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  xlab("Unique A. mellifera Interactions in Dataset") +
  ylab("Frequency")
u_Apis_mell_hist

# create bipartite network visualization with plants using package:bipartite
plantweb <- frame2webs(Apis_mell_unique, varnames = c("target_taxon_name", "source_taxon_name", "source_taxon_name"), type.out = 'array')
plotweb(as.data.frame(plantweb)) 
# not super helpful as only Apis mellifera interacting with thousands of different plant species

# create bipartite network visualization with plants using package:igraph
igraph_apis <- graph_from_incidence_matrix(as.data.frame(plantweb))
igraph_apis %>%
  add_layout_(as_bipartite()) %>%
  plot()
# again, not super helpful with only Apis mellifera interacting with many plants
# note: make sure the graph window is large enough, otherwise you will get a false error



##### APIDAE DATA FROM GLOBI #####
# grab data and pagenation
otherkeys = list("limit"=1000, "skip"=0)
first_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=1000)
second_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=2000)
third_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=3000)
fourth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=4000)
fifth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=5000)
sixth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=6000)
seventh_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=7000)
eighth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=8000)
ninth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=9000)
tenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=10000)
eleventh_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=11000)
twelfth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=12000)
thirteenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=13000)
fourteenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=14000)
fifteenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=15000)
sixteenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=16000)
seventeenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=17000)
eighteenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=18000)
nineteenth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=19000)
twentieth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=20000)
twentyfirst_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=21000)
twentysecond_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "visitsFlowersOf"), otherkeys = otherkeys)


# combine pages into one df
Apidae <- rbind(first_page_of_thousand,second_page_of_thousand, third_page_of_thousand, fourth_page_of_thousand, fifth_page_of_thousand, sixth_page_of_thousand, seventh_page_of_thousand, eighth_page_of_thousand, ninth_page_of_thousand, tenth_page_of_thousand, eleventh_page_of_thousand, twelfth_page_of_thousand, thirteenth_page_of_thousand, fourteenth_page_of_thousand, fifteenth_page_of_thousand, sixteenth_page_of_thousand, seventeenth_page_of_thousand, eighteenth_page_of_thousand, nineteenth_page_of_thousand, twentieth_page_of_thousand, twentyfirst_page_of_thousand, twentysecond_page_of_thousand)
# make a df to count occurences
Ap_count <- Apidae %>% 
  group_by(target_taxon_name) %>% 
  summarize(count = length(target_taxon_name))
Ap_count
# make a histogram with counts
Ap_hist <- ggplot(Ap_count, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  xlab("Frequency of Apidae Interactions in Dataset")
Ap_hist

# remove duplicated rows
Apidae_unique <- unique(Apidae)

# make a df to count unique occurences
u_Apidae_count <- Apidae_unique %>% 
  group_by(target_taxon_name) %>% 
  summarize(count = length(target_taxon_name))
u_Apidae_count
# make a histogram with unique counts
u_Apidae_hist <- ggplot(u_Apidae_count, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  xlab("Unique Apidae Interactions in Dataset") +
  ylab("Frequency")
u_Apidae_hist

# check to see all of the different source taxon resolution levels
list_Apidae <- unique(Apidae_unique$source_taxon_name)
View(list_Apidae)
# pull out unique genera
genera_Apidae <- Apidae_unique %>% 
  separate(source_taxon_name, into = c("genus", "species"), sep = " ") %>% 
  filter(genus != "Apidae")


# make a df to count unique genera
genera_Apidae_count <- genera_Apidae %>% 
  group_by(genus) %>% 
  summarize(count = length(genus))
genera_Apidae_count
# make a histogram with unique counts
genera_Apidae_hist <- ggplot(genera_Apidae_count, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  xlab("Unique Apidae Genera Interactions in Dataset") +
  ylab("Frequency")
genera_Apidae_hist

# make bipartite visualization
apidaeplantweb <- frame2webs(genera_Apidae, varnames = c("target_taxon_name", "genus", "genus"), type.out = 'array')
igraph_apidae <- graph_from_incidence_matrix(as.data.frame(apidaeplantweb))
igraph_apidae %>%
  add_layout_(as_bipartite()) %>%
  plot()
