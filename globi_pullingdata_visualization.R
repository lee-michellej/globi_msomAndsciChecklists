### USING GLOBI DATA FOR TRI-TROPHIC INTERACTION NETWORK ANALYSES ###
# 14 september 2020
# michelle j lee
# This is a dataset pulled from the Global Biotic Interactions Database (GloBi). The following script pulls data using rglobi and visualizes these interaction networks.

##### Libraries used #####
library(tidyverse)
library(rglobi)
library(bipartite)
library(igraph)

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

### EXAMPLE IGRAPH CODE
inc <- matrix(sample(0:1, 50, replace = TRUE, prob=c(2,1)), 10, 5)
g <- graph_from_incidence_matrix(inc)
plot(g, layout = layout_as_bipartite,
     vertex.color=c("green","cyan")[V(g)$type+1])
# Two columns
g %>%
  add_layout_(as_bipartite()) %>%
  plot()




##### Back to pollinators #####
#describe network
poll.network<-graph.data.frame(apis_plants, directed=F)

#bipartite network
g <- graph_from_data_frame(apis_plants)

layer = rep(2, length(V(g)$name))
layer[grep("Recipe:",V(g)$name)]=1
layer[grep("compound:",V(g)$name)]=3

names = V(g)$name
names = sub("Recipe:","", names)
names = sub("compound:","", names)
V(g)$name = names


#plot the graph
plot(bsk.network,vertex.size=5, vertex.label=V, layout=layout_in_circle)

V(g)
E(g)


all_edges <- do.call(rbind,
                     lapply( list(edge_C_G, edge_P_C, edge_P_G), function(x) setNames(x, c("1","2")) )
)

g1 <- graph.data.frame(d = all_edges, directed = TRUE)





### PULL INTERACTIONS FOR APIDAE ###
# grab data and pagenation
otherkeys = list("limit"=1000, "skip"=0)
first_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=1000)
second_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=2000)
third_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=3000)
fourth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=4000)
fifth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=5000)
sixth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=6000)
seventh_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=7000)
eighth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)
otherkeys = list("limit"=1000, "skip"=8000)
ninth_page_of_thousand <- get_interactions(taxon = "Apidae", interaction.type = c("pollinates", "hasParasite"), otherkeys = otherkeys)






# combine pages into one df
Apidae <- rbind(first_page_of_thousand,second_page_of_thousand, third_page_of_thousand, fourth_page_of_thousand, fifth_page_of_thousand, sixth_page_of_thousand, seventh_page_of_thousand, eighth_page_of_thousand, ninth_page_of_thousand)
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


# make a df to count occurences
bee_count <- Apidae %>% 
  group_by(source_taxon_name) %>% 
  summarize(count = length(source_taxon_name))
bee_count
# make a histogram with counts
bee_hist <- ggplot(bee_count, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  xlab("Frequency of Apidae in Dataset")
bee_hist

# filtering and plotting webs
apidae_filtered <- Apidae %>% 
  filter(interaction_type == "pollinates")

plantweb2 <- frame2webs(apidae_filtered, varnames = c("target_taxon_name", "source_taxon_name", "source_taxon_name"), type.out = 'array')
plotweb(as.data.frame(plantweb2))

apidae_parasites <- Apis_mell %>% 
  filter(interaction_type == "hasParasite")
parweb <- frame2webs(apis_parasites, varnames = c("source_taxon_name", "target_taxon_name", "source_taxon_name"), type.out = 'array')
plotweb(as.data.frame(parweb))
