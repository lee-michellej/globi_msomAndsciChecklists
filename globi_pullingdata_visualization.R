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
  geom_histogram(binwidth = 1) +
  xlab("Frequency of A. mellifera Interactions in Dataset")
Apis_mell_hist


# make a df to count occurences
u_Apis_mell_count <- Apis_mell_unique %>% 
  group_by(target_taxon_name) %>% 
  summarize(count = length(target_taxon_name))
u_Apis_mell_count
# make a histogram with counts
u_Apis_mell_hist <- ggplot(u_Apis_mell_count, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  xlab("Unique A. mellifera Interactions in Dataset") +
  ylab("Frequency")
u_Apis_mell_hist

apis_plants <- Apis_mell %>% 
  filter(interaction_type == "pollinates")

plantweb <- frame2webs(apis_plants, varnames = c("target_taxon_name", "source_taxon_name", "source_taxon_name"), type.out = 'array')
plotweb(as.data.frame(plantweb))

apis_parasites <- Apis_mell %>% 
  filter(interaction_type == "hasParasite")
parweb <- frame2webs(apis_parasites, varnames = c("source_taxon_name", "target_taxon_name", "source_taxon_name"), type.out = 'array')
plotweb(as.data.frame(parweb))

# plot tri-trophic 
plotweb2(as.data.frame(plantweb), as.data.frame(parweb), method = "normal", empty = FALSE)





##### PRACTICE #####
data = "From, To
Recipe:Chicken Marsala,flour
Recipe:Chicken Marsala,sage
Recipe:Chicken Marsala,chicken
Recipe:Chicken Marsala,wine
Recipe:Chicken Marsala,butter
Recipe:Glazed Carrots,butter
Recipe:Glazed Carrots,vinegar
Recipe:Glazed Carrots,carrot
Recipe:Glazed Carrots,chive
flour,compound:X2
sage,compound:X3
chicken,compound:X6
chicken,compound:X7
wine,compound:X1
wine,compound:X4
wine,compound:X5
wine,compound:X8
wine,compound:X9
wine,compound:X10
wine,compound:X11
wine,compound:X12
butter,compound:X4
butter,compound:X5
butter,compound:X7
butter,compound:X8
butter,compound:X11
vinegar,compound:X8
vinegar,compound:X13
carrot,compound:X2
carrot,compound:X15
chive,compound:X6
chive,compound:X14
"
data=read.csv(textConnection(data),head=TRUE)
g = graph_from_data_frame(data,directed=FALSE)
layer = rep(2, length(V(g)$name))
layer[grep("Recipe:",V(g)$name)]=1
layer[grep("compound:",V(g)$name)]=3
names = V(g)$name
names = sub("Recipe:","", names)
names = sub("compound:","", names)
V(g)$name = names
layout = layout_with_sugiyama(g, layers=layer)
plot(g,
     layout=cbind(layer,layout$layout[,1]),
     vertex.shape=c("square","circle","none")[layer],
     vertex.size=c(50,20,0)[layer],
     vertex.label.dist=c(0,0,.8)[layer],
     vertex.label.degree=0)


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
