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


# code from bipartite to make interaction matrix -------
df2intmatrix <- function(dframe, varnames = c("lower", "higher", "freq"), type.out = "list", emptylist = TRUE) {
  if (length(varnames)==3) {
    if (any(is.na(dframe[,varnames[3]]))) warning(paste("NAs in", varnames[3], "converted to 0"))
    webarray <- tapply(dframe[,varnames[3]],dframe[,varnames[1:2]], sum)
  }
  if (length(varnames)==2) webarray <- tapply(rep(1,nrow(dframe)),dframe[,varnames[1:2]], sum)
  webarray[is.na(webarray)] <- 0   # needs to be done when using tapply: unobserved combinations always get a zero, even with na.rm=T
  if (type.out=="array") return(webarray)
  if (type.out=="list") {
    weblist <- list()
  }
}


# Main csv files for manipulation ------------------------
dat <- read_csv("./Data/bee-plant-mod-probabilities.csv") %>% 
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

globi_dat <- read_csv("./Data/final-globi-list-clean 2022 02 01.csv") %>% 
  select(resolvedPlantNames, resolvedBeeNames, sourceTaxonFamilyName, targetTaxonOrderName, targetTaxonFamilyName) %>% 
  mutate(plant_order = ifelse(is.na(targetTaxonOrderName),
                              "Boraginales",
                              targetTaxonOrderName)) %>% 
  separate(resolvedPlantNames, into = c("genus", NA), sep = " ", remove = FALSE)




# Modeled network bipartite ------

# cut off here of probability of 3%
cut_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 3) %>% 
  separate(plant.names, into = c("genus", NA), sep = " ", remove = FALSE)

# make interaction matrix

cut_matdat <- as.data.frame(df2intmatrix(as.data.frame(cut_df), 
                                                    varnames = c("plant.names", "bee.names", "prob_10"),
                                                    type.out = "array",
                                                    emptylist = TRUE))


cut_matdat.gen <- as.data.frame(df2intmatrix(as.data.frame(cut_df), 
                                         varnames = c("genus", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))


# Make list of cut-off species -----

# using cut-off above, make unique list of bee species
bee.names <- as.data.frame(unique(cut_df$bee.names))
colnames(bee.names)[1] <- "resolvedBeeNames"

# using cut-off above, make uniqeu list of plant species
plant.names <- as.data.frame(unique(cut_df$plant.names))
colnames(plant.names)[1] <- "resolvedPlantNames"

# using cut-off above, make unique list of plant genera
plant.genera <- as.data.frame(unique(cut_df$genus))
colnames(plant.genera)[1] <- "resolvedPlantGenus"

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

#make filtered list with genera
globi_filtered.gen <- filter(globi_filtered1, globi_filtered1$genus %in% plant.genera$resolvedPlantGenus)



## EDIT 18Nov22: make list of plant names in order to filter the cutmatrix to match

globi_filtered.plantlist <- as.data.frame(unique(globi_filtered$resolvedPlantNames))
colnames(globi_filtered.plantlist)[1] <- "resolvedPlantNames"


# make interaction matrix
globi_matdat <- as.data.frame(df2intmatrix(as.data.frame(globi_filtered), 
                                         varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                                         type.out = "array",
                                         emptylist = TRUE))

globi_matdat.gen <- as.data.frame(df2intmatrix(as.data.frame(globi_filtered.gen), 
                                               varnames = c("genus", "resolvedBeeNames"),
                                               type.out = "array",
                                               emptylist = TRUE))



# EDIT 18Nov22: make new cutmat to match globi plant list
cut_df_matchplant <- filter(cut_df, cut_df$plant.names %in% globi_filtered.plantlist$resolvedPlantNames)


cut_matdat_matchplant <- as.data.frame(df2intmatrix(as.data.frame(cut_df_matchplant), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))






# +++++ ------
# Network metric differences -------
# +++++ ------

# calculate some basic metrics for the model
model_metrics <- as.data.frame(networklevel(cut_matdat, index = c('nestedness', 
                                                 'connectance',
                                                 'interaction evenness',
                                                 'NODF',
                                                 'H2')))
names(model_metrics)[1] <- "model_values"




# calculate some basic metrics for the globi data
globi_metrics <- as.data.frame(networklevel(globi_matdat, index = c('nestedness', 
                                                                  'connectance',
                                                                  'interaction evenness',
                                                                  'NODF',
                                                                  'H2')))
names(globi_metrics)[1] <- "globi_values"

tglobi_metrics <- as.data.frame(t(globi_metrics))




# combine two dataframes
metrics <- cbind(model_metrics, globi_metrics)







# +++++ ------
# Null models -------

# rewrite files as not dataframes ------
model_matrix <- df2intmatrix(as.data.frame(cut_df), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE)


globi_matrix <- df2intmatrix(as.data.frame(globi_filtered), 
                             varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                             type.out = "array",
                             emptylist = TRUE)

# make list of webs and name them ------
webs <- list(model_matrix, globi_matrix)

webs.names <- c("model", "globi")
names(webs) <- webs.names
             

# calculate network measures -------
net.metrics.nest <- lapply(webs, networklevel, index = 'nestedness')
net.metrics.evenness <- lapply(webs, networklevel, index = 'interaction evenness')
net.metrics.connect <- lapply(webs, networklevel, index = 'connectance')
net.metrics.h2 <- lapply(webs, networklevel, index = 'H2')
net.metrics.nodf <- lapply(webs, networklevel, index = 'NODF')



# make nulls ------
net.nulls.vaz <- lapply(webs, nullmodel, method = "vaznull", N = 500)


### +++++ nestedness --------

# calculate nestedness of nulls
net.null.nest = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'nestedness'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

vaz.nest <- net.null.nest(net.nulls.vaz)

# calculate z-scores

net.zscore = function(obsval, nullval) {
  (obsval - mean(nullval))/sd(nullval)  
} 

nest.zscore = function(nulltype){
  net.nest.zscore <- list() 
  for(i in 1:length(net.metrics.nest)){
    net.nest.zscore[[i]] = net.zscore(net.metrics.nest[[i]]['nestedness'], 
                                      nulltype[[i]][ ,'nestedness'])
  }
  names(net.nest.zscore) <- webs.names
  return(net.nest.zscore)
}

vaz.nest.zscore <- nest.zscore(vaz.nest)


# add z scores
add.pvalues = function(net.metric.zscore){
  # Change the output class from list of a list into a matrix
  net.metric.zscore <- do.call('rbind', net.metric.zscore) 
  
  # Convert z-scores to p-values (two-sided)
  net.metric.pvalue <- 2*pnorm(-abs(net.metric.zscore))
  
  # Change matrix into a dataframe
  net.metric.pvalue <- as.data.frame(as.table(net.metric.pvalue))
  colnames(net.metric.pvalue) <- c('site', 'metric', 'pvalue')
  
  net.metric.pvalue <- within(net.metric.pvalue, {
    significance <- ifelse(pvalue <= 0.001, "***", 
                           ifelse(pvalue <= 0.01, "**",
                                  ifelse(pvalue <= 0.05, "*", "not significant")))
  })
  return(net.metric.pvalue)
} 


vaz.test.nest <- add.pvalues(vaz.nest.zscore)
qt(vaz.test.nest$pvalue, 499)
# tstatistics: 
#-2.5544350
#-0.6607475


# print p-values of zscores

print(vaz.test.nest)













### +++++ interaction evenness --------

# calculate inteve of nulls
net.null.inteve = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'interaction evenness'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

vaz.inteve <- net.null.inteve(net.nulls.vaz)

# calculate z-scores

inteve.zscore = function(nulltype){
  net.inteve.zscore <- list() 
  for(i in 1:length(net.metrics.evenness)){
    net.inteve.zscore[[i]] = net.zscore(net.metrics.evenness[[i]]['interaction evenness'], 
                                      nulltype[[i]][ ,'interaction evenness'])
  }
  names(net.inteve.zscore) <- webs.names
  return(net.inteve.zscore)
}

vaz.inteve.zscore <- inteve.zscore(vaz.inteve)


# add z scores

vaz.test.inteve <- add.pvalues(vaz.inteve.zscore)

qt(vaz.test.inteve$pvalue, 499)
# tstatistics: 
#-34.0664169
#-0.7612433


# print p-values of zscores

print(vaz.test.inteve)









### +++++ h2 --------

# calculate h2 of nulls
net.null.h2 = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'H2'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

vaz.h2 <- net.null.h2(net.nulls.vaz)

# calculate z-scores

h2.zscore = function(nulltype){
  net.h2.zscore <- list() 
  for(i in 1:length(net.metrics.h2)){
    net.h2.zscore[[i]] = net.zscore(net.metrics.h2[[i]]['H2'], 
                                        nulltype[[i]][ ,'H2'])
  }
  names(net.h2.zscore) <- webs.names
  return(net.h2.zscore)
}

vaz.h2.zscore <- h2.zscore(vaz.h2)


# add z scores

vaz.test.h2 <- add.pvalues(vaz.h2.zscore)


# print p-values of zscores

print(vaz.test.h2)

qt(vaz.test.h2$pvalue, 499)
# tstatistics: 
#-8.858786
#-6.731044










### +++++ connectance --------

# calculate connectance of nulls
net.null.connectance = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'connectance'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

vaz.connectance <- net.null.connectance(net.nulls.vaz)

# calculate z-scores

connectance.zscore = function(nulltype){
  net.connectance.zscore <- list() 
  for(i in 1:length(net.metrics.connect)){
    net.connectance.zscore[[i]] = net.zscore(net.metrics.connect[[i]]['connectance'], 
                                    nulltype[[i]][ ,'connectance'])
  }
  names(net.connectance.zscore) <- webs.names
  return(net.connectance.zscore)
}

vaz.connectance.zscore <- connectance.zscore(vaz.connectance)


# add z scores

vaz.test.connectance <- add.pvalues(vaz.connectance.zscore)


# print p-values of zscores

print(vaz.test.connectance)
# connectance remains the same in vaz.nulls so this makes sense that it is the same.















### +++++ NODF --------

# calculate nodf of nulls
net.null.nodf = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'NODF'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

vaz.nodf <- net.null.nodf(net.nulls.vaz)

# calculate z-scores

nodf.zscore = function(nulltype){
  net.nodf.zscore <- list() 
  for(i in 1:length(net.metrics.nodf)){
    net.nodf.zscore[[i]] = net.zscore(net.metrics.nodf[[i]]['NODF'], 
                                    nulltype[[i]][ ,'NODF'])
  }
  names(net.nodf.zscore) <- webs.names
  return(net.nodf.zscore)
}

vaz.nodf.zscore <- nodf.zscore(vaz.nodf)


# add z scores

vaz.test.nodf <- add.pvalues(vaz.nodf.zscore)


# print p-values of zscores

print(vaz.test.nodf)

qt(vaz.test.nodf$pvalue, 499)
# tstatistics: 
#-9.4598472
#-0.7830527



# +++++ combine results into one table ---------


tests <- rbind(vaz.test.nest, 
               vaz.test.h2, 
               vaz.test.inteve,
               vaz.test.nodf) %>% 
  select(metric, everything())





# +++++ ------
# Visualizations-------

# Order of species list by phylogenetic order ----
# not completely ordered correctly:
# ordered by family for bees: https://www.researchgate.net/figure/Family-and-subfamily-level-phylogeny-for-bees-based-on-Danforth-et-al-2013_fig1_332959316
# ordered to order for plants: https://www.researchgate.net/publication/279592674_An_ordinal_classification_for_the_families_of_flowering_plants


# ///// for GloBI data -------

plant_phylog <- read_csv("Data/plant_phylog.csv")
bee_phylog <- read_csv("Data/bee_phylog.csv")

#make phylogeny list for globi plant genus
glob_plant_order.gen <- plant_phylog %>% 
  right_join(globi_filtered, by = "plant_order") %>%
  separate(resolvedPlantNames, into = c("genus", NA), sep = " ", remove = FALSE) %>% 
  arrange(plant_phylog, genus)

# order by genus
glob_order.gen <- list(
  seq.high = unique(glob_bee_order$resolvedBeeNames),
  seq.low = unique(glob_plant_order.gen$genus)
)



# ///// for modelled data -------


# make phylogeny list for modeled plant list
mod_plant_list <- as.data.frame(read_csv("Data/plant_phylog_modellist.csv")) %>% 
  filter(!is.na(scientificName))

# check for missing species names
# mod_plant_missing <- anti_join(cut_df,
#                               mod_plant_list, 
#                               by = c("plant.names" = "scientificName"))

mod_plant_order.matchplant <- left_join(cut_df_matchplant, mod_plant_list, 
                                        by = c("plant.names" = "scientificName")) %>% 
  left_join(plant_phylog, by  = c("Order" = "plant_order")) %>% 
  mutate( Family =
    ifelse(resolvedPlantNames %in% c("Phacelia distans", "Phacelia ramosissima"), "Boraginaceae",Family)
  ) %>% 
  arrange(plant_phylog, Family, resolvedPlantNames)


# make phylogeny list for modeled bee list
mod_bee_list <- read_csv("Data/bee_phylog_modellist.csv")
# 23 bee species -- the same as the unique bees included in model cut off

mod_bee_phylog <- left_join(cut_df, mod_bee_list, by = "bee.names") %>% 
  left_join(bee_phylog, by = c("bee.family" = "bee_family")) %>% 
  arrange(bee_phylog, bee.names)


# make list to feed to network code
mod_order.matchplants <- list(
  seq.high = unique(mod_bee_phylog$bee.names),
  seq.low = unique(mod_plant_order.matchplant$plant.names)
)


# +++++ ------

# Plot modeled network -------

#matching plant list to output of globi network
plotweb(cut_matdat_matchplant, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        sequence = mod_order.matchplants
        #plot.axes = TRUE
)

visweb(cut_matdat_matchplant)



# +++++ ------

# Plot GloBI network -------
# with phylogenetic order


plotweb(globi_matdat.gen, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        sequence = glob_order.gen
)