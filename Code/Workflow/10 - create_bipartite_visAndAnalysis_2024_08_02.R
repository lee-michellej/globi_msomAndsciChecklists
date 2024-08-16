# ### Create example network visualizations for GloBI manuscript ###
# created by M. Lee 26 Sep 2022
# updated by M. Lee 02 Aug 2024




# This code does the following ---------------------------------------
# 1 - download cleaned interaction and network metric data

# 2 - compare networks
# calculate network metrics for modeled data
# calculate network metric for globi data
# calculate null models
# calculate network metrics for null models
# compare results

# 3 - create visualization
# subset the modeled and globi data to only one family
# create network visualizations




# Load libraries and set working directory ------------------------------


library(tidyverse)
library(ggplot2)
library(bipartite)
library(vegan)


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


# 1 - Main csv files for manipulation ------------------------
dat <- read_csv("./Data/2024 07 19 - bee-plant-mod-probabilities.csv") %>% 
  dplyr::select(-1) %>% 
  mutate(prob = max_prob * 10000,
         prob_100 = max_prob * 100,
         prob_10 = max_prob * 10)
# 41374 interactions
# 137 bee species
# 302 plant species

hist(dat$prob)
hist(dat$max_prob)
hist(dat$prob_10)

cut_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 1) %>% 
  separate(plant.names, into = c("genus", NA), sep = " ", remove = FALSE)


globi_dat <- read_csv("./Data/final-globi-list-clean 2024 04 07.csv") %>% 
  select(resolvedPlantNames, resolvedBeeNames, sourceTaxonFamilyName, targetTaxonOrderName, targetTaxonFamilyName) %>% 
  mutate(plant_order = ifelse(is.na(targetTaxonOrderName),
                              "Boraginales",
                              targetTaxonOrderName)) %>% 
  separate(resolvedPlantNames, into = c("genus", NA), sep = " ", remove = FALSE)
# 9555 interactions
# 89 bee species
# 172 plant species






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


# filter by cut off species lists above
unmatched.bees <- anti_join(globi_dat, bee.names, by = "resolvedBeeNames")
list(unique(unmatched.bees$resolvedBeeNames)) 

unmatched.plants <- anti_join(globi_dat, plant.names, by = "resolvedPlantNames")
list(unique(unmatched.plants$resolvedPlantNames))











# Convert data into matrices =========

cut_matdat <- as.data.frame(df2intmatrix(as.data.frame(cut_df), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))



globi_matdat <- as.data.frame(df2intmatrix(as.data.frame(globi_filtered), 
                                           varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                                           type.out = "array",
                                           emptylist = TRUE))



# 2 - network measures ------------------------------

# Aug 2 2024 note: There must be a cut off as there is no 0% probability of interactions in the modeled network

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




# 3 - null networks -------------

model_matrix <- df2intmatrix(as.data.frame(cut_df), 
                             varnames = c("plant.names", "bee.names", "prob_10"),
                             type.out = "array",
                             emptylist = TRUE)


globi_matrix <- df2intmatrix(as.data.frame(globi_dat), 
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











# 4 - make visualizations ------------

# first attempt with only apidae family
# this file is likely not complete -- maybe check with Katja if missing anything major
apis <- read_csv("./Data/apidae.csv")

apis_df <- cut_df %>% 
  left_join(bee.list, by = c("bee.names" = "resolvedBeeNames")) %>% 
  dplyr::rename(plant.genus = genus.x,
                bee.genus = genus.y) %>% 
  left_join(apis, by = c("bee.genus" = "genus")) %>% 
  filter(!is.na(family)) %>% 
  filter(prob_10 == 10)

apid_matdat <- as.data.frame(df2intmatrix(as.data.frame(apis_df), 
                                         varnames = c("plant.names", "bee.names", "prob_10"),
                                         type.out = "array",
                                         emptylist = TRUE))

plotweb(apid_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        #sequence = mod_order.matchplants
        #plot.axes = TRUE
)



# take the globi_dat dataset
# join with santa cruz island bee list
# filter for bees in apidae
# may need to filter for plants in the cut model network as well

visglobi <- filter(globi_dat, 
                   globi_dat$resolvedBeeNames %in% apis_df$bee.names)

visglobi_matdat <- as.data.frame(df2intmatrix(as.data.frame(visglobi), 
                                          varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                                          type.out = "array",
                                          emptylist = TRUE))

plotweb(visglobi_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        x.lim = c(0, 2.2),
        #sequence = mod_order.matchplants
        #plot.axes = TRUE
)
# make visualizations





# # 5 - make heat map visualization ----------
# 
# # use the full dataset
# # continue filtering down until legible
# library(viridis)
# 
# globi_dat <- read_csv("./Data/final-globi-list-clean 2024 04 07.csv") %>% 
#   select(resolvedPlantNames, resolvedBeeNames, sourceTaxonFamilyName, targetTaxonOrderName, targetTaxonFamilyName) %>% 
#   mutate(plant_order = ifelse(is.na(targetTaxonOrderName),
#                               "Boraginales",
#                               targetTaxonOrderName)) %>% 
#   separate(resolvedPlantNames, into = c("genus", NA), sep = " ", remove = FALSE)
# 
# sum_globi_dat <- 
# 
# ggplot(globi_dat, aes(reorder(bee.names, -max_prob), 
#                 reorder(plant.names, -max_prob), 
#                 fill= max_prob)) + 
#   geom_tile() +
#   scale_fill_viridis_c() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

