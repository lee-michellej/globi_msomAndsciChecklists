# ### Create example network visualizations for GloBI manuscript ###
# created by M. Lee 26 Sep 2022
# updated by M. Lee 02 Aug 2024
# updated by M. Lee 11 Feb 2025




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
#dat <- read_csv("./Data/2025 02 11 - bee-plant-mod-probabilities.csv") %>% 
dat <- read_csv("~/Downloads/2025 08 05 - bee-plant-mod-probabilities.csv") %>% 
  dplyr::select(-1) %>% 
  mutate(prob = max_prob * 10000,
         prob_100 = max_prob * 100,
         prob_10 = max_prob * 10)
# 40166 interactions
# 133 bee species
# 302 plant species

hist(dat$prob)
hist(dat$max_prob)
hist(dat$prob_10)

cut_df <- dat %>% 
  dplyr::select(bee.names, plant.names, prob_10) %>% 
  dplyr::filter(prob_10 >= 5) %>% 
  separate(plant.names, into = c("genus", NA), sep = " ", remove = FALSE)


globi_dat <- read_csv("./Data/final-globi-list-clean 2024 04 07.csv") %>% 
  select(resolvedPlantNames, resolvedBeeNames, sourceTaxonFamilyName, targetTaxonOrderName, targetTaxonFamilyName) %>% 
  mutate(plant_order = ifelse(is.na(targetTaxonOrderName),
                              "Boraginales",
                              targetTaxonOrderName)) %>% 
  separate(resolvedPlantNames, into = c("genus", NA), sep = " ", remove = FALSE) %>% 
  # need to remove these bee species (2025 update): 
  #[1] "Eucera edwardsii"    "Colletes kincaidii"  "Eucera lunata"       "Bombus occidentalis"
  filter(resolvedBeeNames != "Eucera edwardsii",
        resolvedBeeNames != "Colletes kincaidii",
        resolvedBeeNames !="Eucera lunata",
        resolvedBeeNames !="Bombus occidentalis")
# 9335 interactions
# 85 bee species
# 172 plant species


# make summary file for Katja here (14 feb 25)
globi_simplified <- globi_dat %>% 
  group_by_all() %>% 
  count()

#write.csv(globi_simplified, "./Data/globiIntSimplified_14feb25.csv")



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



globi_matdat <- as.data.frame(df2intmatrix(as.data.frame(globi_dat), 
                                           varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                                           type.out = "array",
                                           emptylist = TRUE))



# 2 - network measures ------------------------------

# Aug 2 2024 note: There must be a cut off as there is no 0% probability of interactions in the modeled network

# calculate some basic metrics for the model
model_metrics <- as.data.frame(networklevel(cut_matdat, index = c(#'nestedness', 
                                                           'connectance',
                                                           'interaction evenness',
                                                           'NODF',
                                                           'H2')))
names(model_metrics)[1] <- "model_values"




# calculate some basic metrics for the globi data
globi_metrics <- as.data.frame(networklevel(globi_matdat, index = c(#'nestedness', 
                                                                 'connectance',
                                                                 'interaction evenness',
                                                                 'NODF',
                                                                 'H2')))
names(globi_metrics)[1] <- "globi_values"

tglobi_metrics <- as.data.frame(t(globi_metrics))




# combine two dataframes
metrics <- cbind(model_metrics, globi_metrics)




# 2.5 -- figure for range of cut offs =========

cutoffs <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
cutoff_values <- data.frame(index = c('connectance', 
                                      #'nestedness',
                                      'NODF',
                                      'interaction evenness',
                                      'H2'))

# for every i in cutoffs
for(i in unique(cutoffs)){
  
  # take modeled df (dat) and then filter (cutoff_df)
  cutoff_df <- dat %>% 
    dplyr::select(bee.names, plant.names, prob_10) %>% 
    dplyr::filter(prob_10 >= i) %>% 
    separate(plant.names, into = c("genus", NA), sep = " ", remove = FALSE)
  
  # convert df into matrix
  cutoff_mat <- as.data.frame(df2intmatrix(as.data.frame(cutoff_df), 
                                           varnames = c("plant.names", "bee.names", "prob_10"),
                                           type.out = "array",
                                           emptylist = TRUE))
  
  # calculate some basic metrics for the model
  cutoff_metrics <- as.data.frame(networklevel(cutoff_mat, index = c('connectance', 
                                                                    #'nestedness',
                                                                    'NODF',
                                                                    'interaction evenness',
                                                                    'H2')))
  names(cutoff_metrics)[1] <- i
  add <- cutoff_metrics[1]
  
  # paste to a dataframe
  
  cutoff_values <- cbind(cutoff_values, add)
  
  
}



# plot data with GloBI as reference

plotdf <- pivot_longer(cutoff_values,
                       c(2:12),
                       names_to = "cutoff")
plotdf$cutoff <- as.numeric(plotdf$cutoff)

plotglobi <- globi_metrics %>% 
  rownames_to_column("index")
  

ggplot(plotdf, aes(x = (cutoff*10), y = value)) +
  geom_point() +
  facet_wrap("index", scales = "free_y") +
  theme_bw(base_size = 12) +
  xlab("Probability cut off (%)") +
  ylab("Metric value") +
  geom_hline(data = plotglobi, 
             aes(yintercept = globi_values), 
             color = "red",
             linewidth = 1,
             linetype = 2)
ggsave("./Figures/Bipartite_figures/varyCutOffs_07aug25.jpeg",
       width = 6,
       height = 4)









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
# [1]      -Inf -12.36888

# print p-values of zscores

print(vaz.test.nest)
#site     metric       pvalue significance
#1 model nestedness 0.000000e+00          ***
#2 globi nestedness 3.861452e-31          ***











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
# [1]      -Inf -40.34883


# print p-values of zscores

print(vaz.test.inteve)
#    site               metric        pvalue significance
#1 model interaction evenness  0.000000e+00          ***
#2 globi interaction evenness 1.607296e-159          ***








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
#[1] -6.494956      -Inf

# tstatistics: 
#-8.858786
#-6.731044

#site metric       pvalue significance
#1 model     H2 1.005677e-10          ***
#  2 globi     H2 0.000000e+00          ***








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
bee.list <- read_csv("./Data/bees-SCI_2021_08_04.csv")

apis_df1 <- cut_df %>% 
  left_join(bee.list, by = c("bee.names" = "ScientificName")) %>% 
  dplyr::rename(plant.genus = genus,
                bee.genus = Genus) %>% 
  left_join(apis, by = c("bee.genus" = "genus")) %>% 
  filter(!is.na(family)) %>% 
  filter(prob_10 > 5)

apis_df <- filter(apis_df1, 
                  apis_df1$plant.names %in% globi_dat$resolvedPlantNames)

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





# 5 - Diadasia visualization ------------

# first attempt with only Diadasia genus

dia_df1 <- cut_df %>% 
  left_join(bee.list, by = c("bee.names" = "scientificName")) %>% 
  dplyr::rename(plant.genus = genus.x,
                bee.genus = genus.y) %>% 
  left_join(apis, by = c("bee.genus" = "genus")) %>% 
  filter(!is.na(family)) %>% 
  filter(prob_10 > 5) %>% 
  filter(bee.genus == "Diadasia")


dia_matdat <- as.data.frame(df2intmatrix(as.data.frame(dia_df1), 
                                          varnames = c("plant.names", "bee.names", "prob_10"),
                                          type.out = "array",
                                          emptylist = TRUE))

plotweb(dia_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        #x.lim = c(0, 2.2),
        #sequence = mod_order.matchplants
        #plot.axes = TRUE
)





visdia <- globi_dat %>% 
  separate(resolvedBeeNames, into = c("bee.genus", "bee.spp"), remove = F) %>% 
  filter(bee.genus == "Diadasia")

visdia_matdat <- as.data.frame(df2intmatrix(as.data.frame(visdia), 
                                              varnames = c("resolvedPlantNames", "resolvedBeeNames"),
                                              type.out = "array",
                                              emptylist = TRUE))

plotweb(visdia_matdat, method = "normal", empty = TRUE, arrow = "no",
        col.interaction = adjustcolor("cornsilk3"),
        col.high = "goldenrod",
        col.low = "olivedrab4",
        bor.col.interaction = NA,
        bor.col.high = NA,
        bor.col.low = NA,
        text.rot = 90,
        y.lim = c(-1.55,3.25),
        #x.lim = c(0, 2.2),
        #sequence = mod_order.matchplants
        #plot.axes = TRUE
)







# # 6 - check specialization of specific species ========
# 
# modelPlantFam <- read.csv("./Data/resolvedplantsci_12feb24.csv") %>% 
#   select(scientificName, Family) %>% 
#   rename("plant.names" = "scientificName")
# 
# cut_df_fam <- cut_df %>% 
#   left_join(modelPlantFam, by = "plant.names")
# 
# cut_matdat_fam <- as.data.frame(df2intmatrix(as.data.frame(cut_df_fam), 
#                                          varnames = c("Family", "bee.names", "prob_10"),
#                                          type.out = "array",
#                                          emptylist = TRUE))
# 
# globi_matdat_fam <- as.data.frame(df2intmatrix(as.data.frame(globi_dat), 
#                                            varnames = c("targetTaxonFamilyName", "resolvedBeeNames"),
#                                            type.out = "array",
#                                            emptylist = TRUE))
# 
# 
# 
# modelspec <- specieslevel(cut_matdat_fam,
#                           index = c("d", "degree"),
#                           level = "higher")
# 
# globispec <- specieslevel(globi_matdat_fam,
#                           index = c("d", "degree"),
#                           level = "higher")
# 
# 
# speclevel_model <- modelspec %>% 
#   rename(spec_model = d,
#          links_model = degree) %>% 
#   rownames_to_column("bee")
# 
# speclevel <- globispec %>% 
#   rename(spec_glob = d,
#          links_glob = degree) %>% 
#   rownames_to_column("bee") %>% 
#   left_join(speclevel_model, "bee")
# 
# 
# 
# 
# 
# # 7 - Pearson Correlation between model and globi species values =====
# 
# # read in file from Katja about SCI bee specialization
# specialization <- read.csv("./Data/scir_beespec_25feb25.csv") %>% 
#   select(ScientificName, dietBreadth) %>% 
#   rename("bee" = "ScientificName") %>% 
#   right_join(speclevel, by = "bee")
# #write.csv(specialization, "./Figures/Bipartite_figures/specLevelOutput_25feb25.csv", row.names = F)
# 
# # pearson correlation
# correlation <- cor(specialization$links_glob, specialization$links_model, method = 'pearson')
# correlation
# # 0.1801355
# 
# 
# 
# 
# ggplot(specialization, aes(x = links_glob, y = links_model)) +
#   geom_smooth(method = "lm", color = "black", se = F, linetype = 2) +
#   geom_point(aes(color = dietBreadth)) +
#   xlab("GloBI network family links") +
#   ylab("Modeled network family links")
# ggsave("./Figures/Bipartite_figures/beeSpecializationLinks_25feb25.jpeg",
#        width = 6,
#        height = 4)
# 
# # pearson correlation
# correlation <- cor(specialization$links_glob, specialization$links_model, method = 'pearson')
# correlation
# # 0.1801355
# 
# ggplot(specialization, aes(x = spec_glob, y = spec_model)) +
#   geom_smooth(method = "lm", color = "black", se = F, linetype = 2) +
#   geom_point(aes(color = dietBreadth)) +
#   xlab("GloBI network specialization") +
#   ylab("Modeled network specialization")
# ggsave("./Figures/Bipartite_figures/beeSpecializationdPrime_25feb25.jpeg",
#        width = 6,
#        height = 4)
# 
# # pearson correlation 2
# correlation2 <- cor(specialization$spec_glob, specialization$spec_model, method = 'pearson')
# correlation2
# # 0.207
