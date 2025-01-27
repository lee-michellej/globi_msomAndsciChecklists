#######################################
#######################################
## Author: Dr. Graziella DiRenzo
##
## Date Created: 2025-01-27
##
## Copyright (c) Graziella DiRenzo, 2025
## Email: gdirenzo@umass.edu
#######################################
#######################################


#######################################
## Code objectives:
#######################################


# To write code that compares the output of 6 different Globi models

# The model types & names are:
  # 1. No bee/plant specification = no_bee_plant
  # 2. bee-specific intercepts = bee_species
  # 3. plant-specific intercepts = plant_species
  # 4. bee family intercepts = bee_family
  # 5. plant family intercepts = plant_family
  # 6. bee and plant family intercepts = bee_plant_family



#######################################
## Output of code:
#######################################


# The statistics and figures that compare the output of 6 different models


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
library(tidyverse)
library(ggplot2)
library(jagsUI)
library(ggridges)
library(plyr)
library(coda)
library(reshape2)
library(mcmcr)
library(wesanderson)



# Set working directory
setwd("~/")
setwd("/Users/gdirenzo/OneDrive - University of Massachusetts/Dropbox_transfer/Globi/")





# 2. Load data -------------------------------------------------------


# date object for folder
date <- "2025 01 26"



#---- bee_species

# Model name
mod_name <- "bee-species-mod"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_species <- out

out_bee_species_df <- as.data.frame(out_bee_species)


#---- bee_family

# Model name
mod_name <- "bee-family-mod"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_family <- out

out_bee_family_df <- as.data.frame(out_bee_family)



#---- plant_species

# Model name
mod_name <- "plant-species-mod"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

# Save the out object with a new model specific name
out_plant_species <- out

out_plant_species_df <- as.data.frame(out_plant_species)



#---- plant_family

# Model name
mod_name <- "plant-family-mod"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

# Save the out object with a new model specific name
out_plant_family <- out

out_plant_family_df <- as.data.frame(out_plant_family)



#---- bee_plant_family

# Model name
mod_name <- "bee-plant-family-mod"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_plant_family <- out

out_bee_plant_family_df <- as.data.frame(out_bee_plant_family)



#---- no_bee_plant

# Model name
mod_name <- "no-bee-plant-mod"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

# Save the out object with a new model specific name
out_no_bee_plant <- out

out_no_bee_plant_df <- as.data.frame(out_no_bee_plant)





# 3. Set universal text size for plots ----------------------------------------




# Specify text size
text.size <- 12
title.size <- 12







# 4. Plot the relationship between psi and bee size ----------------------------------------



# Write function for psi vs bee size plot
bee_size_plot <- function(out_df, 
                          beta,      # For all it will be = out_df$beta_psi.2.
                          intercept, # For most it will be: out_df$beta_psi.1.
                          mod_name){

# Create empty data frame
stats_size <- data.frame(mod_name = rep(mod_name, times = 2),
                         size_beta = c("greater than 0", "less than 0"),
                         proportion = c(NA, NA))
  

# Calcualte the statistics:

  # What proportion of the mass is > 0?
stats_size[1, 3] <- mean(beta > 0)

  # What proportion of the mass is < 0?
stats_size[2, 3] <- mean(beta < 0)

  
# # Create a dataframe with the parameter estimates from each MCMC iteraction
param_df <- data.frame(beta_psi_B_size = beta, 
                       intercept = intercept,
                       iteration = 1:length(intercept))

## Add a column with the covariate
pred.psi <- expand_grid(param_df, 
                        tibble(Size.scaled = seq(-3, 3, length.out = 50)))


# Add predictions
pred.psi$pred <- plogis(pred.psi$intercept + 
                        pred.psi$beta_psi_B_size * pred.psi$Size.scaled)

# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(pred.psi), 50000, replace = FALSE)

# Subset the data
pred.psi.sub <- pred.psi[pred.psi$iteration %in% sub.samp,]

# Calculate the mean value for the relationship
mean_psi_size <- data.frame(Size.scaled = seq(-3, 3, length.out = 50))

mean_psi_size$pred <- plogis(mean(param_df$intercept) +
                             mean(param_df$beta_psi_B_size) * mean_psi_size$Size.scaled)

# Create plot
gplot <- ggplot() +
  geom_line(data = pred.psi.sub, aes(x = as.numeric(Size.scaled), 
                                     y = as.numeric(pred), 
                                     col = as.factor(iteration)), 
            alpha = .4) +
  geom_line(data = mean_psi_size, aes(x = Size.scaled,
                                      y = pred), col = "black")+
  ylab("Probability of interacting \nwith a plant")+
  xlab("Bee size standardized")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))+
  ggtitle(mod_name)

return(list(gplot = gplot,
            stats_size = stats_size))

}





# 5. Make psi and bee size plots & calcualte stats ----------------------------------------



# Bee species model
bee_size_bee_species_mod <- bee_size_plot(out_df = out_bee_species_df, 
                                      beta = out_bee_species_df$beta_psi.2.,      # For all it will be = out_df$beta_psi.2.
                                      intercept = out_bee_species_df$beta_psi.1., # For most it will be: out_df$beta_psi.1.
                                      mod_name = "Bee species model")

# Bee family model
bee_size_bee_family_mod <- bee_size_plot(out_df = out_bee_family_df, 
                                          beta = out_bee_family_df$beta_psi.2.,      # For all it will be = out_df$beta_psi.2.
                                          intercept = out_bee_family_df$beta_psi.1., # For most it will be: out_df$beta_psi.1.
                                          mod_name = "Bee family model")



# Plant species model
bee_size_plant_species_mod <- bee_size_plot(out_df = out_plant_species_df, 
                                          beta = out_plant_species_df$beta_psi.2.,      # For all it will be = out_df$beta_psi.2.
                                          intercept = out_plant_species_df$beta_psi.1., # For most it will be: out_df$beta_psi.1.
                                          mod_name = "Plant species model")

# Plant family model
bee_size_plant_family_mod <- bee_size_plot(out_df = out_plant_family_df, 
                                          beta = out_plant_family_df$beta_psi.2.,      # For all it will be = out_df$beta_psi.2.
                                          intercept = out_plant_family_df$beta_psi.1., # For most it will be: out_df$beta_psi.1.
                                          mod_name = "Plant family model")


# Bee and Plant family model
bee_size_bee_plant_family_mod <- bee_size_plot(out_df = out_bee_plant_family_df, 
                                           beta = out_bee_plant_family_df$beta_psi.2.,      # For all it will be = out_df$beta_psi.2.
                                           intercept = out_bee_plant_family_df$beta_psi.1., # For most it will be: out_df$beta_psi.1.
                                           mod_name = "Bee and plant family model")



# Bee and Plant family model
bee_size_no_bee_plant_mod <- bee_size_plot(out_df = out_no_bee_plant_df, 
                                               beta = out_no_bee_plant_df$beta_psi.2.,      # For all it will be = out_df$beta_psi.2.
                                               intercept = out_no_bee_plant_df$beta_psi.1., # For most it will be: out_df$beta_psi.1.
                                               mod_name = "No bee and plant model")



# Stitch the plots together
(bee_size_bee_species_mod$gplot + bee_size_bee_family_mod$gplot+
 bee_size_plant_species_mod$gplot + bee_size_plant_family_mod$gplot+
 bee_size_bee_plant_family_mod$gplot + bee_size_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-bee-size-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
bee_size_stats <- rbind(bee_size_bee_species_mod$stats_size, bee_size_bee_family_mod$stats_size,
                        bee_size_plant_species_mod$stats_size, bee_size_plant_family_mod$stats_size,
                        bee_size_bee_plant_family_mod$stats_size, bee_size_no_bee_plant_mod$stats_size )



# Save the table
write.csv(paste0("./Tables/", date_folder, "/Supp-table-bee-size-mod-comparison.csv"))







########----------------------################
########----------------------################
#------ I stopped coding here ------- determine if you want plots & stats for each relationship
########----------------------################
########----------------------################









# 8. Plot the relationship between psi and solitary ----------------------------------------






# What proportion of the mass is < 0?
mean(out_df$beta_psi.1. < out_df$beta_psi.3.)




# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.soc <- data.frame(beta_psi_B_solitary = out_df$beta_psi.3., 
                         mu.psi = out_df$beta_psi.1.,
                         iteration = 1:length(out_df$beta_psi.1.))

## Add a column with the covariate
pred.p.soc$solitary <- plogis(pred.p.soc$mu.psi + pred.p.soc$beta_psi_B_solitary) 
pred.p.soc$notSolitary <- plogis(pred.p.soc$mu.psi) 

# Convert to long format
pred.p.soc.long <- melt(pred.p.soc[,4:5])

# Change column names
colnames(pred.p.soc.long) <- c("solitary", "probability")

# Change labels
pred.p.soc.long$solitary <- ifelse(pred.p.soc.long$solitary == "solitary",
                                   "Solitary",
                                   "Not solitary")

# Calcilate group means
mu <- ddply(pred.p.soc.long, "solitary", summarise, 
            grp.mean=mean(probability))

head(mu)




# Create the plot
solitary.plot <- ggplot(pred.p.soc.long, aes(x = probability, y = solitary)) +
  geom_density_ridges(aes(fill = solitary)) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", 2, type = c("discrete")))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=solitary),
             linetype="dashed")+
  scale_color_manual(values = wes_palette("GrandBudapest2", 2, type = c("discrete")))+
  xlab("Probability of interacting \nwith a plant")+
  ylab("Solitary")+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))

# solitary.plot
# # # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-bee-solitary.pdf"), 
#        height = 3, width = 5)











# 9. Plot the relationship between psi and flower color ----------------------------------------



# Probability of one parameter > another

# yellow > blue
mean(out_df$beta_psi.4. > out_df$beta_psi.5.)

# yellow > white
mean(out_df$beta_psi.4. > out_df$beta_psi.6.)

# yellow > other
mean(out_df$beta_psi.4. > out_df$beta_psi.1.)



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.psi.col <- data.frame(beta_psi_yellow = out_df$beta_psi.4., 
                           beta_psi_blue = out_df$beta_psi.5., 
                           beta_psi_white = out_df$beta_psi.6., 
                           mu.psi = out_df$beta_psi.1.,
                           iteration = 1:length(out_df$beta_psi.1.))


## Add a column with the covariate
pred.psi.col$yellow <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_yellow) 
pred.psi.col$blue <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_blue) 
pred.psi.col$white <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_white) 
pred.psi.col$other <- plogis(pred.psi.col$mu.psi) 

# Convert to long format
pred.psi.col.long <- melt(pred.psi.col[,6:9])

# Change column names
colnames(pred.psi.col.long) <- c("color", "probability")

# Calcilate group means
mu <- ddply(pred.psi.col.long, "color", summarise, 
            grp.mean=mean(probability))

head(mu)



# 
annotation_df <- data.frame(
  start =  c("yellow", "yellow"), 
  end = c("blue", "white"),
  probability = c(1, 1.2),
  label = c(mean(out_df$beta_psi.4. > out_df$beta_psi.5.),
            mean(out_df$beta_psi.4. > out_df$beta_psi.6.))
)

pred.psi.col.long$color_num <- as.numeric(pred.psi.col.long$color)

# Annotation data frame
annotation_df <- data.frame(
  start =  c("yellow", "yellow", "yellow"),
  end = c("blue", "white", "other"),
  probability = c(1.1, 1.2, 1.3),
  label = c(mean(out_df$beta_psi.4. > out_df$beta_psi.5.),
            mean(out_df$beta_psi.4. > out_df$beta_psi.6.),
            mean(out_df$beta_psi.4. > out_df$beta_psi.1.))
)

# Create a mapping of color to numeric positions
color_levels <- levels(pred.psi.col.long$color)
color_map <- setNames(seq_along(color_levels), color_levels)

# Adjust annotation_df to use numeric positions
annotation_df$y_start <- color_map[annotation_df$start]
annotation_df$y_end <- color_map[annotation_df$end]


# Create the plot
flower.color.plot <- ggplot(pred.psi.col.long, aes(x = probability, y = color)) +
  geom_density_ridges(aes(fill = color)) +
  scale_fill_manual(values = c("#E7B800","#00AFBB",  "grey", "black"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=color),
             linetype="dashed")+
  scale_color_manual(values = c("#E7B800","#00AFBB", "grey", "black"))+
  xlab("Probability of interacting \nwith a bee")+
  ylab("Flower color")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))+
  geom_segment(data = annotation_df, aes(x = probability, 
                                         xend = probability,
                                         y = y_start, yend = y_end),
               color = "black", size = 0.5) +
  geom_segment(data = annotation_df, aes(x = probability, 
                                         xend = probability - 0.03,
                                         y = y_start, yend = y_start),
               color = "black", size = 0.5) +
  geom_segment(data = annotation_df, aes(x = probability, 
                                         xend = probability - 0.03,
                                         y = y_end, yend = y_end),
               color = "black", size = 0.5) +
  geom_text(data = annotation_df, aes(x = probability + 0.05, 
                                      y = (y_start + y_end) / 2,
                                      label = round(label, 2)), size = 3,
            hjust = 0.5, vjust = 0)

flower.color.plot
# 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-flower-color.pdf")
#        , height = 10, width = 11)
# 
# 









# 10. Plot the relationship between psi and flower shape ----------------------------------------




# Probability of not bowl >  bowl
mean(out_df$beta_psi.1. > out_df$beta_psi.7.)



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.psi.shape <- data.frame(beta_psi_F_shape = out_df$beta_psi.7., 
                             mu.psi = out_df$beta_psi.1.,
                             iteration = 1:length(out_df$beta_psi.1.))

## Add a column with the covariate
pred.psi.shape$bowl <- plogis(pred.psi.shape$mu.psi + pred.psi.shape$beta_psi_F_shape) 
pred.psi.shape$notBowl <- plogis(pred.psi.shape$mu.psi) 

# Convert to long format
pred.psi.shape.long <- melt(pred.psi.shape[,4:5])

# Change column names
colnames(pred.psi.shape.long) <- c("bowl", "probability")

# Change labels
pred.psi.shape.long$bowl <- ifelse(pred.psi.shape.long$bowl == "bowl",
                                   "Bowl",
                                   "Not Bowl")

# Calcilate group means
mu <- ddply(pred.psi.shape.long, "bowl", summarise, 
            grp.mean=mean(probability))

head(mu)





# Create the plot
bowl.plot <- ggplot(pred.psi.shape.long, aes(x = probability, y = bowl)) +
  geom_density_ridges(aes(fill = bowl)) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", 2, type = c("discrete")))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=bowl),
             linetype="dashed")+
  scale_color_manual(values = wes_palette("GrandBudapest1", 2, type = c("discrete")))+
  xlab("Probability of interacting \nwith a bee")+
  ylab("Flower shape")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))

# bowl.plot
# 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-flower-shape.pdf")
#        , height = 10, width = 11)
# 



# 11. Combined figure ----------------------------------------



library(
  patchwork
)



(bee.size.plot + solitary.plot +
    flower.color.plot + bowl.plot) + 
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0("./Figures/", date_folder, "/Fig-2-Bee-plant-interaction-prob.png")
       , height = 8, width = 12)






# 11. Plot the relationship between p and strippiness ----------------------------------------



out_df <- as.data.frame(out)



mean(out_df$beta_p.2. > out_df$beta_p.1.)


# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_B_stripped = out_df$beta_p.2., 
                     mu.p = out_df$beta_p.1.,
                     iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p$stripped <- plogis(pred.p$mu.p + pred.p$beta_p_B_stripped) 
pred.p$notStripped <- plogis(pred.p$mu.p) 

# Convert to long format
pred.p.long <- melt(pred.p[,4:5])

# Change column names
colnames(pred.p.long) <- c("stripe", "probability")

# Change labels
pred.p.long$stripe <- ifelse(pred.p.long$stripe == "stripped",
                             "Stripped",
                             "Not \nstripped")

# Calculate group means
mu <- ddply(pred.p.long, "stripe", summarise, 
            grp.mean=mean(probability))

head(mu)



# Create the plot
strip.plot <- ggplot(pred.p.long, aes(x = probability, y = stripe)) +
  geom_density_ridges(aes(fill = stripe)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 2, type = c("discrete")))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=stripe),
             linetype="dashed")+
  scale_color_manual(values = wes_palette("Moonrise3", 2, type = c("discrete")))+
  xlab("Probability of detecting \nthe bee on a plant")+
  ylab("Stripiness")+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))
# strip.plot

# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-bee-stripes.pdf"), 
#        height = 3, width = 5)









# 12. Plot the relationship between p and source ----------------------------------------



# observation > literature
mean(out_df$beta_p.6. > out_df$beta_p.4.)

# observation > collection
mean(out_df$beta_p.6. > out_df$beta_p.5.)

# observation > aggregated
mean(out_df$beta_p.6. > out_df$beta_p.1.)



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_lit = out_df$beta_p.4., 
                     beta_p_col = out_df$beta_p.5., 
                     beta_p_obs = out_df$beta_p.6., 
                     mu.p = out_df$beta_p.1.,
                     iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p$Literature <- plogis(pred.p$mu.p + pred.p$beta_p_lit)  #literature
pred.p$Collection <- plogis(pred.p$mu.p + pred.p$beta_p_col)  #collection
pred.p$Observation <- plogis(pred.p$mu.p + pred.p$beta_p_obs)  #iNaturalist
pred.p$Aggregated  <- plogis(pred.p$mu.p)  # Aggregated

# Convert to long format
pred.p.long <- melt(pred.p[,6:9])

# Change column names
colnames(pred.p.long) <- c("citation", "probability")


# Calcilate group means
mu <- ddply(pred.p.long, "citation", 
            summarise, 
            grp.mean=mean(probability))

head(mu)




# Create the plot
citation.plot <- ggplot(pred.p.long, aes(x = probability, y = citation)) +
  geom_density_ridges(aes(fill = citation)) +
  scale_fill_manual(values = wes_palette("Moonrise2", 4, type = c("discrete")))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=citation),
             linetype="dashed")+
  scale_color_manual(values = wes_palette("Moonrise2", 4, type = c("discrete")))+
  xlab("Probability of detecting a \nbee-plant interaction")+
  ylab("Source type")+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))

# citation.plot
# 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-citation.pdf"), 
#        height = 3, width = 5)
# 









# 13. Plot the relationship between p and flower color ----------------------------------------




# yellow > blue
mean(out_df$beta_p.7. > out_df$beta_p.8.)

# yellow > white
mean(out_df$beta_p.7. > out_df$beta_p.9.)

# yellow > other
mean(out_df$beta_p.7. > out_df$beta_p.1.)




# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.col <- data.frame(beta_p_yellow = out_df$beta_p.7., 
                         beta_p_blue = out_df$beta_p.8., 
                         beta_p_white = out_df$beta_p.9., 
                         mu.p = out_df$beta_p.1.,
                         iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p.col$yellow <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_yellow)  # yellow
pred.p.col$blue <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_blue)  # blue
pred.p.col$white <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_white)  # white
pred.p.col$other <- plogis(pred.p.col$mu.p)  # other


# Convert to long format
pred.p.col.long <- melt(pred.p.col[,6:9])

# Change column names
colnames(pred.p.col.long) <- c("color", "probability")

# Calcilate group means
mu <- ddply(pred.p.col.long, "color", summarise, 
            grp.mean=mean(probability))

head(mu)


mean(out_df$beta_p.10. > out_df$beta_p.1.)

# Create the plot
flow.col.plot.p <- ggplot(pred.p.col.long, aes(x = probability, y = color)) +
  geom_density_ridges(aes(fill = color)) +
  scale_fill_manual(values = c( "#E7B800", "#00AFBB","grey", "black"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=color),
             linetype="dashed")+
  scale_color_manual(values = c("#E7B800","#00AFBB", "grey", "black"))+
  xlab("Probability of detecting a \nbee interacting on the plant")+
  ylab("Flower color")+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))

# flow.col.plot.p
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-color.pdf"), 
#        height = 3, width = 5)
# 
# 








# 14. Plot the relationship between p and flower shape ----------------------------------------



mean(out_df$beta_p.10. > out_df$beta_p.1.)



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.shape <- data.frame(beta_p_F_shape = out_df$beta_p.10., 
                           mu.p = out_df$beta_p.1.,
                           iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p.shape$bowl <- plogis(pred.p.shape$mu.p + pred.p.shape$beta_p_F_shape) 
pred.p.shape$notBowl <- plogis(pred.p.shape$mu.p) 

# Convert to long format
pred.p.shape.long <- melt(pred.p.shape[,4:5])

# Change column names
colnames(pred.p.shape.long) <- c("bowl", "probability")

# Change labels
pred.p.shape.long$bowl <- ifelse(pred.p.shape.long$bowl == "bowl",
                                 "Bowl",
                                 "Not \nbowl")

# Calcilate group means
mu <- ddply(pred.p.shape.long, "bowl", summarise, 
            grp.mean=mean(probability))

head(mu)



# Create the plot
flow.shape.plot.p <- ggplot(pred.p.shape.long, aes(x = probability, y = bowl)) +
  geom_density_ridges(aes(fill = bowl)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", 2, type = c("discrete")))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=bowl),
             linetype="dashed")+
  scale_color_manual(values = wes_palette("Darjeeling1", 2, type = c("discrete")))+
  xlab("Probability of detecting a \nbee interacting on the plant")+
  ylab("Flower shape")+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))

# flow.shape.plot.p
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-shape.pdf"), 
#        height = 3, width = 5)










# 15. Plot the relationship between p and bee size ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.size <- data.frame(beta_p_B_size = out_df$beta_p.3., 
                          mu.p = out_df$beta_p.1.,
                          iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p.size <- expand_grid(pred.p.size, 
                           tibble(Size.scaled = seq(-3, 3, length.out = 50)))


# Add predictions
pred.p.size$pred <- plogis(pred.p.size$mu.p + 
                             pred.p.size$beta_p_B_size * 
                             pred.p.size$Size.scaled)


# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(out_df), 500, replace = FALSE)

# Subset the data
pred.p.size.sub <- pred.p.size[pred.p.size$iteration %in% sub.samp,]





# Calculate the mean value for the relationship
mean_p_size <- data.frame(Size.scaled = seq(-3, 3, length.out = 50),
                          mu.p = mean(out_df$beta_p.1.),
                          beta_p_B_size = mean(out_df$beta_p.3.))


mean_p_size$pred <- plogis(mean_p_size$mu.p +
                             mean_p_size$beta_p_B_size * mean_p_size$Size.scaled)


# Create plot
bee.size.plot.p <- ggplot() +
  geom_line(data = pred.p.size.sub, aes(x = as.numeric(Size.scaled), 
                                        y = as.numeric(pred), 
                                        col = as.factor(iteration)), 
            alpha = .4) +
  geom_line(data = mean_p_size, aes(x = Size.scaled,
                                    y = pred), col = "black", lwd = 1)+
  
  ylab("Probability of detecting \nthe bee on a plant")+
  xlab("Bee size standardized")+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))

# bee.size.plot.p
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-bee-size.pdf"), 
#        height = 3, width = 5)
# 



# 11. Combined detection figure ----------------------------------------





((bee.size.plot.p + strip.plot + flow.col.plot.p + flow.shape.plot.p)/  citation.plot) + plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0("./Figures/", date_folder, "/Fig-3-Bee-plant-detection-prob.png")
       , height = 6, width = 15)




# End script