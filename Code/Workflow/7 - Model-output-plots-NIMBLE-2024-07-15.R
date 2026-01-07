########################################
########################################
# This code was written by: G. V. DiRenzo & M. J. Lee
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code Objective ##########
##################################


# To create plots from the model output. This script generates the main figures
# for the manuscript showing relationships between covariates and both
# interaction probability (psi) and detection probability (p).



##################################
######## Input Data ##############
##################################


# Model output files (from Script 5):
#   - out-bee_species-with-priors-1-NIMBLE.rds (model output)
#   - result-bee_species-with-priors-1-NIMBLE.rds (model results)
#   - MCMC_posterior_samples-bee_species-with-priors-1.rds (posterior samples)

# Covariate data (from Script 4):
#   - model_covariates - 2025 01 22 - no apis.rds

# Data info (from Script 3):
#   - dat_info_2025_01_22.rds

# Functions (from Script 6):
#   - 6 - functions for covariate plots.R



##################################
######## Output of Code ##########
##################################


# Creates figures in the main text of manuscript
  # Figure 1
  # Figure 2



################################## 
########  Table of Contents ######
################################## 


# 1. Load libraries & set working directory
# 2. Load data
# 3. Summarize the observed data, u, v, and z parameters/states
# 4. Make the Bee - plant interaction probability plot
# 5. Make the Bee - plant Detection probability plot 
# 6. Plot the total number of true vs. observed bee-plant interactions
# 7. Plot the relationship between psi and bee size
# 8. Plot the relationship between psi and solitary
# 9. Plot the relationship between psi and flower color
# 10. Plot the relationship between psi and flower shape
# 11. Plot the relationship between p and strippiness
# 12. Plot the relationship between p and source
# 13. Plot the relationship between p and flower color
# 14. Plot the relationship between p and flower shape
# 15. Plot the relationship between p and bee size
# 16. Plot the relationship between p and quadratic month


################################## 
################################## 
################################## 





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
library(patchwork)

# Add globi folder name
globi_out_folder <- ""
globi_result_folder <- ""
globi_MCMC_folder <- ""

# Add github folder path
github_folder_path <- ""



# date object for folder
date_folder <- "2025 08 05"
date <- "2025 01 26"





# 2. Load data -------------------------------------------------------






#---- bee_species

# Model name
mod_name <- "bee_species"

# Load the model output - out
load(paste0(globi_out_folder, "/out-bee_species-with-priors-1-NIMBLE.rds"))





#------------  Upload the data & format
# object name = bee.plant.cite
# 3-D array
load(paste0(github_folder_path, "/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds"))



# Read in the dat_info
load(paste0(github_folder_path, "/Data/dat_info_2025_01_22.rds"))
# object name = dat_info



# Load covariates
load(paste0(github_folder_path, "/Data/model_covariates - 2025 01 22 - no apis.rds"))
# object name = covariates



# Load in the number of observations
load(paste0(github_folder_path, "/Data/obs_dat-2025-02-11.rds"))
# object name = obs_dat





#---- bee_species - MCMC table


# Load the model output - out
load(paste0(globi_MCMC_folder, "/MCMClist-bee_species-with-priors-1-NIMBLE.rds"))

# Write Parameter table
write.csv(MCMCvis::MCMCsummary(MCMClist), file = paste0(github_folder_path, "/Tables/", date_folder, "/Table-1-", mod_name, "-MCMC-output.csv"))



# 4. Source the code for the functions ----------------------------------------


source(paste0(github_folder_path, "/Code/Workflow/6 - functions for covariate plots.R"))





# 7. Plot the relationship between psi and bee size ----------------------------------------




# Specify text size
text.size <- 12
title.size <- 12


out_df <- as.data.frame(out)

# Bee size effect
# Small bee (5mm) vs large bee (15mm)
# Extract min bee value
small_bee <- covariates$bee.covariates[which(covariates$bee.covariates$size == min(covariates$bee.covariates$size)),]$size_std[1]
# Extract max bee value
large_bee <- covariates$bee.covariates[which(covariates$bee.covariates$size == max(covariates$bee.covariates$size)),]$size_std[1]

# Calculate the probability of interacting with a plant:
small_bee_prob <- plogis(out_df$beta_psi.1. + out_df$beta_psi.2. * small_bee)
mean(small_bee_prob)
quantile(small_bee_prob, c(0.025, 0.975))

large_bee_prob <- plogis(out_df$beta_psi.1. + out_df$beta_psi.2. * large_bee)
mean(large_bee_prob)
quantile(large_bee_prob, c(0.025, 0.975))

small_vs_large <- small_bee_prob - large_bee_prob
mean(small_vs_large)
quantile(small_vs_large, c(0.025, 0.975))



# What proportion of the mass is > 0?
mean(out_df$beta_psi.2. > 0)

# What proportion of the mass is < 0?
mean(out_df$beta_psi.2. < 0)





# Bee species model
bee_size_bee_species_mod <- response_continous_cov_plot(out_df = out_df, 
                                                        beta = out_df$beta_psi.2.,     
                                                        intercept = out_df$beta_psi.1.,
                                                        mod_name = "",
                                                        x_lab_text = "Bee size standardized",
                                                        y_lab_text = "Probability of interacting \nwith a plant")




bee.size.plot <- bee_size_bee_species_mod$gplot
 
bee.size.plot

 # Save the plot
# ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Bee-plant-interaction-prob-V-bee-size.pdf") , height = 10, width =11)
 


 
 
 
 
 





# 8. Plot the relationship between psi and solitary ----------------------------------------






# What proportion of the mass is beta_psi.1. > (out_df$beta_psi.1. + beta_psi.3.)?
  # social bee > solitary bee
mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.3.))


 # Create a dataframe with the parameter estimates from each MCMC iteraction
 pred.p.soc <- data.frame(beta_psi_B_solitary = out_df$beta_psi.3., 
                          mu.psi = out_df$beta_psi.1.,
                          iteration = 1:length(out_df$beta_psi.1.))
 
 
# Calculate the probability of interacting with a plant:
solitary_prob <- plogis(pred.p.soc$mu.psi + pred.p.soc$beta_psi_B_solitary)
mean(solitary_prob)
quantile(solitary_prob, c(0.025, 0.975))

notSolitary_prob <- plogis(pred.p.soc$mu.psi)
mean(notSolitary_prob)
quantile(notSolitary_prob, c(0.025, 0.975))

# Calculate the difference in probabilities
prob_difference <- solitary_prob - notSolitary_prob

# Mean and 95% CI
mean(prob_difference)
quantile(prob_difference, c(0.025, 0.975))



# Bee species model
bee_soc_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                    num_cov = 2,
                                                    beta1 = out_df$beta_psi.1.,
                                                    beta2 = out_df$beta_psi.3.,
                                                    beta3 = NULL,
                                                    beta4 = NULL,
                                                    pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                    beta1_name = "social",
                                                    beta2_name = "solitary",
                                                    beta3_name = NULL,
                                                    beta4_name = NULL,
                                                    x_lab_text = "Probability of interacting \nwith a plant",
                                                    y_lab_text =  "Bee sociality",
                                                    mod_name = "")


# Create the plot
solitary.plot <- bee_soc_bee_species_mod$gplot

solitary.plot

# # # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-bee-solitary.pdf"), 
#        height = 3, width = 5)











# 9. Plot the relationship between psi and flower color ----------------------------------------







 # Create a dataframe with the parameter estimates from each MCMC iteraction
 pred.psi.col <- data.frame(beta_psi_other = out_df$beta_psi.4., 
                            beta_psi_blue = out_df$beta_psi.5., 
                            beta_psi_white = out_df$beta_psi.6., 
                            mu.psi = out_df$beta_psi.1., # yellow
                            iteration = 1:length(out_df$beta_psi.1.))

 # Calculate the probability of interacting with a bee:
 yellow_prob  <- plogis(pred.psi.col$mu.psi)
 mean(yellow_prob)
 quantile(yellow_prob, c(0.025, 0.975))
 
 white_prob <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_white) 
 mean(white_prob)
 quantile(white_prob, c(0.025, 0.975))
 
 blue_prob <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_blue)
 mean(blue_prob)
 quantile(blue_prob, c(0.025, 0.975))
 
 other_prob <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_other)
 mean(other_prob)
 quantile(other_prob, c(0.025, 0.975))

  
 # Calculate the differences:
 yellow_vs_white <- yellow_prob - white_prob
 mean(yellow_vs_white)
 quantile(yellow_vs_white, c(0.025, 0.975))
 
 yellow_vs_blue  <- yellow_prob - blue_prob
 mean(yellow_vs_blue)
 quantile(yellow_vs_blue, c(0.025, 0.975))
 
 yellow_vs_other  <- yellow_prob - other_prob
 mean(yellow_vs_other)
 quantile(yellow_vs_other, c(0.025, 0.975))
 
 
 
 
 # Probability of one parameter > another
 
 # yellow > other
 mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.4.))
 
 # yellow > blue
 mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.5.))
 
 # yellow > white
 mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.6.))
 
 
 
 
 # color palette
 pal_cols <- c("#E7B800", "pink", "#00AFBB", "grey")
 
 # Bee species model
 flow_col_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                      num_cov = 4,
                                                      beta1 = out_df$beta_psi.1.,
                                                      beta2 = out_df$beta_psi.4.,
                                                      beta3 = out_df$beta_psi.5.,
                                                      beta4 = out_df$beta_psi.6.,
                                                      pal_cols = pal_cols,
                                                      beta1_name = "yellow",
                                                      beta2_name = "other",
                                                      beta3_name = "blue",
                                                      beta4_name = "white",
                                                      x_lab_text = "Probability of interacting \nwith a bee",
                                                      y_lab_text =  "Flower color",
                                                      mod_name = "")
 

 # Create the plot
 flower.color.plot <- flow_col_bee_species_mod$gplot
 
 flower.color.plot
# 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-flower-color.pdf")
#        , height = 10, width = 11)
# 
# 









# 10. Plot the relationship between psi and flower shape ----------------------------------------



 
 # Probability of not bowl >  bowl
 mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.7.))
 
 
 
 # Create a dataframe with the parameter estimates from each MCMC iteraction
 pred.psi.shape <- data.frame(beta_psi_F_shape = out_df$beta_psi.7., 
                               mu.psi = out_df$beta_psi.1.,
                               iteration = 1:length(out_df$beta_psi.1.))
 
 # Calculate the probability of interacting with a bee:
 notBowl_prob <- plogis(pred.psi.shape$mu.psi)
 mean(notBowl_prob)
 quantile(notBowl_prob, c(0.025, 0.975))
 
 bowl_prob <- plogis(pred.psi.shape$mu.psi + pred.psi.shape$beta_psi_F_shape)
 mean(bowl_prob)
 quantile(bowl_prob, c(0.025, 0.975))
 

 
 # Calculate the differences:
 notbowl_vs_bowl <- notBowl_prob  - bowl_prob
 mean(notbowl_vs_bowl)
 quantile(notbowl_vs_bowl, c(0.025, 0.975))
 

 
 # Bee species model
 flow_sha_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                      num_cov = 2,
                                                      beta1 = out_df$beta_psi.1.,
                                                      beta2 = out_df$beta_psi.7.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                      beta1_name = "Not bowl",
                                                      beta2_name = "Bowl",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of interacting \nwith a bee",
                                                      y_lab_text =  "Flower shape",
                                                      mod_name = "")
 
 
 # Create the plot
 bowl.plot <- flow_sha_bee_species_mod$gplot
 
 bowl.plot
# 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-flower-shape.pdf")
#        , height = 10, width = 11)
# 



 # 11. Combined figure ----------------------------------------
 
 
 




(bee.size.plot + solitary.plot +
  flower.color.plot + bowl.plot) + 
   plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Fig-1-Bee-plant-interaction-prob.png")
       , height = 8, width = 13)






# 11. Plot the relationship between p and bee stripes ----------------------------------------



 out_df <- as.data.frame(out)

 
 
## Not striped > striped
 mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.2.))
 
 
 # Create a dataframe with the parameter estimates from each MCMC iteraction
 pred.p <- data.frame(beta_p_B_stripped = out_df$beta_p.2., 
                      mu.p = out_df$beta_p.1.,
                      iteration = 1:length(out_df$beta_p.1.))
 
 
 
 ## Add a column with the covariate
 pred.p$stripped <- plogis(pred.p$mu.p + pred.p$beta_p_B_stripped)
 mean(pred.p$stripped)
 quantile(pred.p$stripped, c(0.025, 0.975))
 
 pred.p$notStripped <- plogis(pred.p$mu.p) 
 mean(pred.p$notStripped)
 quantile(pred.p$notStripped, c(0.025, 0.975))
 
 
 # Bee species model
 bee_str_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                     num_cov = 2,
                                                     beta1 = out_df$beta_p.1.,
                                                     beta2 = out_df$beta_p.2.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                     beta1_name = "Not striped",
                                                     beta2_name = "Striped",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of detecting \na bee on a plant",
                                                     y_lab_text =  "Bee stripes",
                                                     mod_name = "")
 
 # Create the plot
 strip.plot <- bee_str_bee_species_mod$gplot
 
  strip.plot
 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-bee-stripes.pdf"), 
#        height = 3, width = 5)
 
 

  
  
  
  # 15. Plot the relationship between p and bee size ----------------------------------------
  
  
  
  # What proportion of the mass is > 0?
  mean(out_df$beta_p.3. > 0)
  
  
  
  # Create a dataframe with the parameter estimates from each MCMC iteraction
  pred.p.size <- data.frame(beta_p_B_size = out_df$beta_p.3., 
                            mu.p = out_df$beta_p.1.,
                            iteration = 1:length(out_df$beta_p.1.))
  
  # Create a vector of size values
  size_values <- seq(-3, 3, length.out = 50)
  
  ## Add a column with the covariate
  pred.p.size <- pred.p.size %>%
                  crossing(Size.scaled = size_values)
  
  
  # Add predictions
  pred.p.size$pred <- plogis(pred.p.size$mu.p + 
                             pred.p.size$beta_p_B_size * 
                             pred.p.size$Size.scaled)
  
  # Round to ensure grouping works properly
  pred.p.size$Size.scaled <- round(pred.p.size$Size.scaled, 2)
  
  # Group by Size.scaled and calculate quantiles
  size_vals <- unique(pred.p.size$Size.scaled)
  
  
  # Calculate the probability of detecting a bee:
  small_bee_prob <- plogis(pred.p.size$mu.p + 
                             pred.p.size$beta_p_B_size * small_bee)
  mean(small_bee_prob)
  quantile(small_bee_prob, c(0.025, 0.975))
  
  large_bee_prob <- plogis(pred.p.size$mu.p + 
                             pred.p.size$beta_p_B_size * large_bee)
  mean(large_bee_prob)
  quantile(large_bee_prob, c(0.025, 0.975))
  
  small_vs_large <- small_bee_prob - large_bee_prob
  mean(small_vs_large)
  quantile(small_vs_large, c(0.025, 0.975))
  
  
  
  # Bee species model
  bee_size_p_bee_species_mod <- response_continous_cov_plot(out_df = out_df,
                                                            beta = out_df$beta_p.3.,
                                                            intercept = out_df$beta_p.1.,
                                                            x_lab_text = "Bee size standardized",
                                                            y_lab_tex = "Probability of detecting \na bee on a plant",
                                                            mod_name = "")
  
  
  # Create plot
  bee.size.plot.p <-  bee_size_p_bee_species_mod$gplot
  
  
  bee.size.plot.p
  # # Save the plot
  # ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-bee-size.pdf"), 
  #        height = 3, width = 5)
  # 
  






# 12. Plot the relationship between p and source ----------------------------------------



 # observation > literature
 mean(out_df$beta_p.1. > out_df$beta_p.4.)
 
 # observation > collection
 mean(out_df$beta_p.1. > out_df$beta_p.5.)
 
 # observation > aggregated
 mean(out_df$beta_p.1. > out_df$beta_p.6.)
 


# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_lit = out_df$beta_p.4., 
                     beta_p_col = out_df$beta_p.5., 
                     beta_p_aggs = out_df$beta_p.6., 
                     mu.p = out_df$beta_p.1.,
                     iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p$Literature <- plogis(pred.p$mu.p + pred.p$beta_p_lit)  #literature
mean(pred.p$Literature)
quantile(pred.p$Literature, c(0.025, 0.975))

pred.p$Collection <- plogis(pred.p$mu.p + pred.p$beta_p_col)  #collection
mean(pred.p$Collection)
quantile(pred.p$Collection, c(0.025, 0.975))

pred.p$Aggregated <- plogis(pred.p$mu.p + pred.p$beta_p_aggs)  #iNaturalist
mean(pred.p$Aggregated)
quantile(pred.p$Aggregated, c(0.025, 0.975))

pred.p$Observation   <- plogis(pred.p$mu.p)  # Aggregated
mean(pred.p$Observation)
quantile(pred.p$Observation, c(0.025, 0.975))

# Calculate differences
obs_vs_agg <- pred.p$Observation - pred.p$Aggregated
mean(obs_vs_agg)
quantile(obs_vs_agg, c(0.025, 0.975))

obs_vs_col <- pred.p$Observation - pred.p$Collection
mean(obs_vs_col)
quantile(obs_vs_col, c(0.025, 0.975))

obs_vs_lit <- pred.p$Observation - pred.p$Literature
mean(obs_vs_lit)
quantile(obs_vs_lit, c(0.025, 0.975))


# Bee species model
source_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                   num_cov = 4,
                                                   beta1 = out_df$beta_p.1.,
                                                   beta2 = out_df$beta_p.4.,
                                                   beta3 = out_df$beta_p.5.,
                                                   beta4 = out_df$beta_p.6.,
                                                   pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                   beta1_name = "observation",
                                                   beta2_name = "literature",
                                                   beta3_name = "collection",
                                                   beta4_name = "aggregated",
                                                   x_lab_text =  "Probability of detecting \na bee on a plant",
                                                   y_lab_text =  "Source type",
                                                   mod_name = "")

# Create the plot
citation.plot <- source_bee_species_mod$gplot


 citation.plot

 # 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-citation.pdf"), 
#        height = 3, width = 5)
# 









# 13. Plot the relationship between p and flower color ----------------------------------------








# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.col <- data.frame(beta_p_other = out_df$beta_p.7., 
                         beta_p_blue = out_df$beta_p.8., 
                         beta_p_white = out_df$beta_p.9., 
                         mu.p = out_df$beta_p.1.,
                         iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p.col$yellow  <- plogis(pred.p.col$mu.p)  # other
mean(pred.p.col$yellow)
quantile(pred.p.col$yellow, c(0.025, 0.975))

pred.p.col$white <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_white)  # white
mean(pred.p.col$white)
quantile(pred.p.col$white, c(0.025, 0.975))

pred.p.col$blue <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_blue)  # blue
mean(pred.p.col$blue)
quantile(pred.p.col$blue, c(0.025, 0.975))

pred.p.col$other <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_other)  # other
mean(pred.p.col$other)
quantile(pred.p.col$other, c(0.025, 0.975))



# Calculate the differences:
yellow_vs_other  <- pred.p.col$yellow - pred.p.col$other
mean(yellow_vs_other)
quantile(yellow_vs_other, c(0.025, 0.975))


white_vs_yellow <- pred.p.col$white - pred.p.col$yellow
mean(white_vs_yellow)
quantile(white_vs_yellow, c(0.025, 0.975))


blue_vs_yellow  <- pred.p.col$blue - pred.p.col$yellow
mean(blue_vs_yellow)
quantile(blue_vs_yellow, c(0.025, 0.975))

# yellow > other
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.7.))

# yellow > white
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.9.))


# yellow > blue
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.8.))



# Bee species model
flow_col_p_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                       num_cov = 4,
                                                       beta1 = out_df$beta_p.1.,
                                                       beta2 = out_df$beta_p.7.,
                                                       beta3 = out_df$beta_p.8.,
                                                       beta4 = out_df$beta_p.9.,
                                                       pal_cols = pal_cols,
                                                       beta1_name = "yellow",
                                                       beta2_name = "other",
                                                       beta3_name = "blue",
                                                       beta4_name = "white",
                                                       x_lab_text =  "Probability of detecting \na bee on a plant",
                                                       y_lab_text =  "Flower color",
                                                       mod_name = "")


# Create the plot
flow.col.plot.p <- flow_col_p_bee_species_mod$gplot

 flow.col.plot.p
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-color.pdf"), 
#        height = 3, width = 5)
# 
# 








# 14. Plot the relationship between p and flower shape ----------------------------------------



 # Not bowl > bowl
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.10.))



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.shape <- data.frame(beta_p_F_shape = out_df$beta_p.10., 
                           mu.p = out_df$beta_p.1.,
                           iteration = 1:length(out_df$beta_p.1.))

## Add a column with the covariate
pred.p.shape$bowl <- plogis(pred.p.shape$mu.p + pred.p.shape$beta_p_F_shape) 
mean(pred.p.shape$bowl)
quantile(pred.p.shape$bowl, c(0.025, 0.975))

pred.p.shape$notBowl <- plogis(pred.p.shape$mu.p) 
mean(pred.p.shape$notBowl)
quantile(pred.p.shape$notBowl, c(0.025, 0.975))

bowl_diff <- pred.p.shape$bowl - pred.p.shape$notBowl
mean(bowl_diff)
quantile(bowl_diff, c(0.025, 0.975))



# Bee species model
flow_sha_p_bee_species_mod <- response_factor_cov_plot(out_df = out_df,
                                                       num_cov = 2,
                                                       beta1 = out_df$beta_p.1.,
                                                       beta2 = out_df$beta_p.10.,
                                                       beta3 = NULL,
                                                       beta4 = NULL,
                                                       pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                       beta1_name = "Not bowl",
                                                       beta2_name = "Bowl",
                                                       beta3_name = NULL,
                                                       beta4_name = NULL,
                                                       x_lab_text =  "Probability of detecting \na bee on a plant",
                                                       y_lab_text =  "Flower shape",
                                                       mod_name = "")



# Create the plot
flow.shape.plot.p <- flow_sha_p_bee_species_mod$gplot

flow.shape.plot.p
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-shape.pdf"), 
#        height = 3, width = 5)











# 11. Combined detection figure ----------------------------------------





((strip.plot + bee.size.plot.p + 
    flow.col.plot.p + flow.shape.plot.p)/  citation.plot) + 
  plot_layout(ncol = 2, widths = c(1.15, 0.6))+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Fig-2-Bee-plant-detection-prob.png")
       , height = 6, width = 15)



# End script
