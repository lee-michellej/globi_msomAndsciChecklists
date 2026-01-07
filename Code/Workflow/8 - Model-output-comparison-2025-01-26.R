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
## Input Data:
#######################################


# Model output files for all 6 model types (from Script 5):
#   - out-[model_type]-with-priors-1-NIMBLE.rds
#   - result-[model_type]-with-priors-1-NIMBLE.rds
# Where [model_type] = no_bee_plant, bee_species, plant_species, bee_family,
#                      plant_family, bee_plant_family

# Covariate data (from Script 4):
#   - model_covariates - 2025 01 22 - no apis.rds



#######################################
## Output of code:
#######################################


# The statistics and figures that compare the output of 6 different models
# Generates: Appendix S1 Figure S1, and all figures in Appendix S2



#######################################
############ Table of Contents ########
#######################################


# 1. Load libraries & set working directory
# 2. Load data
# 3. Set universal text size for plots
# 4. Write a function to plot the relationship between response and continous covariate
# 5. Plot the relationship between psi and bee size & calculate stats
# 6. Write a function to plot the relationship between response and categorical covariate(s) 
# 7. Plot the relationship between psi and sociality
# 8. Plot the relationship between psi and flower color
# 9. Plot the relationship between psi and flower shape
# 10. Plot the relationship between p and stripe
# 11. Plot the relationship between p and source
# 12. Plot the relationship between p and flower color
# 13. Plot the relationship between p and flower shape
# 14. Plot the relationship between p and bee size


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
library(patchwork)
library(ggdist)





# 2. Load data -------------------------------------------------------



# Set paths
globi_out_folder <- ""
globi_result_folder <- ""
globi_MCMC_folder <- ""
github_folder_path <- ""




# date object for folder
date_folder <- "2025 08 05"
date <- "2025 01 26"



#---- bee_species

# Model name
mod_name <- "bee_species"

# Load the model output - out
load(paste0(globi_out_folder, "/out-bee_species-with-priors-1-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_species <- out

out_bee_species_df <- as.data.frame(out_bee_species)


# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-bee_species-with-priors-1-NIMBLE.rds"))

# Save the out object with a new model specific name
result_bee_species <- subset_result_quarter






#---- bee_family

# Model name
mod_name <- "bee_family"

# Load the model output
load(paste0(globi_out_folder, "/out-bee_family-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
out_bee_family <- out

out_bee_family_df <- as.data.frame(out_bee_family)


# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-bee_family-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
result_bee_family <- subset_result_quarter




#---- plant_species

# Model name
mod_name <- "plant_species"

# Load the model output
load(paste0(globi_out_folder, "/out-plant_species-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
out_plant_species <- out

out_plant_species_df <- as.data.frame(out_plant_species)


# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-plant_species-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
result_plant_species <- subset_result_quarter


#---- plant_family

# Model name
mod_name <- "plant_family"

# Load the model output
load(paste0(globi_out_folder, "/out-plant_family-with-priors-6-NIMBLE.rds"))

# Save the out object with a new model specific name
out_plant_family <- out

out_plant_family_df <- as.data.frame(out_plant_family)


# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-plant_family-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
result_plant_family <- subset_result_quarter




#---- bee_plant_family

# Model name
mod_name <- "bee_plant_family"

# Load the model output
load(paste0(globi_out_folder, "/out-bee_plant_family-with-priors-5-NIMBLE.rds"))


# Save the out object with a new model specific name
out_bee_plant_family <- out

out_bee_plant_family_df <- as.data.frame(out_bee_plant_family)


# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-bee_plant_family-with-priors-5-NIMBLE.rds"))


# Save the out object with a new model specific name
result_bee_plant_family <- subset_result_quarter



#---- no_bee_plant

# Model name
mod_name <- "no_bee_plant"

# Load the model output
load(paste0(globi_out_folder, "/out-no_bee_plant-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
out_no_bee_plant <- out

out_no_bee_plant_df <- as.data.frame(out_no_bee_plant)



# Load the model output - result
load(paste0(globi_result_folder, "/result-quarter-no_bee_plant-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
result_no_bee_plant <- subset_result_quarter






#------------  Upload the data & format
# set working directory

github_folder_path <- ""

setwd(github_folder_path)


# object name = bee.plant.cite
# 3-D array
load(paste0(github_folder_path, "/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds"))



# Read in the dat_info
load(paste0(github_folder_path, "/Data/dat_info_2025_01_22.rds"))
# object name = dat_info



# Load covariates
load(paste0(github_folder_path, "/Data/model_covariates - 2025 01 22 - no apis.rds"))


# Flatten the array
dat_long <- melt(bee.plant.cite) 
colnames(dat_long) <- c("bee_ID", "plant_ID", "cite_ID", "observation")


# Create unique bee-plant pairs for the ecological model
dat_pairs <- unique(subset(dat_long, select = c("bee_ID","plant_ID"))) 
colnames(dat_pairs) <- c("bee_species", "plant_species")

head(dat_long)









# 3. Set universal text size for plots ----------------------------------------




# Specify text size
text.size <- 12
title.size <- 12





# 4. Source the code for the functions ----------------------------------------


source(paste0(github_folder_path, "/Code/Workflow/6 - functions for covariate plots.R"))




# 5. Plot the relationship between psi and bee size & calculate stats ----------------------------------------



# Bee size
  # out_df = out_bee_species_df
  # beta = out_bee_species_df$beta_psi.2.
  # intercept = out_bee_species_df$beta_psi.1.
  # x_lab_text = "Bee size standardized"
  # y_lab_tex = "Probability of interacting \nwith a plant"
  # mod_name = "Bee species model"



# Bee species model
bee_size_bee_species_mod <- response_continous_cov_plot(out_df = out_bee_species_df, 
                                      beta = out_bee_species_df$beta_psi.2.,     
                                      intercept = out_bee_species_df$beta_psi.1.,
                                      mod_name = "Bee species model",
                                      x_lab_text = "Bee size standardized",
                                      y_lab_text = "Probability of interacting \nwith a plant")

# Bee family model
bee_size_bee_family_mod <- response_continous_cov_plot(out_df = out_bee_family_df, 
                                          beta = out_bee_family_df$beta_psi.2.,     
                                          intercept = out_bee_family_df$beta_psi.1.,
                                          mod_name = "Bee family model",
                                         x_lab_text = "Bee size standardized",
                                         y_lab_text = "Probability of interacting \nwith a plant")



# Plant species model
bee_size_plant_species_mod <- response_continous_cov_plot(out_df = out_plant_species_df, 
                                          beta = out_plant_species_df$beta_psi.2.,     
                                          intercept = out_plant_species_df$beta_psi.1.,
                                          mod_name = "Plant species model",
                                          x_lab_text = "Bee size standardized",
                                          y_lab_text = "Probability of interacting \nwith a plant")

# Plant family model
bee_size_plant_family_mod <- response_continous_cov_plot(out_df = out_plant_family_df, 
                                          beta = out_plant_family_df$beta_psi.2.,      
                                          intercept = out_plant_family_df$beta_psi.1., 
                                          mod_name = "Plant family model",
                                          x_lab_text = "Bee size standardized",
                                          y_lab_text = "Probability of interacting \nwith a plant")


# Bee and Plant family model
bee_size_bee_plant_family_mod <- response_continous_cov_plot(out_df = out_bee_plant_family_df, 
                                           beta = out_bee_plant_family_df$beta_psi.2.,     
                                           intercept = out_bee_plant_family_df$beta_psi.1.,
                                           mod_name = "Bee and plant family model",
                                           x_lab_text = "Bee size standardized",
                                           y_lab_text = "Probability of interacting \nwith a plant")



# Bee and Plant family model
bee_size_no_bee_plant_mod <- response_continous_cov_plot(out_df = out_no_bee_plant_df, 
                                               beta = out_no_bee_plant_df$beta_psi.2.,     
                                               intercept = out_no_bee_plant_df$beta_psi.1.,
                                               mod_name = "No bee and plant model",
                                           x_lab_text = "Bee size standardized",
                                           y_lab_text = "Probability of interacting \nwith a plant")



# Stitch the plots together
bee_size_bee_species_mod$gplot + bee_size_bee_family_mod$gplot+
 bee_size_plant_species_mod$gplot + bee_size_plant_family_mod$gplot+
 bee_size_bee_plant_family_mod$gplot + bee_size_no_bee_plant_mod$gplot  + 
  plot_layout(ncol = 2) +  
  plot_annotation(tag_levels = "A")


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S1-psi-bee-size-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
bee_size_stats <- rbind(bee_size_bee_species_mod$stats_df, bee_size_bee_family_mod$stats_df,
                        bee_size_plant_species_mod$stats_df, bee_size_plant_family_mod$stats_df,
                        bee_size_bee_plant_family_mod$stats_df, bee_size_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_size_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-psi-bee-size-mod-comparison.csv"))
















# 7. Plot the relationship between psi and sociality ---------------------------------------



# Sociality:
  # out_df = out_bee_species_df 
  # num_cov = 2
  # beta1 = out_bee_species_df$beta_psi.1.
  # beta2 = out_bee_species_df$beta_psi.3.
  # beta3 = NULL
  # beta4 = NULL
  # pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete"))
  # beta1_name = "social"
  # beta2_name = "solitary"
  # beta3_name = NULL
  # beta4_name = NULL
  # x_lab_text = "Probability of interacting \nwith a flower"
  # y_lab_text =  "Bee sociality"
  # mod_name = "Bee species model"


# Bee species model
bee_soc_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 2,
                                                     beta1 = out_bee_species_df$beta_psi.1.,
                                                     beta2 = out_bee_species_df$beta_psi.3.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                     beta1_name = "social",
                                                     beta2_name = "solitary",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of interacting \nwith a flower",
                                                     y_lab_text =  "Bee sociality",
                                                     mod_name = "Bee species model")

# Bee family model
bee_soc_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 2,
                                                    beta1 = out_bee_family_df$beta_psi.1.,
                                                    beta2 = out_bee_family_df$beta_psi.3.,
                                                    beta3 = NULL,
                                                    beta4 = NULL,
                                                    pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                    beta1_name = "social",
                                                    beta2_name = "solitary",
                                                    beta3_name = NULL,
                                                    beta4_name = NULL,
                                                    x_lab_text = "Probability of interacting \nwith a flower",
                                                    y_lab_text =  "Bee sociality",
                                                    mod_name = "Bee family model")

# Plant species model
bee_soc_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 2,
                                                       beta1 = out_plant_species_df$beta_psi.1.,
                                                       beta2 = out_plant_species_df$beta_psi.3.,
                                                       beta3 = NULL,
                                                       beta4 = NULL,
                                                       pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                       beta1_name = "social",
                                                       beta2_name = "solitary",
                                                       beta3_name = NULL,
                                                       beta4_name = NULL,
                                                       x_lab_text = "Probability of interacting \nwith a flower",
                                                       y_lab_text =  "Bee sociality",
                                                       mod_name = "Plant species model")


# Plant family model
bee_soc_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 2,
                                                      beta1 = out_plant_family_df$beta_psi.1.,
                                                      beta2 = out_plant_family_df$beta_psi.3.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                      beta1_name = "social",
                                                      beta2_name = "solitary",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of interacting \nwith a flower",
                                                      y_lab_text =  "Bee sociality",
                                                      mod_name = "Plant family model")


# Bee and Plant family model
bee_soc_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 2,
                                                          beta1 = out_bee_plant_family_df$beta_psi.1.,
                                                          beta2 = out_bee_plant_family_df$beta_psi.3.,
                                                          beta3 = NULL,
                                                          beta4 = NULL,
                                                          pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                          beta1_name = "social",
                                                          beta2_name = "solitary",
                                                          beta3_name = NULL,
                                                          beta4_name = NULL,
                                                          x_lab_text = "Probability of interacting \nwith a flower",
                                                          y_lab_text =  "Bee sociality",
                                                          mod_name = "Bee and plant family model")


# Bee and Plant family model
bee_soc_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 2,
                                                      beta1 = out_no_bee_plant_df$beta_psi.1.,
                                                      beta2 = out_no_bee_plant_df$beta_psi.3.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete")),
                                                      beta1_name = "social",
                                                      beta2_name = "solitary",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of interacting \nwith a flower",
                                                      y_lab_text =  "Bee sociality",
                                                      mod_name = "No bee and plant family model")

# Stitch the plots together
(bee_soc_bee_species_mod$gplot      + bee_soc_bee_family_mod$gplot+
 bee_soc_plant_species_mod$gplot    + bee_soc_plant_family_mod$gplot+
 bee_soc_bee_plant_family_mod$gplot + bee_soc_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S2-psi-bee-sociality-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
bee_sociality_stats <- rbind(bee_soc_bee_species_mod$stats_df,      bee_soc_bee_family_mod$stats_df,
                             bee_soc_plant_species_mod$stats_df,    bee_soc_plant_family_mod$stats_df,
                             bee_soc_bee_plant_family_mod$stats_df, bee_soc_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_sociality_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-psi-bee-sociality-mod-comparison.csv"))










# 8. Plot the relationship between psi and flower color ----------------------------------------





# Flower color:
  # out_df = out_bee_species_df 
  # num_cov = 4 # either 2 or 4
  # beta1 = out_bee_species_df$beta_psi.1.
  # beta2 = out_bee_species_df$beta_psi.4.
  # beta3 = out_bee_species_df$beta_psi.5.
  # beta4 = out_bee_species_df$beta_psi.6.
  # mod_name = "Bee species model"
  # x_lab_text = "Probability of interacting \nwith a bee"
  # y_lab_text =  "Flower color"
  # pal_cols = c("#E7B800","#00AFBB",  "grey", "black")
  # beta1_name = "yellow"
  # beta2_name = "other"
  # beta3_name = "blue"
  # beta4_name = "white"


pal_cols <- c("#E7B800", "pink", "#00AFBB", "grey")

# Bee species model
flow_col_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 4,
                                                     beta1 = out_bee_species_df$beta_psi.1.,
                                                     beta2 = out_bee_species_df$beta_psi.4.,
                                                     beta3 = out_bee_species_df$beta_psi.5.,
                                                     beta4 = out_bee_species_df$beta_psi.6.,
                                                     pal_cols = pal_cols,
                                                     beta1_name = "yellow",
                                                     beta2_name = "other",
                                                     beta3_name = "blue",
                                                     beta4_name = "white",
                                                     x_lab_text = "Probability of interacting \nwith a bee",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "Bee species model")

# Bee family model
flow_col_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                   num_cov = 4,
                                                   beta1 = out_bee_family_df$beta_psi.1.,
                                                   beta2 = out_bee_family_df$beta_psi.4.,
                                                   beta3 = out_bee_family_df$beta_psi.5.,
                                                   beta4 = out_bee_family_df$beta_psi.6.,
                                                   pal_cols = pal_cols,
                                                   beta1_name = "yellow",
                                                   beta2_name = "other",
                                                   beta3_name = "blue",
                                                   beta4_name = "white",
                                                   x_lab_text = "Probability of interacting \nwith a bee",
                                                   y_lab_text =  "Flower color",
                                                   mod_name = "Bee family model")

# Plant species model
flow_col_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                      num_cov = 4,
                                                      beta1 = out_plant_species_df$beta_psi.1.,
                                                      beta2 = out_plant_species_df$beta_psi.4.,
                                                      beta3 = out_plant_species_df$beta_psi.5.,
                                                      beta4 = out_plant_species_df$beta_psi.6.,
                                                      pal_cols = pal_cols,
                                                      beta1_name = "yellow",
                                                      beta2_name = "other",
                                                      beta3_name = "blue",
                                                      beta4_name = "white",
                                                      x_lab_text = "Probability of interacting \nwith a bee",
                                                      y_lab_text =  "Flower color",
                                                      mod_name = "Plant species model")


# Plant family model
flow_col_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                     num_cov = 4,
                                                     beta1 = out_plant_family_df$beta_psi.1.,
                                                     beta2 = out_plant_family_df$beta_psi.4.,
                                                     beta3 = out_plant_family_df$beta_psi.5.,
                                                     beta4 = out_plant_family_df$beta_psi.6.,
                                                     pal_cols = pal_cols,
                                                     beta1_name = "yellow",
                                                     beta2_name = "other",
                                                     beta3_name = "blue",
                                                     beta4_name = "white",
                                                     x_lab_text = "Probability of interacting \nwith a bee",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "Plant family model")


# Bee and Plant family model
flow_col_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                         num_cov = 4,
                                                         beta1 = out_bee_plant_family_df$beta_psi.1.,
                                                         beta2 = out_bee_plant_family_df$beta_psi.4.,
                                                         beta3 = out_bee_plant_family_df$beta_psi.5.,
                                                         beta4 = out_bee_plant_family_df$beta_psi.6.,
                                                         pal_cols = pal_cols,
                                                         beta1_name = "yellow",
                                                         beta2_name = "other",
                                                         beta3_name = "blue",
                                                         beta4_name = "white",
                                                         x_lab_text = "Probability of interacting \nwith a bee",
                                                         y_lab_text =  "Flower color",
                                                         mod_name = "Bee and plant family model")


# Bee and Plant family model
flow_col_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                     num_cov = 4,
                                                     beta1 = out_no_bee_plant_df$beta_psi.1.,
                                                     beta2 = out_no_bee_plant_df$beta_psi.4.,
                                                     beta3 = out_no_bee_plant_df$beta_psi.5.,
                                                     beta4 = out_no_bee_plant_df$beta_psi.6.,
                                                     pal_cols = pal_cols,
                                                     beta1_name = "yellow",
                                                     beta2_name = "other",
                                                     beta3_name = "blue",
                                                     beta4_name = "white",
                                                     x_lab_text = "Probability of interacting \nwith a bee",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "No bee and plant family model")

# Stitch the plots together
(flow_col_bee_species_mod$gplot      + flow_col_bee_family_mod$gplot+
 flow_col_plant_species_mod$gplot    + flow_col_plant_family_mod$gplot+
 flow_col_bee_plant_family_mod$gplot + flow_col_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S3-psi-flow-col-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
flow_col_stats <- rbind(flow_col_bee_species_mod$stats_df,      flow_col_bee_family_mod$stats_df,
                        flow_col_plant_species_mod$stats_df,    flow_col_plant_family_mod$stats_df,
                        flow_col_bee_plant_family_mod$stats_df, flow_col_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_col_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-psi-flow-col-mod-comparison.csv"))







# 9. Plot the relationship between psi and flower shape ----------------------------------------


# Flower shape:
  # out_df = out_bee_species_df 
  # num_cov = 2
  # beta1 = out_bee_species_df$beta_psi.1.
  # beta2 = out_bee_species_df$beta_psi.7.
  # beta3 = NULL
  # beta4 = NULL
  # pal_cols = wes_palette("GrandBudapest2", 2, type = c("discrete"))
  # beta1_name = "Not bowl"
  # beta2_name = "Bowl"
  # beta3_name = NULL
  # beta4_name = NULL
  # x_lab_text = "Probability of interacting \nwith a bee"
  # y_lab_text =  "Flower shape"

# Bee species model
flow_sha_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 2,
                                                     beta1 = out_bee_species_df$beta_psi.1.,
                                                     beta2 = out_bee_species_df$beta_psi.7.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                     beta1_name = "Not bowl",
                                                     beta2_name = "Bowl",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of interacting \nwith a bee",
                                                     y_lab_text =  "Flower shape",
                                                     mod_name = "Bee species model")

# Bee family model
flow_sha_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 2,
                                                    beta1 = out_bee_family_df$beta_psi.1.,
                                                    beta2 = out_bee_family_df$beta_psi.7.,
                                                    beta3 = NULL,
                                                    beta4 = NULL,
                                                    pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                    beta1_name = "Not bowl",
                                                    beta2_name = "Bowl",
                                                    beta3_name = NULL,
                                                    beta4_name = NULL,
                                                    x_lab_text = "Probability of interacting \nwith a bee",
                                                    y_lab_text =  "Flower shape",
                                                    mod_name = "Bee family model")

# Plant species model
flow_sha_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 2,
                                                       beta1 = out_plant_species_df$beta_psi.1.,
                                                       beta2 = out_plant_species_df$beta_psi.7.,
                                                       beta3 = NULL,
                                                       beta4 = NULL,
                                                       pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                       beta1_name = "Not bowl",
                                                       beta2_name = "Bowl",
                                                       beta3_name = NULL,
                                                       beta4_name = NULL,
                                                       x_lab_text = "Probability of interacting \nwith a bee",
                                                       y_lab_text =  "Flower shape",
                                                       mod_name = "Plant species model")


# Plant family model
flow_sha_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 2,
                                                      beta1 = out_plant_family_df$beta_psi.1.,
                                                      beta2 = out_plant_family_df$beta_psi.7.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                      beta1_name = "Not bowl",
                                                      beta2_name = "Bowl",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of interacting \nwith a bee",
                                                      y_lab_text =  "Flower shape",
                                                      mod_name = "Plant family model")


# Bee and Plant family model
flow_sha_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 2,
                                                          beta1 = out_bee_plant_family_df$beta_psi.1.,
                                                          beta2 = out_bee_plant_family_df$beta_psi.7.,
                                                          beta3 = NULL,
                                                          beta4 = NULL,
                                                          pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                          beta1_name = "Not bowl",
                                                          beta2_name = "Bowl",
                                                          beta3_name = NULL,
                                                          beta4_name = NULL,
                                                          x_lab_text = "Probability of interacting \nwith a bee",
                                                          y_lab_text =  "Flower shape",
                                                          mod_name = "Bee and plant family model")


# Bee and Plant family model
flow_sha_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 2,
                                                      beta1 = out_no_bee_plant_df$beta_psi.1.,
                                                      beta2 = out_no_bee_plant_df$beta_psi.7.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("GrandBudapest1", 2, type = c("discrete")),
                                                      beta1_name = "Not bowl",
                                                      beta2_name = "Bowl",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of interacting \nwith a bee",
                                                      y_lab_text =  "Flower shape",
                                                      mod_name = "No bee and plant family model")

# Stitch the plots together
(flow_sha_bee_species_mod$gplot      + flow_sha_bee_family_mod$gplot+
 flow_sha_plant_species_mod$gplot    + flow_sha_plant_family_mod$gplot+
 flow_sha_bee_plant_family_mod$gplot + flow_sha_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S4-psi-flow-sha-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
flow_sha_stats <- rbind(flow_sha_bee_species_mod$stats_df,      flow_sha_bee_family_mod$stats_df,
                             flow_sha_plant_species_mod$stats_df,    flow_sha_plant_family_mod$stats_df,
                             flow_sha_bee_plant_family_mod$stats_df, flow_sha_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_sha_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-psi-flow-sha-mod-comparison.csv"))








# 10. Plot the relationship between p and stripe ----------------------------------------




# Stripe:
  # out_df = out_bee_species_df 
  # num_cov = 2
  # beta1 = out_bee_species_df$beta_p.1.
  # beta2 = out_bee_species_df$beta_p.2.
  # beta3 = NULL
  # beta4 = NULL
  # pal_cols = wes_palette("Moonrise3", 2, type = c("discrete"))
  # beta1_name = "Not striped"
  # beta2_name = "Striped"
  # beta3_name = NULL
  # beta4_name = NULL
  # x_lab_text = "Probability of detecting \nthe bee on a plant"
  # y_lab_text =  "Bee stripes"



# Bee species model
bee_str_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 2,
                                                     beta1 = out_bee_species_df$beta_p.1.,
                                                     beta2 = out_bee_species_df$beta_p.2.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                     beta1_name = "Not striped",
                                                     beta2_name = "Striped",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of detecting \nthe bee on a plant",
                                                     y_lab_text =  "Bee stripes",
                                                     mod_name = "Bee species model")

# Bee family model
bee_str_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 2,
                                                   beta1 = out_bee_family_df$beta_p.1.,
                                                   beta2 = out_bee_family_df$beta_p.2.,
                                                   beta3 = NULL,
                                                   beta4 = NULL,
                                                   pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                   beta1_name = "Not striped",
                                                   beta2_name = "Striped",
                                                   beta3_name = NULL,
                                                   beta4_name = NULL,
                                                   x_lab_text = "Probability of detecting \nthe bee on a plant",
                                                   y_lab_text =  "Bee stripes",
                                                   mod_name = "Bee family model")

# Plant species model
bee_str_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 2,
                                                      beta1 = out_plant_species_df$beta_p.1.,
                                                      beta2 = out_plant_species_df$beta_p.2.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                      beta1_name = "Not striped",
                                                      beta2_name = "Striped",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of detecting \nthe bee on a plant",
                                                      y_lab_text =  "Bee stripes",
                                                      mod_name = "Plant species model")


# Plant family model
bee_str_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 2,
                                                     beta1 = out_plant_family_df$beta_p.1.,
                                                     beta2 = out_plant_family_df$beta_p.2.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                     beta1_name = "Not striped",
                                                     beta2_name = "Striped",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of detecting \nthe bee on a plant",
                                                     y_lab_text =  "Bee stripes",
                                                     mod_name = "Plant family model")


# Bee and Plant family model
bee_str_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 2,
                                                         beta1 = out_bee_plant_family_df$beta_p.1.,
                                                         beta2 = out_bee_plant_family_df$beta_p.2.,
                                                         beta3 = NULL,
                                                         beta4 = NULL,
                                                         pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                         beta1_name = "Not striped",
                                                         beta2_name = "Striped",
                                                         beta3_name = NULL,
                                                         beta4_name = NULL,
                                                         x_lab_text = "Probability of detecting \nthe bee on a plant",
                                                         y_lab_text =  "Bee stripes",
                                                         mod_name = "Bee and plant family model")


# Bee and Plant family model
bee_str_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 2,
                                                     beta1 = out_no_bee_plant_df$beta_p.1.,
                                                     beta2 = out_no_bee_plant_df$beta_p.2.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("Moonrise3", 2, type = c("discrete")),
                                                     beta1_name = "Not striped",
                                                     beta2_name = "Striped",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of detecting \nthe bee on a plant",
                                                     y_lab_text =  "Bee stripes",
                                                     mod_name = "No bee and plant family model")

# Stitch the plots together
(bee_str_bee_species_mod$gplot      + bee_str_bee_family_mod$gplot+
 bee_str_plant_species_mod$gplot    + bee_str_plant_family_mod$gplot+
 bee_str_bee_plant_family_mod$gplot + bee_str_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S5-p-bee-str-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
bee_str_stats <- rbind(bee_str_bee_species_mod$stats_df,      bee_str_bee_family_mod$stats_df,
                       bee_str_plant_species_mod$stats_df,    bee_str_plant_family_mod$stats_df,
                       bee_str_bee_plant_family_mod$stats_df, bee_str_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_str_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-p-bee-str-mod-comparison.csv"))







# 11. Plot the relationship between p and source ----------------------------------------


# Source type:
  # out_df = out_bee_species_df 
  # num_cov = 4
  # beta1 = out_bee_species_df$beta_p.1.
  # beta2 = out_bee_species_df$beta_p.4.
  # beta3 = out_bee_species_df$beta_p.5.
  # beta4 = out_bee_species_df$beta_p.6.
  # pal_cols = wes_palette("Moonrise2", 4, type = c("discrete"))
  # beta1_name = "observation"
  # beta2_name = "literature"
  # beta3_name = "collection"
  # beta4_name = "aggregated"s
  # x_lab_text = "Probability of detecting a \nbee-plant interaction"
  # y_lab_text =  "Source type"


# Bee species model
source_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 4,
                                                     beta1 = out_bee_species_df$beta_p.1.,
                                                     beta2 = out_bee_species_df$beta_p.4.,
                                                     beta3 = out_bee_species_df$beta_p.5.,
                                                     beta4 = out_bee_species_df$beta_p.6.,
                                                     pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                     beta1_name = "observation",
                                                     beta2_name = "literature",
                                                     beta3_name = "collection",
                                                     beta4_name = "aggregated",
                                                     x_lab_text = "Probability of detecting a \nbee-plant interaction",
                                                     y_lab_text =  "Source type",
                                                     mod_name = "Bee species model")

# Bee family model
source_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 4,
                                                  beta1 = out_bee_family_df$beta_p.1.,
                                                  beta2 = out_bee_family_df$beta_p.4.,
                                                  beta3 = out_bee_family_df$beta_p.5.,
                                                  beta4 = out_bee_family_df$beta_p.6.,
                                                  pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                  beta1_name = "observation",
                                                  beta2_name = "literature",
                                                  beta3_name = "collection",
                                                  beta4_name = "aggregated",
                                                  x_lab_text = "Probability of detecting a \nbee-plant interaction",
                                                  y_lab_text =  "Source type",
                                                    mod_name = "Bee family model")

# Plant species model
source_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 4,
                                                     beta1 = out_plant_species_df$beta_p.1.,
                                                     beta2 = out_plant_species_df$beta_p.4.,
                                                     beta3 = out_plant_species_df$beta_p.5.,
                                                     beta4 = out_plant_species_df$beta_p.6.,
                                                     pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                     beta1_name = "observation",
                                                     beta2_name = "literature",
                                                     beta3_name = "collection",
                                                     beta4_name = "aggregated",
                                                     x_lab_text = "Probability of detecting a \nbee-plant interaction",
                                                     y_lab_text =  "Source type",
                                                       mod_name = "Plant species model")


# Plant family model
source_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 4,
                                                    beta1 = out_plant_family_df$beta_p.1.,
                                                    beta2 = out_plant_family_df$beta_p.4.,
                                                    beta3 = out_plant_family_df$beta_p.5.,
                                                    beta4 = out_plant_family_df$beta_p.6.,
                                                    pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                    beta1_name = "observation",
                                                    beta2_name = "literature",
                                                    beta3_name = "collection",
                                                    beta4_name = "aggregated",
                                                    x_lab_text = "Probability of detecting a \nbee-plant interaction",
                                                    y_lab_text =  "Source type",
                                                      mod_name = "Plant family model")


# Bee and Plant family model
source_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 4,
                                                        beta1 = out_bee_plant_family_df$beta_p.1.,
                                                        beta2 = out_bee_plant_family_df$beta_p.4.,
                                                        beta3 = out_bee_plant_family_df$beta_p.5.,
                                                        beta4 = out_bee_plant_family_df$beta_p.6.,
                                                        pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                        beta1_name = "observation",
                                                        beta2_name = "literature",
                                                        beta3_name = "collection",
                                                        beta4_name = "aggregated",
                                                        x_lab_text = "Probability of detecting a \nbee-plant interaction",
                                                        y_lab_text =  "Source type",
                                                          mod_name = "Bee and plant family model")


# Bee and Plant family model
source_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 4,
                                                    beta1 = out_no_bee_plant_df$beta_p.1.,
                                                    beta2 = out_no_bee_plant_df$beta_p.4.,
                                                    beta3 = out_no_bee_plant_df$beta_p.5.,
                                                    beta4 = out_no_bee_plant_df$beta_p.6.,
                                                    pal_cols = wes_palette("Moonrise2", 4, type = c("discrete")),
                                                    beta1_name = "observation",
                                                    beta2_name = "literature",
                                                    beta3_name = "collection",
                                                    beta4_name = "aggregated",
                                                    x_lab_text = "Probability of detecting a \nbee-plant interaction",
                                                    y_lab_text =  "Source type",
                                                      mod_name = "No bee and plant family model")

# Stitch the plots together
(source_bee_species_mod$gplot      + source_bee_family_mod$gplot+
 source_plant_species_mod$gplot    + source_plant_family_mod$gplot+
 source_bee_plant_family_mod$gplot + source_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S6-p-source-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
source_stats <- rbind(source_bee_species_mod$stats_df,      source_bee_family_mod$stats_df,
                        source_plant_species_mod$stats_df,    source_plant_family_mod$stats_df,
                        source_bee_plant_family_mod$stats_df, source_no_bee_plant_mod$stats_df )



# Save the table
write.csv(source_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-p-source-mod-comparison.csv"))








# 12. Plot the relationship between p and flower color ----------------------------------------



# Flower color:
  # out_df = out_bee_species_df 
  # num_cov = 4
  # beta1 = out_bee_species_df$beta_p.1.
  # beta2 = out_bee_species_df$beta_p.7.
  # beta3 = out_bee_species_df$beta_p.8.
  # beta4 = out_bee_species_df$beta_p.9.
  # pal_cols = c( "#E7B800", "#00AFBB","grey", "black")
  # beta1_name = "yellow"
  # beta2_name = "other"
  # beta3_name = "blue"
  # beta4_name = "white"
  # x_lab_text = "Probability of detecting a \nbee interacting on the plant"
  # y_lab_text =  "Flower color"


# Bee species model
flow_col_p_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 4,
                                                     beta1 = out_bee_species_df$beta_p.1.,
                                                     beta2 = out_bee_species_df$beta_p.7.,
                                                     beta3 = out_bee_species_df$beta_p.8.,
                                                     beta4 = out_bee_species_df$beta_p.9.,
                                                     pal_cols = pal_cols,
                                                     beta1_name = "yellow",
                                                     beta2_name = "other",
                                                     beta3_name = "blue",
                                                     beta4_name = "white",
                                                     x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "Bee species model")

# Bee family model
flow_col_p_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 4,
                                                    beta1 = out_bee_family_df$beta_p.1.,
                                                    beta2 = out_bee_family_df$beta_p.7.,
                                                    beta3 = out_bee_family_df$beta_p.8.,
                                                    beta4 = out_bee_family_df$beta_p.9.,
                                                    pal_cols = pal_cols,
                                                    beta1_name = "yellow",
                                                    beta2_name = "other",
                                                    beta3_name = "blue",
                                                    beta4_name = "white",
                                                    x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                    y_lab_text =  "Flower color",
                                                    mod_name = "Bee family model")

# Plant species model
flow_col_p_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 4,
                                                       beta1 = out_plant_species_df$beta_p.1.,
                                                       beta2 = out_plant_species_df$beta_p.7.,
                                                       beta3 = out_plant_species_df$beta_p.8.,
                                                       beta4 = out_plant_species_df$beta_p.9.,
                                                       pal_cols = pal_cols,
                                                       beta1_name = "yellow",
                                                       beta2_name = "other",
                                                       beta3_name = "blue",
                                                       beta4_name = "white",
                                                       x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                       y_lab_text =  "Flower color",
                                                       mod_name = "Plant species model")


# Plant family model
flow_col_p_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 4,
                                                      beta1 = out_plant_family_df$beta_p.1.,
                                                      beta2 = out_plant_family_df$beta_p.7.,
                                                      beta3 = out_plant_family_df$beta_p.8.,
                                                      beta4 = out_plant_family_df$beta_p.9.,
                                                      pal_cols = pal_cols,
                                                      beta1_name = "yellow",
                                                      beta2_name = "other",
                                                      beta3_name = "blue",
                                                      beta4_name = "white",
                                                      x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                      y_lab_text =  "Flower color",
                                                      mod_name = "Plant family model")


# Bee and Plant family model
flow_col_p_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 4,
                                                          beta1 = out_bee_plant_family_df$beta_p.1.,
                                                          beta2 = out_bee_plant_family_df$beta_p.7.,
                                                          beta3 = out_bee_plant_family_df$beta_p.8.,
                                                          beta4 = out_bee_plant_family_df$beta_p.9.,
                                                          pal_cols = pal_cols,
                                                          beta1_name = "yellow",
                                                          beta2_name = "other",
                                                          beta3_name = "blue",
                                                          beta4_name = "white",
                                                          x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                          y_lab_text =  "Flower color",
                                                          mod_name = "Bee and plant family model")


# Bee and Plant family model
flow_col_p_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 4,
                                                      beta1 = out_no_bee_plant_df$beta_p.1.,
                                                      beta2 = out_no_bee_plant_df$beta_p.7.,
                                                      beta3 = out_no_bee_plant_df$beta_p.8.,
                                                      beta4 = out_no_bee_plant_df$beta_p.9.,
                                                      pal_cols = pal_cols,
                                                      beta1_name = "yellow",
                                                      beta2_name = "other",
                                                      beta3_name = "blue",
                                                      beta4_name = "white",
                                                      x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                      y_lab_text =  "Flower color",
                                                      mod_name = "No bee and plant family model")

# Stitch the plots together
(flow_col_p_bee_species_mod$gplot      + flow_col_p_bee_family_mod$gplot+
 flow_col_p_plant_species_mod$gplot    + flow_col_p_plant_family_mod$gplot+
 flow_col_p_bee_plant_family_mod$gplot + flow_col_p_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S7-p-flow-col-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
flow_col_p_stats <- rbind(flow_col_p_bee_species_mod$stats_df,      flow_col_p_bee_family_mod$stats_df,
                          flow_col_p_plant_species_mod$stats_df,    flow_col_p_plant_family_mod$stats_df,
                          flow_col_p_bee_plant_family_mod$stats_df, flow_col_p_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_col_p_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-p-flow-col-mod-comparison.csv"))








# 13. Plot the relationship between p and flower shape ----------------------------------------



# Flower shape:
  # out_df = out_bee_species_df 
  # num_cov = 2
  # beta1 = out_bee_species_df$beta_p.1.
  # beta2 = out_bee_species_df$beta_p.10.
  # beta3 = NULL
  # beta4 = NULL
  # pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete"))
  # beta1_name = "Not bowl"
  # beta2_name = "Bowl"
  # beta3_name = NULL
  # beta4_name = NULL
  # x_lab_text = "Probability of detecting a \nbee interacting on the plant"
  # y_lab_text =  "Flower shape"



# Bee species model
flow_sha_p_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 2,
                                                     beta1 = out_bee_species_df$beta_p.1.,
                                                     beta2 = out_bee_species_df$beta_p.10.,
                                                     beta3 = NULL,
                                                     beta4 = NULL,
                                                     pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                     beta1_name = "Not bowl",
                                                     beta2_name = "Bowl",
                                                     beta3_name = NULL,
                                                     beta4_name = NULL,
                                                     x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                     y_lab_text =  "Flower shape",
                                                     mod_name = "Bee species model")

# Bee family model
flow_sha_p_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 2,
                                                    beta1 = out_bee_family_df$beta_p.1.,
                                                    beta2 = out_bee_family_df$beta_p.10.,
                                                    beta3 = NULL,
                                                    beta4 = NULL,
                                                    pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                    beta1_name = "Not bowl",
                                                    beta2_name = "Bowl",
                                                    beta3_name = NULL,
                                                    beta4_name = NULL,
                                                    x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                    y_lab_text =  "Flower shape",
                                                    mod_name = "Bee family model")

# Plant species model
flow_sha_p_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 2,
                                                       beta1 = out_plant_species_df$beta_p.1.,
                                                       beta2 = out_plant_species_df$beta_p.10.,
                                                       beta3 = NULL,
                                                       beta4 = NULL,
                                                       pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                       beta1_name = "Not bowl",
                                                       beta2_name = "Bowl",
                                                       beta3_name = NULL,
                                                       beta4_name = NULL,
                                                       x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                       y_lab_text =  "Flower shape",
                                                       mod_name = "Plant species model")


# Plant family model
flow_sha_p_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 2,
                                                      beta1 = out_plant_family_df$beta_p.1.,
                                                      beta2 = out_plant_family_df$beta_p.10.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                      beta1_name = "Not bowl",
                                                      beta2_name = "Bowl",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                      y_lab_text =  "Flower shape",
                                                      mod_name = "Plant family model")


# Bee and Plant family model
flow_sha_p_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 2,
                                                          beta1 = out_bee_plant_family_df$beta_p.1.,
                                                          beta2 = out_bee_plant_family_df$beta_p.10.,
                                                          beta3 = NULL,
                                                          beta4 = NULL,
                                                          pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                          beta1_name = "Not bowl",
                                                          beta2_name = "Bowl",
                                                          beta3_name = NULL,
                                                          beta4_name = NULL,
                                                          x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                          y_lab_text =  "Flower shape",
                                                          mod_name = "Bee and plant family model")


# Bee and Plant family model
flow_sha_p_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 2,
                                                      beta1 = out_no_bee_plant_df$beta_p.1.,
                                                      beta2 = out_no_bee_plant_df$beta_p.10.,
                                                      beta3 = NULL,
                                                      beta4 = NULL,
                                                      pal_cols = wes_palette("Darjeeling1", 2, type = c("discrete")),
                                                      beta1_name = "Not bowl",
                                                      beta2_name = "Bowl",
                                                      beta3_name = NULL,
                                                      beta4_name = NULL,
                                                      x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                      y_lab_text =  "Flower shape",
                                                      mod_name = "No bee and plant family model")

# Stitch the plots together
(flow_sha_p_bee_species_mod$gplot      + flow_sha_p_bee_family_mod$gplot+
 flow_sha_p_plant_species_mod$gplot    + flow_sha_p_plant_family_mod$gplot+
 flow_sha_p_bee_plant_family_mod$gplot + flow_sha_p_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S8-p-flow-sha-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
flow_sha_p_stats <- rbind(flow_sha_p_bee_species_mod$stats_df,      flow_sha_p_bee_family_mod$stats_df,
                             flow_sha_p_plant_species_mod$stats_df,    flow_sha_p_plant_family_mod$stats_df,
                             flow_sha_p_bee_plant_family_mod$stats_df, flow_sha_p_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_sha_p_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-p-flow-sha-mod-comparison.csv"))










# 14. Plot the relationship between p and bee size ----------------------------------------



# Bee size
  # out_df = out_bee_species_df
  # beta = out_bee_species_df$beta_p.3.
  # intercept = out_bee_species_df$beta_p.1.
  # x_lab_text = "Bee size standardized"
  # y_lab_tex = "Probability of detecting \nthe bee on a plant"
  # mod_name = "Bee species model"



# Bee species model
bee_size_p_bee_species_mod <- response_continous_cov_plot(out_df = out_bee_species_df,
                                                          beta = out_bee_species_df$beta_p.3.,
                                                          intercept = out_bee_species_df$beta_p.1.,
                                                          x_lab_text = "Bee size standardized",
                                                          y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                                          mod_name = "Bee species model")

# Bee family model
bee_size_p_bee_family_mod <- response_continous_cov_plot(out_df = out_bee_family_df, 
                                           beta = out_bee_family_df$beta_p.3.,
                                           intercept = out_bee_family_df$beta_p.1.,
                                           x_lab_text = "Bee size standardized",
                                           y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                           mod_name = "Bee family model")



# Plant species model
bee_size_p_plant_species_mod <- response_continous_cov_plot(out_df = out_plant_species_df, 
                                              beta = out_plant_species_df$beta_p.3.,
                                              intercept = out_plant_species_df$beta_p.1.,
                                              x_lab_text = "Bee size standardized",
                                              y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                            mod_name = "Plant species model")

# Plant family model
bee_size_p_plant_family_mod <- response_continous_cov_plot(out_df = out_plant_family_df, 
                                             beta = out_plant_family_df$beta_p.3.,
                                             intercept = out_plant_family_df$beta_p.1.,
                                             x_lab_text = "Bee size standardized",
                                             y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                             mod_name = "Plant family model")


# Bee and Plant family model
bee_size_p_bee_plant_family_mod <- response_continous_cov_plot(out_df = out_bee_plant_family_df, 
                                                 beta = out_bee_plant_family_df$beta_p.3.,
                                                 intercept = out_bee_plant_family_df$beta_p.1.,
                                                 x_lab_text = "Bee size standardized",
                                                 y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                                 mod_name = "Bee and plant family model")



# Bee and Plant family model
bee_size_p_no_bee_plant_mod <- response_continous_cov_plot(out_df = out_no_bee_plant_df, 
                                             beta = out_no_bee_plant_df$beta_p.3.,
                                             intercept = out_no_bee_plant_df$beta_p.1.,
                                             x_lab_text = "Bee size standardized",
                                             y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                             mod_name = "No bee and plant model")



# Stitch the plots together
(bee_size_p_bee_species_mod$gplot      + bee_size_p_bee_family_mod$gplot+
 bee_size_p_plant_species_mod$gplot    + bee_size_p_plant_family_mod$gplot+
 bee_size_p_bee_plant_family_mod$gplot + bee_size_p_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S9-p-bee-size-mod-comparison.png")
       , height = 10, width = 12)




# Stitch together the stats
bee_size_p_stats <- rbind(bee_size_p_bee_species_mod$stats_df,      bee_size_p_bee_family_mod$stats_df,
                        bee_size_p_plant_species_mod$stats_df,    bee_size_p_plant_family_mod$stats_df,
                        bee_size_p_bee_plant_family_mod$stats_df, bee_size_p_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_size_p_stats, paste0(github_folder_path, "/Tables/", date_folder, "/Supp-table-p-bee-size-mod-comparison.csv"))







# 15. Calculate the number of plant species interactions per bee species  ----------------------------------------





mod_name <- "bee_species"


# n_plants_per_bee

  
  # Summarize the observed data
  
  # We will take the max (0 or 1) value across the 3rd dimension
  y.bee.plant <- apply(bee.plant.cite, c(1, 2), max, na.rm = TRUE)
  y.bee.plant[y.bee.plant == "-Inf"] <- 0
  
  
  # Then, we will sum the number of plant species that each bee species interacts with
  y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)
  
  
  # Observed number of plant species that each bee interacts with
  obs.dat <- data.frame(obs = y.bee.plant)
  
  
  # Calcilate the total number of POSSIBLE bee-plant interactions (regardless of month & citation)
  bee.plant.pos <- apply(bee.plant.cite, c(1, 2), max, na.rm = TRUE)
  bee.plant.pos[bee.plant.pos == "-Inf"] <- NA
  bee.plant.pos[bee.plant.pos == 0] <- 1
  bee.plant.pos <- apply(bee.plant.pos, 1, sum, na.rm = TRUE)
  
  
  
  # Take a random subset of MCMC iterations to keep
  row.subset <- sample(1:dim(result_bee_species[[1]]$samples2)[1], size = n_samp, replace = F)

  
  # Extract the number of plant interactions per bee species (n_plants_per_bee)
  out_n <- as.mcmc(data.frame(rbind(result_bee_species[[1]]$samples2[row.subset, grep("n_", colnames(result_bee_species[[1]]$samples2))],
                                    result_bee_species[[2]]$samples2[row.subset, grep("n_", colnames(result_bee_species[[1]]$samples2))],
                                    result_bee_species[[3]]$samples2[row.subset, grep("n_", colnames(result_bee_species[[1]]$samples2))])))
  
  
  # Calculate mean and 95% CI for the number of plant interactions per bee species
  bee.interactions.mean <- apply(out_n, c(2), mean, na.rm = TRUE)
  bee.interactions.lower <- apply(out_n, 2, function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[1])
  bee.interactions.upper <- apply(out_n, 2, function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[2])
  

  # Combine the names, and model output
  dat <- data.frame(names = c(rep("observed", times = length(dat_info$bee.species)),
                              rep("Num-interact", times = length(dat_info$bee.species))), 
                    species = rep(dat_info$bee.species, times = 2),
                    bee_num = rep(1:length(dat_info$bee.species), times = 2),
                    mod.mean = c( y.bee.plant,
                                  bee.interactions.mean), 
                    mod.q2.5 = c(rep(NA, times = length(y.bee.plant)),
                                 bee.interactions.lower), 
                    mod.q97.5 = c(rep(NA, times = length(y.bee.plant)),
                                  bee.interactions.upper))
  
  
  # Aggregate data - these are already in the correct species order per file "4 - Format_covariates_2025_01_07.R"
  covariates$bee.covariates$bee_num <- 1:nrow(covariates$bee.covariates)
  
  # Subset bee family and species ID
  bee_family_species <- covariates$bee.covariates %>% 
    select(family_num, bee_num, family)
  
  dat <- dat %>%
    left_join(bee_family_species, by = "bee_num") 
  
  
  
  # Plot with numer of plant interactions per bee species
  ggplot(dat[grep("Num-interact", dat$names),], 
         aes(x= species, y=mod.mean, 
             ymin= mod.q2.5, 
             ymax= mod.q97.5))+ 
    geom_linerange(linewidth = 1) +
    geom_point(size = 1) +
    geom_point(data = dat[grep("observed", dat$names),], 
               aes(x= species, y = mod.mean), size = 1, col = "red") +
    facet_wrap(~family, scale = "free")+
    geom_hline(yintercept = 0, lty=2) +
    coord_flip() + 
    ylab('Number of plant species') +
    xlab("Species names") +
    theme_bw()+ 
    theme(axis.text.x = element_text(size = 10, color = "black"), 
          axis.text.y = element_text(size = 10, color = "black", face = "italic"), 
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.title.x =element_text(size = 10, color = "black")
    ) 
  
  
  # Save the plot
  ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S1-Fig-S1-Number-of-bee-plant-interactions.png"), 
         height = 12, width = 14)
  

  
  
  
  
# 16. Create function to extract "probability of interaction" and plot by species or family ----------------------------------------
  
  
  
  # This function will be used for:
    # bee_family
    # plant_family
  
  
  # To calculate the interaction probability per species, you need to calculate psi
  # logit(psi[bee_species[i], plant_species[i]]) <- 
  #   
  #   # Plant family-specific random effect
  #   u[plant_family[plant_species[i]]] +
  # 
  #   # Intercept
  #   # Average size bee
  #   # Not solitary bee
  #   # Yellow flower color
  #   # Not bowl
  #   beta_psi[1] +
  #   
  #   # Bee size
  #   beta_psi[2] * size[bee_species[i]]+ 
  #   
  #   # Bee solitary (1 = yes; 0 = no)
  #   beta_psi[3] * solitary[bee_species[i]]+ 
  #   
  #   # Flower color
  #   # Different bee genus have different probabilities of interacting with different plant colors
  #   beta_psi[4] * other_flower_color[plant_species[i]]+
  #   beta_psi[5] * blue_flower_color[plant_species[i]]+
  #   beta_psi[6] * white_flower_color[plant_species[i]]+
  #   
  #   # Flower shape (== bowl)
  #   beta_psi[7] * flower_shape[plant_species[i]]
  
  
  # Write a function to calculate the average interaction probability across species per family
  
fam_level_interaction_plot <- function(mod_name,
                             n_samp = 3,
                             # latent variable from the model output
                             result,
                             
                             # bee_species and plant_species vectors
                             bee_species = dat_pairs$bee_species,
                             plant_species = dat_pairs$plant_species,
                             
                             # bee and plant family IDs (as numeric)
                             bee_family = covariates$bee.covariates$family_num,
                             plant_family = covariates$plant.covariates$family_num,
                             
                             # Bee Covariates
                             size          = covariates$bee.covariates$size_std,
                             solitary      = covariates$bee.covariates$solitary,
                             
                             # Flower covariates - color
                             blue_flower_color  = covariates$plant.covariates$blue.new,
                             white_flower_color  = covariates$plant.covariates$white,
                             other_flower_color  = covariates$plant.covariates$other,
                             
                             # Flower covariates - shape
                             flower_shape  = covariates$plant.covariates$bowl
){

  
  
  # Create empty array to hold values
  psi <- array(NA, dim = c(max(dat_long$bee_ID),
                           max(dat_long$plant_ID),
                           n_samp))
  
  # Name the parameters
  beta_psi <- out[,grep("psi", colnames(out))]
  

  # Latent variable from model output
    # Extract u = random effect
  out_u <- as.mcmc(data.frame(rbind(result[[1]]$samples2[, grep("u", colnames(result[[1]]$samples2))],
                                    result[[2]]$samples2[, grep("u", colnames(result[[1]]$samples2))],
                                    result[[3]]$samples2[, grep("u", colnames(result[[1]]$samples2))])))
  
  # random sample of MCMC iterations:
  MCMC.subset <- sample(x = 1:nrow(out_u), n_samp, replace = F)
  
  
  # Loop through each bee and plant family and MCMC iteration to calculate the interaction probability
  
  for(i in 1:length(dat_pairs$bee_species)){ # for each bee and plant pair
      
    if(mod_name == "bee_family"){
      
      # To calculate the interaction probability per family, you need to calculate psi
      psi[bee_species[i], plant_species[i], ] <- 
        
          # Bee family-specific random effect
          out_u[MCMC.subset, bee_family[bee_species[i]]] +
            
            # Intercept
            # Average size bee
            # Not solitary bee
            # Yellow flower color
            # Not bowl
            beta_psi[MCMC.subset, 1] +
            
            # Bee size
            beta_psi[MCMC.subset, 2] * size[bee_species[i]]+ 
            
            # Bee solitary (1 = yes; 0 = no)
            beta_psi[MCMC.subset, 3] * solitary[bee_species[i]]+ 
            
            # Flower color
            # Different bee genus have different probabilities of interacting with different plant colors
            beta_psi[MCMC.subset, 4] * other_flower_color[plant_species[i]]+
            beta_psi[MCMC.subset, 5] * blue_flower_color[plant_species[i]]+
            beta_psi[MCMC.subset, 6] * white_flower_color[plant_species[i]]+
            
            # Flower shape (== bowl)
            beta_psi[MCMC.subset, 7] * flower_shape[plant_species[i]]
      
      
    }
      
      if(mod_name == "plant_family"){
        
      
  # To calculate the interaction probability per family, you need to calculate psi
   psi[bee_species[i], plant_species[i], ] <- 
     
     # Plant family-specific random effect
     out_u[MCMC.subset, plant_family[plant_species[i]]] +
   
     # Intercept
     # Average size bee
     # Not solitary bee
     # Yellow flower color
     # Not bowl
     beta_psi[MCMC.subset, 1] +
     
     # Bee size
     beta_psi[MCMC.subset, 2] * size[bee_species[i]]+ 
     
     # Bee solitary (1 = yes; 0 = no)
     beta_psi[MCMC.subset, 3] * solitary[bee_species[i]]+ 
     
     # Flower color
     # Different bee genus have different probabilities of interacting with different plant colors
     beta_psi[MCMC.subset, 4] * other_flower_color[plant_species[i]]+
     beta_psi[MCMC.subset, 5] * blue_flower_color[plant_species[i]]+
     beta_psi[MCMC.subset, 6] * white_flower_color[plant_species[i]]+
     
     # Flower shape (== bowl)
     beta_psi[MCMC.subset, 7] * flower_shape[plant_species[i]]
      
      
    }
  
  }
  
  
  # Aggregate psi values per family
  
  # First - transform the data from 3D to 2D
  psi_long <- melt(psi)
  colnames(psi_long) <- c("bee_num", "plant_num", "MCMC", "psi")
  
  
  #--- need if else statement here for plant vs bee family model/data formatting
  
  if(mod_name == "plant_family"){
    
    # Aggregate data - these are already in the correct species order per file "4 - Format_covariates_2025_01_07.R"
    covariates$plant.covariates$plant_num <- 1:nrow(covariates$plant.covariates)
    
    # Subset plant family and species ID
    plant_family_species <- covariates$plant.covariates %>% 
                              select(family_num, plant_num, Family)
    
    psi_with_family <- psi_long %>%
      left_join(plant_family_species, by = "plant_num") %>%
      dplyr::group_by(family_num, Family) %>%
      dplyr::summarize(mean_psi = mean(psi), 
                       lower_95 = quantile(psi, probs = c(0.025, 0.975), na.rm = TRUE)[1],
                       upper_95 = quantile(psi, probs = c(0.025, 0.975), na.rm = TRUE)[2],
                       
                       mean_psi_p = mean(plogis(psi)), 
                       lower_95_p = quantile(plogis(psi), probs = c(0.025, 0.975), na.rm = TRUE)[1],
                       upper_95_p = quantile(plogis(psi), probs = c(0.025, 0.975), na.rm = TRUE)[2],
                       
                       .groups = "drop")
    
    ggplot_title <- "Plant family model"
    
  }
  
  if(mod_name == "bee_family"){
    
    # Aggregate data - these are already in the correct species order per file "4 - Format_covariates_2025_01_07.R"
    covariates$bee.covariates$bee_num <- 1:nrow(covariates$bee.covariates)
    
    # Subset bee family and species ID
    bee_family_species <- covariates$bee.covariates %>% 
      select(family_num, bee_num, family)
    
    psi_with_family <- psi_long %>%
      left_join(bee_family_species, by = "bee_num") %>%
      dplyr::group_by(family_num, family) %>%
      dplyr::summarize(mean_psi = mean(psi), 
                       lower_95 = quantile(psi, probs = c(0.025, 0.975), na.rm = TRUE)[1],
                       upper_95 = quantile(psi, probs = c(0.025, 0.975), na.rm = TRUE)[2],
                       
                       mean_psi_p = mean(plogis(psi)), 
                       lower_95_p = quantile(plogis(psi), probs = c(0.025, 0.975), na.rm = TRUE)[1],
                       upper_95_p = quantile(plogis(psi), probs = c(0.025, 0.975), na.rm = TRUE)[2],
                       .groups = "drop")
    
    # Rename bee family column name with capital (so the ggplot function works next)
    colnames(psi_with_family)[which(colnames(psi_with_family) == "family")] <- "Family"
    
    ggplot_title <- "Bee family model"
    
  }
  
  
  # Plot
  gplot <- ggplot(psi_with_family, 
         aes(x= Family, y=mean_psi, 
             ymin= lower_95, 
             ymax= upper_95))+ 
    geom_linerange(linewidth = 1) +
    geom_point(size = 1) +
   geom_hline(yintercept = 0, lty=2) +
    coord_flip() + 
    ylab('Interaction probability on the logit scale') +
    xlab("Family name") +
    theme_bw()+ 
    ggtitle(ggplot_title)+
    theme(axis.text.x = element_text(size = 10, color = "black"), 
          axis.text.y = element_text(size = 10, color = "black", face = "italic"), 
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.title.x =element_text(size = 10, color = "black")
    ) 
  
  # Plot
  gplot_p <- ggplot(psi_with_family, 
                  aes(x= Family, y=mean_psi_p, 
                      ymin= lower_95_p, 
                      ymax= upper_95_p))+ 
    geom_linerange(linewidth = 1) +
    geom_point(size = 1) +
    geom_hline(yintercept = 0, lty=2) +
    coord_flip() + 
    ylab('Interaction probability') +
    xlab("Family name") +
    theme_bw()+ 
    ggtitle(ggplot_title)+
    theme(axis.text.x = element_text(size = 10, color = "black"), 
          axis.text.y = element_text(size = 10, color = "black", face = "italic"), 
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.title.x =element_text(size = 10, color = "black")
    ) 
  
  return(list(gplot = gplot,
              gplot_p = gplot_p,
              psi_with_family = psi_with_family))
  
}


#---- bee_family
bee_fam_inter_plot <- fam_level_interaction_plot(mod_name = "bee_family",
                                                  n_samp = 500,
                                                  result = result_bee_family)


#---- plant_family
plant_fam_inter_plot <- fam_level_interaction_plot(mod_name = "plant_family",
                                                 n_samp = 500,
                                                 result = result_plant_family)



# Create 1 plot
# Logit plot
bee_fam_inter_plot$gplot + plant_fam_inter_plot$gplot +
  plot_layout(ncol = 2) +  
  plot_annotation(tag_levels = "A")


# Probabilty plot
bee_fam_inter_plot$gplot_p + plant_fam_inter_plot$gplot_p +
  plot_layout(ncol = 2) +  
  plot_annotation(tag_levels = "A")


# Calculate the number of families with < 0.20 interaction probability
# Bee family
length(which(bee_fam_inter_plot$psi_with_family$mean_psi_p < 0.2))
# Plant family
length(which(plant_fam_inter_plot$psi_with_family$mean_psi_p < 0.2))
length(plant_fam_inter_plot$psi_with_family$mean_psi_p)


  
# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Appendix-S2-Fig-S10-Number-of-bee-plant-interactions.png"), 
       height = 10, width = 8)


# End script