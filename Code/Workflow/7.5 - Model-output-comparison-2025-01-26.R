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



# Things to change:
  # Change the spacing in some of the graphs
  # Create the "Probability of interaction plots" for each of the models:
    # species
    # family levels








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
globi_folder <- "globi-20250210"

# Set working directory
setwd(paste0("/Volumes/DIRENZO/", globi_folder, "/gdirenzo/globi/"))
#setwd("/Users/gdirenzo/OneDrive - University of Massachusetts/Dropbox_transfer/Globi/")


# Add github folder path
github_folder_path <- "/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/"




# 2. Load data -------------------------------------------------------






# date object for folder
date_folder <- "2025 01 26"
date <- "2025 01 26"



#---- bee_species

# Model name
mod_name <- "bee_species"

# Load the model output - out
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_species <- out

out_bee_species_df <- as.data.frame(out_bee_species)


# Load the model output - result
load(file = paste0("./ModelOutput/", date, "/result-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
result_bee_species <- result






#---- bee_family

# Model name
mod_name <- "bee_family"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-", 
                   mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_family <- out

out_bee_family_df <- as.data.frame(out_bee_family)


# Load the model output - result
load(file = paste0("./ModelOutput/", date, "/result-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
result_bee_family <- result




#---- plant_species

# Model name
mod_name <- "plant_species"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_plant_species <- out

out_plant_species_df <- as.data.frame(out_plant_species)



#---- plant_family

# Model name
mod_name <- "plant_family"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_plant_family <- out

out_plant_family_df <- as.data.frame(out_plant_family)


# Load the model output - result
load(file = paste0("./ModelOutput/", date, "/result-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
result_plant_family <- result




#---- bee_plant_family

# Model name
mod_name <- "bee_plant_family"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_plant_family <- out

out_bee_plant_family_df <- as.data.frame(out_bee_plant_family)



#---- no_bee_plant

# Model name
mod_name <- "no_bee_plant"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_no_bee_plant <- out

out_no_bee_plant_df <- as.data.frame(out_no_bee_plant)




#------------  Upload the data & format
# object name = bee.plant.cite
# 3-D array
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")



# Read in the dat_info
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/dat_info_2025_01_22.rds")
# object name = dat_info



# Load covariates
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/model_covariates - 2025 01 22 - no apis.rds")


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


# Number of samples for MCMC lines on continuous variable plots
n_samp <- 5000





# 4. Write a function to plot the relationship between response and continous covariate ------------------------------------



# Write function for response vs continous covariate plot
response_continous_cov_plot <- function(out_df, 
                                        beta,      # For all it will be = out_df$beta_psi.2.
                                        intercept, # For most it will be: out_df$beta_psi.1.
                                        mod_name,
                                        x_lab_text,
                                        y_lab_text,
                                        n_sub){

# Create empty data frame
  stats_df <- data.frame(mod_name = rep(mod_name, times = 2),
                         beta_direction = c("greater than 0", "less than 0"),
                         proportion = c(NA, NA))
  

# Calculate the statistics:
  # What proportion of the mass is > 0?
stats_df[1, 3] <- mean(beta > 0)

  # What proportion of the mass is < 0?
stats_df[2, 3] <- mean(beta < 0)

  
# Create a dataframe with the parameter estimates from each MCMC iteraction
param_df <- data.frame(beta_psi = beta, 
                       intercept = intercept,
                       iteration = 1:length(intercept))

# Add a column with the covariate
pred_df <- expand_grid(param_df, 
                        tibble(Cov.scaled = seq(-3, 3, length.out = 50)))


# Add predictions
pred_df$pred <- plogis(pred_df$intercept + 
                         pred_df$beta_psi * pred_df$Cov.scaled)

# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(pred_df), n_sub, replace = FALSE)

# Subset the data
pred_df_sub <- pred_df[pred_df$iteration %in% sub.samp,]

# Calculate the mean value for the relationship
mean_response <- data.frame(Cov.scaled = seq(-3, 3, length.out = 50))

mean_response$pred <- plogis(mean(param_df$intercept) +
                             mean(param_df$beta_psi) * mean_response$Cov.scaled)

# Create data frame to label each scenario if it is significant or not
annotation_df <- data.frame(
                          # x-axis value
                            bee_size_value = mean(pred_df_sub$Cov.scaled),
                          # y-axis value
                            prob_value = max(pred_df_sub$pred),
                          # labelz
                            label = paste0("Pr(Slope is greater than 0) = ", round(stats_df[1, 3], dig = 2)))


# Create plot
gplot <- ggplot() +
  geom_line(data = pred_df_sub, aes(x = as.numeric(Cov.scaled), 
                                     y = as.numeric(pred), 
                                     col = as.factor(iteration)), 
            alpha = .4) +
  geom_line(data = mean_response, aes(x = Cov.scaled,
                                      y = pred), col = "black")+
  ylab(y_lab_text)+
  xlab(x_lab_text)+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))+
  ggtitle(mod_name)+
  
  # Add annotation if the relationship is significant or not
  geom_text(data = annotation_df, aes(x = bee_size_value, 
                                      y = prob_value,
                                      label = label), 
            size = 3,
            hjust = 0.5, 
            vjust = 0)

return(list(gplot = gplot,
            stats_df = stats_df))

}





# 5. Plot the relationship between psi and bee size & calculate stats ----------------------------------------



# Bee size
  # out_df = out_bee_species_df
  # beta = out_bee_species_df$beta_psi.2.
  # intercept = out_bee_species_df$beta_psi.1.
  # x_lab_text = "Bee size standardized"
  # y_lab_tex = "Probability of interacting \nwith a plant"
  # mod_name = "Bee species model"
  # n_sub = n_samp
  


# Bee species model
bee_size_bee_species_mod <- response_continous_cov_plot(out_df = out_bee_species_df, 
                                      beta = out_bee_species_df$beta_psi.2.,     
                                      intercept = out_bee_species_df$beta_psi.1.,
                                      mod_name = "Bee species model",
                                      x_lab_text = "Bee size standardized",
                                      y_lab_text = "Probability of interacting \nwith a plant",
                                      n_sub = n_samp)

# Bee family model
bee_size_bee_family_mod <- response_continous_cov_plot(out_df = out_bee_family_df, 
                                          beta = out_bee_family_df$beta_psi.2.,     
                                          intercept = out_bee_family_df$beta_psi.1.,
                                          mod_name = "Bee family model",
                                         x_lab_text = "Bee size standardized",
                                         y_lab_text = "Probability of interacting \nwith a plant",
                                         n_sub = n_samp)



# Plant species model
bee_size_plant_species_mod <- response_continous_cov_plot(out_df = out_plant_species_df, 
                                          beta = out_plant_species_df$beta_psi.2.,     
                                          intercept = out_plant_species_df$beta_psi.1.,
                                          mod_name = "Plant species model",
                                          x_lab_text = "Bee size standardized",
                                          y_lab_text = "Probability of interacting \nwith a plant",
                                          n_sub = n_samp)

# Plant family model
bee_size_plant_family_mod <- response_continous_cov_plot(out_df = out_plant_family_df, 
                                          beta = out_plant_family_df$beta_psi.2.,      
                                          intercept = out_plant_family_df$beta_psi.1., 
                                          mod_name = "Plant family model",
                                          x_lab_text = "Bee size standardized",
                                          y_lab_text = "Probability of interacting \nwith a plant",
                                          n_sub = n_samp)


# Bee and Plant family model
bee_size_bee_plant_family_mod <- response_continous_cov_plot(out_df = out_bee_plant_family_df, 
                                           beta = out_bee_plant_family_df$beta_psi.2.,     
                                           intercept = out_bee_plant_family_df$beta_psi.1.,
                                           mod_name = "Bee and plant family model",
                                           x_lab_text = "Bee size standardized",
                                           y_lab_text = "Probability of interacting \nwith a plant",
                                           n_sub = n_samp)



# Bee and Plant family model
bee_size_no_bee_plant_mod <- response_continous_cov_plot(out_df = out_no_bee_plant_df, 
                                               beta = out_no_bee_plant_df$beta_psi.2.,     
                                               intercept = out_no_bee_plant_df$beta_psi.1.,
                                               mod_name = "No bee and plant model",
                                           x_lab_text = "Bee size standardized",
                                           y_lab_text = "Probability of interacting \nwith a plant",
                                           n_sub = n_samp)



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









# 6. Write a function to plot the relationship between response and categorical covariate(s) ----------------------------------------



# Write function for response vs continuous covariate plot
response_factor_cov_plot <- function(out_df, 
                                     num_cov, # either 2 or 4
                                     beta1,  # This is the intercept - always
                                     beta2,
                                     beta3,
                                     beta4,
                                     beta1_name,
                                     beta2_name,
                                     beta3_name,
                                     beta4_name,
                                     mod_name,
                                     x_lab_text,
                                     y_lab_text,
                                     pal_cols){
  
  # Calculating the stats:
  # Setting up 2 conditions:
  # Number of covariates == 1 or 
  # Number of covariates == 4
  if(num_cov == 2){
    
    # Create empty data frame
    stats_df <- data.frame(mod_name = rep(mod_name, times = 1),
                           beta_direction = c(paste0(beta1_name, " greater than ", beta2_name)),
                           proportion = c(NA))
    
    
    # What proportion of the mass is beta1 < (beta1 + beta2)?
    stats_df[1, 3] <- mean(beta1 < (beta1 + beta2))
    
  }
  
  if(num_cov == 4){
    
    # Create empty data frame
    stats_df <- data.frame(mod_name = rep(mod_name, times = 3),
                           beta_direction = c(paste0(beta1_name, " greater than ", beta2_name),
                                              paste0(beta1_name, " greater than ", beta3_name),
                                              paste0(beta1_name, " greater than ", beta4_name)),
                           proportion = c(NA, NA, NA))
    
    
    # What proportion of the mass is beta1 < (beta1 + beta2)?
    stats_df[1, 3] <- mean(beta1 < (beta1 + beta2))
    
    # What proportion of the mass is beta1 < (beta1 + beta3)?
    stats_df[2, 3] <- mean(beta1 < (beta1 + beta3))
    
    # What proportion of the mass is beta1 < (beta1 + beta4)?
    stats_df[3, 3] <- mean(beta1 < (beta1 + beta4))
    
  }
  
  
  # Create dataframes for plotting
  if(num_cov == 2){
    
    # Create a dataframe with the parameter estimates from each MCMC iteraction
    # These are on the logit scale
    pred_df <- data.frame(beta1 = beta1, 
                          beta2 = beta2,
                          iteration = 1:length(beta1))
    
    # Add a column with the covariate
    # These are on the probability scale
    pred_df$beta1_prob <- plogis(pred_df$beta1) 
    pred_df$beta2_prob <- plogis(pred_df$beta1 + pred_df$beta2) 
    
    # Convert to long format
    pred_df_long <- melt(pred_df[,4:5])
    
    # Change column names
    colnames(pred_df_long) <- c("covariate", "probability")
    
    # Change labels
    pred_df_long$covariate <- ifelse(pred_df_long$covariate == "beta1_prob",
                                     beta1_name,
                                     beta2_name)
    
    # Calculate group means
    mu <- ddply(pred_df_long, 
                "covariate",
                summarise, 
                grp.mean = mean(probability))
    
    
    # Compute x-axis range dynamically
    x_min <- min(pred_df_long$probability, na.rm = TRUE)
    x_max <- max(pred_df_long$probability, na.rm = TRUE)
    x_range <- x_max - x_min
    
    # Dynamic offset values (scaling relative to the x-range)
    offset_major <- x_range * 0.03  # Used for text placement
    offset_major1 <- x_range * 0.09  # Used for text placement
    offset_major2 <- x_range * 0.15  # Used for text placement
    offset_minor <- x_range * 0.02  # Used for segment adjustments
    
    # Annotation data frame - this adds the Bayesian p-values to the plot
    annotation_df <- data.frame(
      start =  c(beta1_name),
      end = c(beta2_name),
      probability = c(max(pred_df$beta2_prob) + offset_major1),
      label = c(mean( pred_df$beta1_prob > pred_df$beta2_prob)),
      y_start = 1,
      y_end = 2
    )
    
    # Create the plot
    gplot <- ggplot(pred_df_long, aes(x = probability, y = covariate)) +
      geom_density_ridges(aes(fill = covariate)) +
      scale_fill_manual(values = pal_cols)+
      geom_vline(data=mu, aes(xintercept = grp.mean, color = covariate),
                 linetype="dashed")+
      scale_color_manual(values = pal_cols)+
      xlab(x_lab_text)+
      ylab(y_lab_text)+
      theme_bw()+ 
      theme(legend.position = "none",
            strip.background = element_rect(colour = "black", fill = "white"),
            strip.text = element_text(size = title.size, color = "black"), 
            panel.background = element_rect(colour = "black", fill = NA),
            axis.text.x = element_text(size = text.size, color = "black"),
            axis.text.y = element_text(size = text.size, color = "black"),
            axis.title.x = element_text(size = title.size, color = "black"),
            axis.title.y = element_text(size = title.size, color = "black"))+
      ggtitle(mod_name)+
      geom_segment(data = annotation_df, aes(x = probability, 
                                             xend = probability,
                                             y = y_start, 
                                             yend = y_end),
                   color = "black", size = 0.5)+
      geom_segment(data = annotation_df, aes(x = probability, 
                                             xend = probability - offset_minor,
                                             y = y_start, 
                                             yend = y_start),
                   color = "black", size = 0.5) +
      geom_segment(data = annotation_df, aes(x = probability, 
                                             xend = probability - offset_minor,
                                             y = y_end, 
                                             yend = y_end),
                   color = "black", size = 0.5)+
      geom_text(data = annotation_df, aes(x = probability + offset_major, 
                                          y = (y_start + y_end) / 2,
                                          label = round(label, 2)), size = 3,
                hjust = 0.5, vjust = 0)
    
  }
  
  if(num_cov == 4){
    
    # Create a dataframe with the parameter estimates from each MCMC iteraction
    # These are on the logit scale
    pred_df <- data.frame(beta1 = beta1, 
                          beta2 = beta2,
                          beta3 = beta3,
                          beta4 = beta4,
                          iteration = 1:length(beta1))
    
    # Add a column with the covariate
    # These are on the probability scale
    pred_df$beta1_prob <- plogis(pred_df$beta1) 
    pred_df$beta2_prob <- plogis(pred_df$beta1 + pred_df$beta2) 
    pred_df$beta3_prob <- plogis(pred_df$beta1 + pred_df$beta3) 
    pred_df$beta4_prob <- plogis(pred_df$beta1 + pred_df$beta4) 
    
    # Convert to long format
    pred_df_long <- melt(pred_df[,6:9])
    
    # Change column names
    colnames(pred_df_long) <- c("covariate", "probability")
    
    # Change labels
    pred_df_long$covariate <- ifelse(pred_df_long$covariate == "beta1_prob",
                                     beta1_name,
                                     ifelse(pred_df_long$covariate == "beta2_prob",
                                            beta2_name,
                                            ifelse(pred_df_long$covariate == "beta3_prob",
                                                   beta3_name,
                                                   ifelse(pred_df_long$covariate == "beta4_prob",
                                                          beta4_name, NA))))
    
    # Make pred_df_long$covariate into a factor
    pred_df_long$covariate <- factor(pred_df_long$covariate, levels = c(beta1_name,
                                                                        beta2_name,
                                                                        beta3_name,
                                                                        beta4_name))
    
    # Calculate group means
    mu <- ddply(pred_df_long, 
                "covariate",
                summarise, 
                grp.mean = mean(probability))
    
    
    # Compute x-axis range dynamically
    x_min <- min(pred_df_long$probability, na.rm = TRUE)
    x_max <- max(pred_df_long$probability, na.rm = TRUE)
    x_range <- x_max - x_min
    
    # Dynamic offset values (scaling relative to the x-range)
    offset_major <- x_range * 0.03  # Used for text placement
    offset_major1 <- x_range * 0.09  # Used for text placement
    offset_major2 <- x_range * 0.15  # Used for text placement
    offset_major3 <- x_range * 0.21  # Used for text placement
    offset_minor <- x_range * 0.02  # Used for segment adjustments
    
    # Create annotation data frame with scaled offsets
    annotation_df <- data.frame(
      start =  c(beta1_name, beta1_name, beta1_name),
      end = c(beta2_name, beta3_name, beta4_name),
      probability = c(max(c(pred_df$beta1_prob, pred_df$beta2_prob, 
                            pred_df$beta3_prob, pred_df$beta4_prob))+offset_major1,
                      max(c(pred_df$beta1_prob, pred_df$beta2_prob, 
                            pred_df$beta3_prob, pred_df$beta4_prob))+offset_major2,
                      max(c(pred_df$beta1_prob, pred_df$beta2_prob, 
                            pred_df$beta3_prob, pred_df$beta4_prob))+offset_major3),
      label = c(mean( pred_df$beta1_prob > pred_df$beta2_prob),
                mean( pred_df$beta1_prob > pred_df$beta3_prob),
                mean( pred_df$beta1_prob > pred_df$beta4_prob))
    )
    
    # Create a mapping of color to numeric positions
    color_levels <- c(beta1_name, beta2_name, beta3_name, beta4_name)
    color_map <- setNames(seq_along(color_levels), color_levels)
    
    # Adjust annotation_df to use numeric positions
    annotation_df$y_start <- color_map[annotation_df$start]
    annotation_df$y_end   <- color_map[annotation_df$end]
    
    # Create the plot with scaled offsets
    gplot <- ggplot(pred_df_long, aes(x = probability, y = covariate)) +
      geom_density_ridges(aes(fill = covariate)) +
      scale_fill_manual(values = pal_cols) +
      geom_vline(data = mu, aes(xintercept = grp.mean, color = covariate),
                 linetype = "dashed") +
      scale_color_manual(values = pal_cols) +
      xlab(x_lab_text) +
      ylab(y_lab_text) +
      theme(legend.position = "none",
            strip.background = element_rect(colour = "black", fill = "white"),
            strip.text = element_text(size = title.size), 
            panel.background = element_rect(colour = "black", fill = NA),
            axis.text.x = element_text(size = text.size),
            axis.text.y = element_text(size = text.size),
            axis.title.x = element_text(size = title.size),
            axis.title.y = element_text(size = title.size)) +
      geom_segment(data = annotation_df, aes(x = probability, 
                                             xend = probability,
                                             y = y_start, yend = y_end),
                   color = "black", size = 0.5) +
      geom_segment(data = annotation_df, aes(x = probability, 
                                             xend = probability - offset_minor,
                                             y = y_start, 
                                             yend = y_start),
                   color = "black", size = 0.5) +
      geom_segment(data = annotation_df, aes(x = probability, 
                                             xend = probability - offset_minor,
                                             y = y_end, 
                                             yend = y_end),
                   color = "black", size = 0.5) +
      geom_text(data = annotation_df, aes(x = probability + offset_major, 
                                          y = (y_start + y_end) / 2,
                                          label = round(label, 2)), size = 3,
                hjust = 0.5, vjust = 0) +
      ggtitle(mod_name)
    
  }
  
  
  return(list(stats_df = stats_df,
              gplot = gplot))
}








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


pal_cols <- c("#E7B800", "grey", "#00AFBB", "white")

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
                                                          mod_name = "Bee species model", 
                                                          n_sub = n_samp)

# Bee family model
bee_size_p_bee_family_mod <- response_continous_cov_plot(out_df = out_bee_family_df, 
                                           beta = out_bee_family_df$beta_p.3.,
                                           intercept = out_bee_family_df$beta_p.1.,
                                           x_lab_text = "Bee size standardized",
                                           y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                           mod_name = "Bee family model", 
                                           n_sub = n_samp)



# Plant species model
bee_size_p_plant_species_mod <- response_continous_cov_plot(out_df = out_plant_species_df, 
                                              beta = out_plant_species_df$beta_p.3.,
                                              intercept = out_plant_species_df$beta_p.1.,
                                              x_lab_text = "Bee size standardized",
                                              y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                            mod_name = "Plant species model", 
                                            n_sub = n_samp)

# Plant family model
bee_size_p_plant_family_mod <- response_continous_cov_plot(out_df = out_plant_family_df, 
                                             beta = out_plant_family_df$beta_p.3.,
                                             intercept = out_plant_family_df$beta_p.1.,
                                             x_lab_text = "Bee size standardized",
                                             y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                             mod_name = "Plant family model", 
                                             n_sub = n_samp)


# Bee and Plant family model
bee_size_p_bee_plant_family_mod <- response_continous_cov_plot(out_df = out_bee_plant_family_df, 
                                                 beta = out_bee_plant_family_df$beta_p.3.,
                                                 intercept = out_bee_plant_family_df$beta_p.1.,
                                                 x_lab_text = "Bee size standardized",
                                                 y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                                 mod_name = "Bee and plant family model", 
                                                 n_sub = n_samp)



# Bee and Plant family model
bee_size_p_no_bee_plant_mod <- response_continous_cov_plot(out_df = out_no_bee_plant_df, 
                                             beta = out_no_bee_plant_df$beta_p.3.,
                                             intercept = out_no_bee_plant_df$beta_p.1.,
                                             x_lab_text = "Bee size standardized",
                                             y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                             mod_name = "No bee and plant model", 
                                             n_sub = n_samp)



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
  row.subset <- sample(1:dim(result[[1]]$samples2)[1], size = n_samp, replace = F)

  
  # Extract the number of plant interactions per bee species (n_plants_per_bee)
  out_n <- as.mcmc(data.frame(rbind(result[[1]]$samples2[row.subset, grep("n_", colnames(result[[1]]$samples2))],
                                    result[[2]]$samples2[row.subset, grep("n_", colnames(result[[1]]$samples2))],
                                    result[[3]]$samples2[row.subset, grep("n_", colnames(result[[1]]$samples2))])))
  
  
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
                                                  n_samp = 5000,
                                                  result = result_bee_family)


#---- plant_family
plant_fam_inter_plot <- fam_level_interaction_plot(mod_name = "plant_family",
                                                 n_samp = 5000,
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