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








# Before running this code:
  # Check to see if you used means vs effects parameterization (you are calculating the p-value differently than what you are plotting)










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



# Set working directory
setwd("/Volumes/DIRENZO/globi/home/gdirenzo/globi/")
#setwd("/Users/gdirenzo/OneDrive - University of Massachusetts/Dropbox_transfer/Globi/")





# 2. Load data -------------------------------------------------------






# date object for folder
date <- "2025 01 26"



#---- bee_species

# Model name
mod_name <- "bee_species"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_species <- out

out_bee_species_df <- as.data.frame(out_bee_species)


#---- bee_family

# Model name
mod_name <- "bee_family"

# Load the model output
load(file = paste0("./ModelOutput/", date, "/out-", 
                   mod_name, "-NIMBLE.rds"))

# Save the out object with a new model specific name
out_bee_family <- out

out_bee_family_df <- as.data.frame(out_bee_family)



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



# 3. Check model convergence and traceplots






# Traceplots
ggs_BYMeco <- ggs(MCMClist) 

# Beta_psi
ggs_BYMeco %>% 
  filter(Parameter %in% c( paste("beta_psi[", 1:4, "]", sep = ""))) %>% 
  ggs_traceplot() + 
  theme_bw()

ggsave(paste0("./Figures/", date, "traceplots-psi-", model_name, ".png"))


# Beta_p
ggs_BYMeco %>% 
  filter(Parameter %in% c( paste("beta_p[", 1:8, "]", sep = ""))) %>% 
  ggs_traceplot() + 
  theme_bw()

ggsave(paste0("./Figures/", date, "traceplots-p-", model_name, ".png"))






# 3. Set universal text size for plots ----------------------------------------




# Specify text size
text.size <- 12
title.size <- 12







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
  ggtitle(mod_name)

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
  


# Bee species model
bee_size_bee_species_mod <- response_continous_cov_plot(out_df = out_bee_species_df, 
                                      beta = out_bee_species_df$beta_psi.2.,     
                                      intercept = out_bee_species_df$beta_psi.1.,
                                      mod_name = "Bee species model",
                                      x_lab_text = "Bee size standardized",
                                      y_lab_text = "Probability of interacting \nwith a plant",
                                      n_sub = 3)

# Bee family model
bee_size_bee_family_mod <- response_continous_cov_plot(out_df = out_bee_family_df, 
                                          beta = out_bee_family_df$beta_psi.2.,     
                                          intercept = out_bee_family_df$beta_psi.1.,
                                          mod_name = "Bee family model",
                                         x_lab_text = "Bee size standardized",
                                         y_lab_text = "Probability of interacting \nwith a plant",
                                         n_sub = 3)



# Plant species model
bee_size_plant_species_mod <- response_continous_cov_plot(out_df = out_plant_species_df, 
                                          beta = out_plant_species_df$beta_psi.2.,     
                                          intercept = out_plant_species_df$beta_psi.1.,
                                          mod_name = "Plant species model",
                                          x_lab_text = "Bee size standardized",
                                          y_lab_text = "Probability of interacting \nwith a plant",
                                          n_sub = 3)

# Plant family model
bee_size_plant_family_mod <- response_continous_cov_plot(out_df = out_plant_family_df, 
                                          beta = out_plant_family_df$beta_psi.2.,      
                                          intercept = out_plant_family_df$beta_psi.1., 
                                          mod_name = "Plant family model",
                                          x_lab_text = "Bee size standardized",
                                          y_lab_text = "Probability of interacting \nwith a plant",
                                          n_sub = 3)


# Bee and Plant family model
bee_size_bee_plant_family_mod <- response_continous_cov_plot(out_df = out_bee_plant_family_df, 
                                           beta = out_bee_plant_family_df$beta_psi.2.,     
                                           intercept = out_bee_plant_family_df$beta_psi.1.,
                                           mod_name = "Bee and plant family model",
                                           x_lab_text = "Bee size standardized",
                                           y_lab_text = "Probability of interacting \nwith a plant",
                                           n_sub = 3)



# Bee and Plant family model
bee_size_no_bee_plant_mod <- response_continous_cov_plot(out_df = out_no_bee_plant_df, 
                                               beta = out_no_bee_plant_df$beta_psi.2.,     
                                               intercept = out_no_bee_plant_df$beta_psi.1.,
                                               mod_name = "No bee and plant model",
                                           x_lab_text = "Bee size standardized",
                                           y_lab_text = "Probability of interacting \nwith a plant",
                                           n_sub = 3)



# Stitch the plots together
bee_size_bee_species_mod$gplot + bee_size_bee_family_mod$gplot+
 bee_size_plant_species_mod$gplot + bee_size_plant_family_mod$gplot+
 bee_size_bee_plant_family_mod$gplot + bee_size_no_bee_plant_mod$gplot  + 
  plot_layout(ncol = 2)


# Save the plot
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-psi-bee-size-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
bee_size_stats <- rbind(bee_size_bee_species_mod$stats_df, bee_size_bee_family_mod$stats_df,
                        bee_size_plant_species_mod$stats_df, bee_size_plant_family_mod$stats_df,
                        bee_size_bee_plant_family_mod$stats_df, bee_size_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_size_stats, paste0("./Tables/", date_folder, "/Supp-table-psi-bee-size-mod-comparison.csv"))









# 6. Write a function to plot the relationship between response and categorical covariate(s) ----------------------------------------



# Write function for response vs continuous covariate plot
response_factor_cov_plot <- function(out_df, 
                                     num_cov, # either 2 or 4
                                     beta1,
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
  
  
  # What proportion of the mass is beta1 < beta2?
  stats_df[1, 3] <- mean(beta1 < beta2)
  
}
  
if(num_cov == 4){
  
  # Create empty data frame
  stats_df <- data.frame(mod_name = rep(mod_name, times = 3),
                         beta_direction = c(paste0(beta1_name, " greater than ", beta2_name),
                                            paste0(beta1_name, " greater than ", beta3_name),
                                            paste0(beta1_name, " greater than ", beta4_name)),
                         proportion = c(NA, NA, NA))
  
  
  # What proportion of the mass is beta1 < beta2?
  stats_df[1, 3] <- mean(beta1 < beta2)
  
  # What proportion of the mass is beta1 < beta3?
  stats_df[2, 3] <- mean(beta1 < beta3)
  
  # What proportion of the mass is beta1 < beta4?
  stats_df[3, 3] <- mean(beta1 < beta4)
  
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


# Annotation data frame - this adds the Bayesian p-values to the plot
annotation_df <- data.frame(
  start =  c(beta1_name),
  end = c(beta2_name),
  probability = c(1.1),
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
                                         y = y_start, yend = y_end),
               color = "black", size = 0.5)+
  geom_segment(data = annotation_df, aes(x = probability, 
                                         xend = probability - 0.03,
                                         y = y_start, yend = y_start),
               color = "black", size = 0.5) +
  geom_segment(data = annotation_df, aes(x = probability, 
                                         xend = probability - 0.03,
                                         y = y_end, yend = y_end),
               color = "black", size = 0.5)+
  geom_text(data = annotation_df, aes(x = probability + 0.05, 
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
    
    
    # Annotation data frame - this adds the Bayesian p-values to the plot
    annotation_df <- data.frame(
      start =  c(beta1_name, beta1_name, beta1_name),
      end = c(beta2_name, beta3_name, beta4_name),
      probability = c(1.1, 1.2, 1.3),
      label = c(mean( pred_df$beta1_prob > pred_df$beta2_prob),
                mean( pred_df$beta1_prob > pred_df$beta3_prob),
                mean( pred_df$beta1_prob > pred_df$beta4_prob))
    )
    
    # Create a mapping of color to numeric positions
    color_levels <- c(beta1_name, beta2_name, beta3_name, beta4_name)
    color_map    <- setNames(seq_along(color_levels), color_levels)
    
    # Adjust annotation_df to use numeric positions
    annotation_df$y_start <- color_map[annotation_df$start]
    annotation_df$y_end   <- color_map[annotation_df$end]
    
    # Create the plot
    gplot <- ggplot(pred_df_long, aes(x = probability, y = covariate)) +
      geom_density_ridges(aes(fill = covariate)) +
      scale_fill_manual(values = pal_cols)+
      geom_vline(data=mu, aes(xintercept = grp.mean, 
                              color = covariate),
                 linetype="dashed")+
      scale_color_manual(values = pal_cols)+
      xlab(x_lab_text)+
      ylab(y_lab_text)+
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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-psi-bee-sociality-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
bee_sociality_stats <- rbind(bee_soc_bee_species_mod$stats_df,      bee_soc_bee_family_mod$stats_df,
                             bee_soc_plant_species_mod$stats_df,    bee_soc_plant_family_mod$stats_df,
                             bee_soc_bee_plant_family_mod$stats_df, bee_soc_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_sociality_stats, paste0("./Tables/", date_folder, "/Supp-table-psi-bee-sociality-mod-comparison.csv"))










# 8. Plot the relationship between psi and flower color ----------------------------------------





# Flower color:
  # out_df = out_bee_species_df 
  # num_cov = 4 # either 2 or 4
  # beta1 = out_bee_species_df$beta_psi.4.
  # beta2 = out_bee_species_df$beta_psi.5.
  # beta3 = out_bee_species_df$beta_psi.6.
  # beta4 = out_bee_species_df$beta_psi.1.
  # mod_name = "Bee species model"
  # x_lab_text = "Probability of interacting \nwith a bee"
  # y_lab_text =  "Flower color"
  # pal_cols = c("#E7B800","#00AFBB",  "grey", "black")
  # beta1_name = "yellow"
  # beta2_name = "blue"
  # beta3_name = "white"
  # beta4_name = "other"


# Bee species model
flow_col_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 4,
                                                     beta1 = out_bee_species_df$beta_psi.4.,
                                                     beta2 = out_bee_species_df$beta_psi.5.,
                                                     beta3 = out_bee_species_df$beta_psi.6.,
                                                     beta4 = out_bee_species_df$beta_psi.1.,
                                                     pal_cols = c("#E7B800","#00AFBB",  "grey", "black"),
                                                     beta1_name = "yellow",
                                                     beta2_name = "blue",
                                                     beta3_name = "white",
                                                     beta4_name = "other",
                                                     x_lab_text = "Probability of interacting \nwith a bee",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "Bee species model")

# Bee family model
flow_col_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                   num_cov = 4,
                                                   beta1 = out_bee_family_df$beta_psi.4.,
                                                   beta2 = out_bee_family_df$beta_psi.5.,
                                                   beta3 = out_bee_family_df$beta_psi.6.,
                                                   beta4 = out_bee_family_df$beta_psi.1.,
                                                   pal_cols = c("#E7B800","#00AFBB",  "grey", "black"),
                                                   beta1_name = "yellow",
                                                   beta2_name = "blue",
                                                   beta3_name = "white",
                                                   beta4_name = "other",
                                                   x_lab_text = "Probability of interacting \nwith a bee",
                                                   y_lab_text =  "Flower color",
                                                   mod_name = "Bee family model")

# Plant species model
flow_col_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                      num_cov = 4,
                                                      beta1 = out_plant_species_df$beta_psi.4.,
                                                      beta2 = out_plant_species_df$beta_psi.5.,
                                                      beta3 = out_plant_species_df$beta_psi.6.,
                                                      beta4 = out_plant_species_df$beta_psi.1.,
                                                      pal_cols = c("#E7B800","#00AFBB",  "grey", "black"),
                                                      beta1_name = "yellow",
                                                      beta2_name = "blue",
                                                      beta3_name = "white",
                                                      beta4_name = "other",
                                                      x_lab_text = "Probability of interacting \nwith a bee",
                                                      y_lab_text =  "Flower color",
                                                      mod_name = "Plant species model")


# Plant family model
flow_col_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                     num_cov = 4,
                                                     beta1 = out_plant_family_df$beta_psi.4.,
                                                     beta2 = out_plant_family_df$beta_psi.5.,
                                                     beta3 = out_plant_family_df$beta_psi.6.,
                                                     beta4 = out_plant_family_df$beta_psi.1.,
                                                     pal_cols = c("#E7B800","#00AFBB",  "grey", "black"),
                                                     beta1_name = "yellow",
                                                     beta2_name = "blue",
                                                     beta3_name = "white",
                                                     beta4_name = "other",
                                                     x_lab_text = "Probability of interacting \nwith a bee",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "Plant family model")


# Bee and Plant family model
flow_col_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                         num_cov = 4,
                                                         beta1 = out_bee_plant_family_df$beta_psi.4.,
                                                         beta2 = out_bee_plant_family_df$beta_psi.5.,
                                                         beta3 = out_bee_plant_family_df$beta_psi.6.,
                                                         beta4 = out_bee_plant_family_df$beta_psi.1.,
                                                         pal_cols = c("#E7B800","#00AFBB",  "grey", "black"),
                                                         beta1_name = "yellow",
                                                         beta2_name = "blue",
                                                         beta3_name = "white",
                                                         beta4_name = "other",
                                                         x_lab_text = "Probability of interacting \nwith a bee",
                                                         y_lab_text =  "Flower color",
                                                         mod_name = "Bee and plant family model")


# Bee and Plant family model
flow_col_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                     num_cov = 4,
                                                     beta1 = out_no_bee_plant_df$beta_psi.4.,
                                                     beta2 = out_no_bee_plant_df$beta_psi.5.,
                                                     beta3 = out_no_bee_plant_df$beta_psi.6.,
                                                     beta4 = out_no_bee_plant_df$beta_psi.1.,
                                                     pal_cols = c("#E7B800","#00AFBB",  "grey", "black"),
                                                     beta1_name = "yellow",
                                                     beta2_name = "blue",
                                                     beta3_name = "white",
                                                     beta4_name = "other",
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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-psi-flow-col-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
flow_col_stats <- rbind(flow_col_bee_species_mod$stats_df,      flow_col_bee_family_mod$stats_df,
                        flow_col_plant_species_mod$stats_df,    flow_col_plant_family_mod$stats_df,
                        flow_col_bee_plant_family_mod$stats_df, flow_col_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_col_stats, paste0("./Tables/", date_folder, "/Supp-table-psi-flow-col-mod-comparison.csv"))







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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-psi-flow-sha-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
flow_sha_stats <- rbind(flow_sha_bee_species_mod$stats_df,      flow_sha_bee_family_mod$stats_df,
                             flow_sha_plant_species_mod$stats_df,    flow_sha_plant_family_mod$stats_df,
                             flow_sha_bee_plant_family_mod$stats_df, flow_sha_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_sha_stats, paste0("./Tables/", date_folder, "/Supp-table-psi-flow-sha-mod-comparison.csv"))








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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-p-bee-str-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
bee_str_stats <- rbind(bee_str_bee_species_mod$stats_df,      bee_str_bee_family_mod$stats_df,
                       bee_str_plant_species_mod$stats_df,    bee_str_plant_family_mod$stats_df,
                       bee_str_bee_plant_family_mod$stats_df, bee_str_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_str_stats, paste0("./Tables/", date_folder, "/Supp-table-p-bee-str-mod-comparison.csv"))







# 11. Plot the relationship between p and source ----------------------------------------


# Source type:
  # out_df = out_bee_species_df 
  # num_cov = 4
  # beta1 = out_bee_species_df$beta_p.6.
  # beta2 = out_bee_species_df$beta_p.4.
  # beta3 = out_bee_species_df$beta_p.5.
  # beta4 = out_bee_species_df$beta_p.1.
  # pal_cols = wes_palette("Moonrise2", 4, type = c("discrete"))
  # beta1_name = "observation"
  # beta2_name = "literature"
  # beta3_name = "collection"
  # beta4_name = "aggregated"
  # x_lab_text = "Probability of detecting a \nbee-plant interaction"
  # y_lab_text =  "Source type"


# Bee species model
source_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 4,
                                                     beta1 = out_bee_species_df$beta_p.6.,
                                                     beta2 = out_bee_species_df$beta_p.4.,
                                                     beta3 = out_bee_species_df$beta_p.5.,
                                                     beta4 = out_bee_species_df$beta_p.1.,
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
                                                  beta1 = out_bee_family_df$beta_p.6.,
                                                  beta2 = out_bee_family_df$beta_p.4.,
                                                  beta3 = out_bee_family_df$beta_p.5.,
                                                  beta4 = out_bee_family_df$beta_p.1.,
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
                                                     beta1 = out_plant_species_df$beta_p.6.,
                                                     beta2 = out_plant_species_df$beta_p.4.,
                                                     beta3 = out_plant_species_df$beta_p.5.,
                                                     beta4 = out_plant_species_df$beta_p.1.,
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
                                                    beta1 = out_plant_family_df$beta_p.6.,
                                                    beta2 = out_plant_family_df$beta_p.4.,
                                                    beta3 = out_plant_family_df$beta_p.5.,
                                                    beta4 = out_plant_family_df$beta_p.1.,
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
                                                        beta1 = out_bee_plant_family_df$beta_p.6.,
                                                        beta2 = out_bee_plant_family_df$beta_p.4.,
                                                        beta3 = out_bee_plant_family_df$beta_p.5.,
                                                        beta4 = out_bee_plant_family_df$beta_p.1.,
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
                                                    beta1 = out_no_bee_plant_df$beta_p.6.,
                                                    beta2 = out_no_bee_plant_df$beta_p.4.,
                                                    beta3 = out_no_bee_plant_df$beta_p.5.,
                                                    beta4 = out_no_bee_plant_df$beta_p.1.,
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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-p-source-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
source_stats <- rbind(source_bee_species_mod$stats_df,      source_bee_family_mod$stats_df,
                        source_plant_species_mod$stats_df,    source_plant_family_mod$stats_df,
                        source_bee_plant_family_mod$stats_df, source_no_bee_plant_mod$stats_df )



# Save the table
write.csv(source_stats, paste0("./Tables/", date_folder, "/Supp-table-p-source-mod-comparison.csv"))








# 12. Plot the relationship between p and flower color ----------------------------------------



# Flower color:
  # out_df = out_bee_species_df 
  # num_cov = 4
  # beta1 = out_bee_species_df$beta_p.7.
  # beta2 = out_bee_species_df$beta_p.8.
  # beta3 = out_bee_species_df$beta_p.9.
  # beta4 = out_bee_species_df$beta_p.1.
  # pal_cols = c( "#E7B800", "#00AFBB","grey", "black")
  # beta1_name = "yellow"
  # beta2_name = "blue"
  # beta3_name = "white"
  # beta4_name = "other"
  # x_lab_text = "Probability of detecting a \nbee interacting on the plant"
  # y_lab_text =  "Flower color"


# Bee species model
flow_col_p_bee_species_mod <- response_factor_cov_plot(out_df = out_bee_species_df,
                                                     num_cov = 4,
                                                     beta1 = out_bee_species_df$beta_p.7.,
                                                     beta2 = out_bee_species_df$beta_p.8.,
                                                     beta3 = out_bee_species_df$beta_p.9.,
                                                     beta4 = out_bee_species_df$beta_p.1.,
                                                     pal_cols = c( "#E7B800", "#00AFBB","grey", "black"),
                                                     beta1_name = "yellow",
                                                     beta2_name = "blue",
                                                     beta3_name = "white",
                                                     beta4_name = "other",
                                                     x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                     y_lab_text =  "Flower color",
                                                     mod_name = "Bee species model")

# Bee family model
flow_col_p_bee_family_mod <- response_factor_cov_plot(out_df = out_bee_family_df,
                                                    num_cov = 4,
                                                    beta1 = out_bee_family_df$beta_p.7.,
                                                    beta2 = out_bee_family_df$beta_p.8.,
                                                    beta3 = out_bee_family_df$beta_p.9.,
                                                    beta4 = out_bee_family_df$beta_p.1.,
                                                    pal_cols = c( "#E7B800", "#00AFBB","grey", "black"),
                                                    beta1_name = "yellow",
                                                    beta2_name = "blue",
                                                    beta3_name = "white",
                                                    beta4_name = "other",
                                                    x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                    y_lab_text =  "Flower color",
                                                    mod_name = "Bee family model")

# Plant species model
flow_col_p_plant_species_mod <- response_factor_cov_plot(out_df = out_plant_species_df,
                                                       num_cov = 4,
                                                       beta1 = out_plant_species_df$beta_p.7.,
                                                       beta2 = out_plant_species_df$beta_p.8.,
                                                       beta3 = out_plant_species_df$beta_p.9.,
                                                       beta4 = out_plant_species_df$beta_p.1.,
                                                       pal_cols = c( "#E7B800", "#00AFBB","grey", "black"),
                                                       beta1_name = "yellow",
                                                       beta2_name = "blue",
                                                       beta3_name = "white",
                                                       beta4_name = "other",
                                                       x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                       y_lab_text =  "Flower color",
                                                       mod_name = "Plant species model")


# Plant family model
flow_col_p_plant_family_mod <- response_factor_cov_plot(out_df = out_plant_family_df,
                                                      num_cov = 4,
                                                      beta1 = out_plant_family_df$beta_p.7.,
                                                      beta2 = out_plant_family_df$beta_p.8.,
                                                      beta3 = out_plant_family_df$beta_p.9.,
                                                      beta4 = out_plant_family_df$beta_p.1.,
                                                      pal_cols = c( "#E7B800", "#00AFBB","grey", "black"),
                                                      beta1_name = "yellow",
                                                      beta2_name = "blue",
                                                      beta3_name = "white",
                                                      beta4_name = "other",
                                                      x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                      y_lab_text =  "Flower color",
                                                      mod_name = "Plant family model")


# Bee and Plant family model
flow_col_p_bee_plant_family_mod <- response_factor_cov_plot(out_df = out_bee_plant_family_df,
                                                          num_cov = 4,
                                                          beta1 = out_bee_plant_family_df$beta_p.7.,
                                                          beta2 = out_bee_plant_family_df$beta_p.8.,
                                                          beta3 = out_bee_plant_family_df$beta_p.9.,
                                                          beta4 = out_bee_plant_family_df$beta_p.1.,
                                                          pal_cols = c( "#E7B800", "#00AFBB","grey", "black"),
                                                          beta1_name = "yellow",
                                                          beta2_name = "blue",
                                                          beta3_name = "white",
                                                          beta4_name = "other",
                                                          x_lab_text = "Probability of detecting a \nbee interacting on the plant",
                                                          y_lab_text =  "Flower color",
                                                          mod_name = "Bee and plant family model")


# Bee and Plant family model
flow_col_p_no_bee_plant_mod <- response_factor_cov_plot(out_df = out_no_bee_plant_df,
                                                      num_cov = 4,
                                                      beta1 = out_no_bee_plant_df$beta_p.7.,
                                                      beta2 = out_no_bee_plant_df$beta_p.8.,
                                                      beta3 = out_no_bee_plant_df$beta_p.9.,
                                                      beta4 = out_no_bee_plant_df$beta_p.1.,
                                                      pal_cols = c( "#E7B800", "#00AFBB","grey", "black"),
                                                      beta1_name = "yellow",
                                                      beta2_name = "blue",
                                                      beta3_name = "white",
                                                      beta4_name = "other",
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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-p-flow-col-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
flow_col_p_stats <- rbind(flow_col_p_bee_species_mod$stats_df,      flow_col_p_bee_family_mod$stats_df,
                          flow_col_p_plant_species_mod$stats_df,    flow_col_p_plant_family_mod$stats_df,
                          flow_col_p_bee_plant_family_mod$stats_df, flow_col_p_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_col_p_stats, paste0("./Tables/", date_folder, "/Supp-table-p-flow-col-mod-comparison.csv"))








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
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-p-flow-sha-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
flow_sha_p_stats <- rbind(flow_sha_p_bee_species_mod$stats_df,      flow_sha_p_bee_family_mod$stats_df,
                             flow_sha_p_plant_species_mod$stats_df,    flow_sha_p_plant_family_mod$stats_df,
                             flow_sha_p_bee_plant_family_mod$stats_df, flow_sha_p_no_bee_plant_mod$stats_df )



# Save the table
write.csv(flow_sha_p_stats, paste0("./Tables/", date_folder, "/Supp-table-p-flow-sha-mod-comparison.csv"))










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
                                                          n_sub = 3)

# Bee family model
bee_size_p_bee_family_mod <- response_continous_cov_plot(out_df = out_bee_family_df, 
                                           beta = out_bee_family_df$beta_p.3.,
                                           intercept = out_bee_family_df$beta_p.1.,
                                           x_lab_text = "Bee size standardized",
                                           y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                           mod_name = "Bee family model", 
                                           n_sub = 3)



# Plant species model
bee_size_p_plant_species_mod <- response_continous_cov_plot(out_df = out_plant_species_df, 
                                              beta = out_plant_species_df$beta_p.3.,
                                              intercept = out_plant_species_df$beta_p.1.,
                                              x_lab_text = "Bee size standardized",
                                              y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                            mod_name = "Plant species model", 
                                            n_sub = 3)

# Plant family model
bee_size_p_plant_family_mod <- response_continous_cov_plot(out_df = out_plant_family_df, 
                                             beta = out_plant_family_df$beta_p.3.,
                                             intercept = out_plant_family_df$beta_p.1.,
                                             x_lab_text = "Bee size standardized",
                                             y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                             mod_name = "Plant family model", 
                                             n_sub = 3)


# Bee and Plant family model
bee_size_p_bee_plant_family_mod <- response_continous_cov_plot(out_df = out_bee_plant_family_df, 
                                                 beta = out_bee_plant_family_df$beta_p.3.,
                                                 intercept = out_bee_plant_family_df$beta_p.1.,
                                                 x_lab_text = "Bee size standardized",
                                                 y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                                 mod_name = "Bee and plant family model", 
                                                 n_sub = 3)



# Bee and Plant family model
bee_size_p_no_bee_plant_mod <- response_continous_cov_plot(out_df = out_no_bee_plant_df, 
                                             beta = out_no_bee_plant_df$beta_p.3.,
                                             intercept = out_no_bee_plant_df$beta_p.1.,
                                             x_lab_text = "Bee size standardized",
                                             y_lab_tex = "Probability of detecting \nthe bee on a plant",
                                             mod_name = "No bee and plant model", 
                                             n_sub = 3)



# Stitch the plots together
(bee_size_p_bee_species_mod$gplot      + bee_size_p_bee_family_mod$gplot+
 bee_size_p_plant_species_mod$gplot    + bee_size_p_plant_family_mod$gplot+
 bee_size_p_bee_plant_family_mod$gplot + bee_size_p_no_bee_plant_mod$gplot ) + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0("./Figures/", date_folder, "/Supp-fig-p-bee-size-mod-comparison.png")
       , height = 6, width = 15)




# Stitch together the stats
bee_size_p_stats <- rbind(bee_size_p_bee_species_mod$stats_df,      bee_size_p_bee_family_mod$stats_df,
                        bee_size_p_plant_species_mod$stats_df,    bee_size_p_plant_family_mod$stats_df,
                        bee_size_p_bee_plant_family_mod$stats_df, bee_size_p_no_bee_plant_mod$stats_df )



# Save the table
write.csv(bee_size_p_stats, paste0("./Tables/", date_folder, "/Supp-table-p-bee-size-mod-comparison.csv"))






# End script