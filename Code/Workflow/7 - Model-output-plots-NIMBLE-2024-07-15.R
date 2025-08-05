########################################
########################################
# This code was written by: G. V. DiRenzo & M. J. Lee
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code objective ##########
##################################


# To create plots from the model output


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


# Add globi folder name
globi_folder <- "globi-20250210"

# Set working directory
setwd(paste0("/Volumes/DIRENZO/", globi_folder, "/gdirenzo/globi/"))
#setwd("/Users/gdirenzo/OneDrive - University of Massachusetts/Dropbox_transfer/Globi/")
setwd(paste0("/Volumes/DIRENZO/globi20250715-ModOutput/"))


# Add github folder path
github_folder_path <- "/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/"



# date object for folder
date_folder <- "2025 07 15"
date <- "2025 01 26"





# 2. Load data -------------------------------------------------------





#---- bee_species model

# Model name
mod_name <- "bee_species"

# Load the model output - out
load(file = paste0("./ModelOutput/", date, "/out-"
                   , mod_name, "-NIMBLE.rds"))


# Save the out object with a new model specific name
out_bee_species <- out

out_bee_species_df <- as.data.frame(out_bee_species)




#------------  Upload the data & format
# object name = bee.plant.cite
# 3-D array
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")



# Read in the dat_info
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/dat_info_2025_01_22.rds")
# object name = dat_info



# Load covariates
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/model_covariates - 2025 01 22 - no apis.rds")
# object name = covariates



# Load in the number of observations
load("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/Data/obs_dat-2025-02-11.rds")
# object name = obs_dat








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


# # Create a dataframe with the parameter estimates from each MCMC iteraction
 param_df <- data.frame(beta_psi_B_size = out_df$beta_psi.2., 
                        mu.psi = out_df$beta_psi.1.,
                        iteration = 1:length(out_df$beta_psi.1.))
 # Create a vector of size values
 size_values <- seq(-3, 3, length.out = 50)
 
 # Create pred.psi more carefully
 pred.psi <- param_df %>%
   crossing(Size.scaled = size_values)
 
 
 # Add predictions
 pred.psi$pred <- plogis(pred.psi$mu.psi + 
                         pred.psi$beta_psi_B_size * pred.psi$Size.scaled)
 
 # Look at head
 head(pred.psi)
 
 # Round to ensure grouping works properly
 pred.psi$Size.scaled <- round(pred.psi$Size.scaled, 2)
 
 # Group by Size.scaled and calculate quantiles
 size_vals <- unique(pred.psi$Size.scaled)
 
 # Create an empty data frame
 credible_intervals <- data.frame(
   Size.scaled = size_vals,
   lower_95 = NA,
   upper_95 = NA,
   lower_80 = NA,
   upper_80 = NA,
   lower_50 = NA,
   upper_50 = NA,
   mean_pred = NA
 )
 
 # Loop through all calculations for the credible intervals
 for(i in 1:length(size_vals)) {
   
   subset_data <- pred.psi$pred[pred.psi$Size.scaled == size_vals[i]]
   
   credible_intervals$lower_95[i] <- quantile(subset_data, 0.025)
   credible_intervals$upper_95[i] <- quantile(subset_data, 0.975)
   credible_intervals$lower_80[i] <- quantile(subset_data, 0.10)
   credible_intervals$upper_80[i] <- quantile(subset_data, 0.90)
   credible_intervals$lower_50[i] <- quantile(subset_data, 0.25)
   credible_intervals$upper_50[i] <- quantile(subset_data, 0.75)
     
   credible_intervals$me_pred[i] <- median(subset_data)
   
}
   
   # Create plot
   bee.size.plot <- ggplot(data = credible_intervals) +
     # 95% CI - lightest
     geom_ribbon(aes(x = Size.scaled, ymin = lower_95, ymax = upper_95), 
                 alpha = 0.2, fill = "darkblue") +
     # 80% CI - medium
     geom_ribbon(aes(x = Size.scaled, ymin = lower_80, ymax = upper_80), 
                 alpha = 0.3, fill = "darkblue") +
     # 50% CI - darkest
     geom_ribbon(aes(x = Size.scaled, ymin = lower_50, ymax = upper_50), 
                 alpha = 0.4, fill = "darkblue") +
     # Mean or median (depending on how it is calculated) line
     geom_line(aes(x = Size.scaled, y = me_pred), 
               col = "darkblue", linewidth = 1.5)  +  
     ylab("Probability of interacting \nwith a plant") +
     xlab("Bee size standardized") +
     theme(legend.position = "none",
           strip.background = element_rect(colour = "black", fill = "white"),
           strip.text = element_text(size = title.size), 
           panel.background = element_rect(colour = "black", fill = NA),
           axis.text.x = element_text(size = text.size),
           axis.text.y = element_text(size = text.size),
           axis.title.x = element_text(size = title.size),
           axis.title.y = element_text(size = title.size))
 

# bee.size.plot
 
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
                                 "Social")

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
  ylab("Sociality")+
  xlim(c(0, 1))+
  theme_bw()+ 
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size, color = "black"), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size, color = "black"),
        axis.text.y = element_text(size = text.size, color = "black"),
        axis.title.x = element_text(size = title.size, color = "black"),
        axis.title.y = element_text(size = title.size, color = "black"))

# # # solitary.plot
# # # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-interaction-prob-V-bee-solitary.pdf"), 
#        height = 3, width = 5)











# 9. Plot the relationship between psi and flower color ----------------------------------------



# Probability of one parameter > another

# yellow > other
mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.4.))

# yellow > blue
mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.5.))

# yellow > white
mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.6.))




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
 
 
 ## Add a column with the covariate
 pred.psi.col$other <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_other) 
 pred.psi.col$blue <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_blue) 
 pred.psi.col$white <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_white) 
 pred.psi.col$yellow  <- plogis(pred.psi.col$mu.psi) 
 
 # Convert to long format
 pred.psi.col.long <- melt(pred.psi.col[,6:9])
 
 # Change column names
 colnames(pred.psi.col.long) <- c("color", "probability")
 
 # Calcilate group means
 mu <- ddply(pred.psi.col.long, "color", summarise, 
             grp.mean=mean(probability))
 
 head(mu)

 pred.psi.col.long$color_num <- as.numeric(pred.psi.col.long$color)
 
 # Annotation data frame
 annotation_df <- data.frame(
   start =  c("yellow", "yellow", "yellow"),
   end = c("other", "blue", "white"),
   probability = c(0.9, 0.95, 1.0),
   label = c(mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.4.)),
             mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.5.)),
             mean(out_df$beta_psi.1. > (out_df$beta_psi.1. + out_df$beta_psi.6.))
 ))
 
 # Arrange the colors in the order we want them displayed
 pred.psi.col.long$color <- factor(pred.psi.col.long$color, levels = c("yellow", 
                                                                       "other",
                                                                       "blue", 
                                                                       "white"))
 
 
 # Create a mapping of color to numeric positions
 color_levels <- levels(pred.psi.col.long$color)
 color_map <- setNames(seq_along(color_levels), color_levels)
 
 # Adjust annotation_df to use numeric positions
 annotation_df$y_start <- color_map[annotation_df$start]
 annotation_df$y_end <- color_map[annotation_df$end]
 
 pal_cols <- c("#E7B800", "grey", "#00AFBB", "white")
 

 # Create the plot
 flower.color.plot <- ggplot(pred.psi.col.long, aes(x = probability, y = color)) +
   geom_density_ridges(aes(fill = color)) +
   scale_fill_manual(values = pal_cols)+
   geom_vline(data= mu, aes(xintercept=grp.mean, color = color),
              linetype="dashed")+
   scale_color_manual(values = pal_cols)+
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
   geom_text(data = annotation_df, aes(x = probability + 0.025, 
                                       y = (y_start + y_end) / 2,
                                       label = round(label, 2)), size = 2.2)
 
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
 bowl_prob <- plogis(pred.psi.shape$mu.psi + pred.psi.shape$beta_psi_F_shape)
 mean(bowl_prob)
 quantile(bowl_prob, c(0.025, 0.975))
 
 notBowl_prob <- plogis(pred.psi.shape$mu.psi)
 mean(notBowl_prob)
 quantile(notBowl_prob, c(0.025, 0.975))
 
 
 # Calculate the differences:
 notbowl_vs_bowl <- notBowl_prob  - bowl_prob
 mean(notbowl_vs_bowl)
 quantile(notbowl_vs_bowl, c(0.025, 0.975))
 
 
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
 
 bowl.plot
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
 
 # Convert to long format
 pred.p.long <- melt(pred.p[,4:5])
 
 # Change column names
 colnames(pred.p.long) <- c("stripe", "probability")
 
 # Change labels
 pred.p.long$stripe <- ifelse(pred.p.long$stripe == "stripped",
                              "Striped",
                              "Not \nstriped")
 
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
   xlab("Probability of detecting \na bee on a plant")+
   ylab("Bee stripes")+
 theme_bw()+ 
   theme(legend.position = "none",
         strip.background = element_rect(colour = "black", fill = "white"),
         strip.text = element_text(size = title.size, color = "black"), 
         panel.background = element_rect(colour = "black", fill = NA),
         axis.text.x = element_text(size = text.size, color = "black"),
         axis.text.y = element_text(size = text.size, color = "black"),
         axis.title.x = element_text(size = title.size, color = "black"),
         axis.title.y = element_text(size = title.size, color = "black"))
 
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
  
  
  
  # Create an empty data frame
  credible_intervals_p <- data.frame(
    Size.scaled = size_vals,
    lower_95 = NA,
    upper_95 = NA,
    lower_80 = NA,
    upper_80 = NA,
    lower_50 = NA,
    upper_50 = NA,
    me_pred = NA
  )
  
  # Loop through all calculations for the credible intervals
  for(i in 1:length(size_vals)) {
    
    subset_data <- pred.p.size$pred[pred.p.size$Size.scaled == size_vals[i]]
    
    credible_intervals_p$lower_95[i] <- quantile(subset_data, 0.025)
    credible_intervals_p$upper_95[i] <- quantile(subset_data, 0.975)
    credible_intervals_p$lower_80[i] <- quantile(subset_data, 0.10)
    credible_intervals_p$upper_80[i] <- quantile(subset_data, 0.90)
    credible_intervals_p$lower_50[i] <- quantile(subset_data, 0.25)
    credible_intervals_p$upper_50[i] <- quantile(subset_data, 0.75)
    
    credible_intervals_p$me_pred[i] <- median(subset_data)
    
    }
  
  
  # Create plot
  bee.size.plot.p <-  ggplot(data = credible_intervals_p) +
    # 95% CI - lightest
    geom_ribbon(aes(x = Size.scaled, ymin = lower_95, ymax = upper_95), 
                alpha = 0.2, fill = "darkblue") +
    # 80% CI - medium
    geom_ribbon(aes(x = Size.scaled, ymin = lower_80, ymax = upper_80), 
                alpha = 0.3, fill = "darkblue") +
    # 50% CI - darkest
    geom_ribbon(aes(x = Size.scaled, ymin = lower_50, ymax = upper_50), 
                alpha = 0.4, fill = "darkblue") +
    # Mean or median (depending on how it is calculated) line
    geom_line(aes(x = Size.scaled, y = me_pred), 
              col = "darkblue", linewidth = 1.5)  +  
    ylab("Probability of detecting \nthe bee on a plant")+
    xlab("Bee size standardized") +
    theme(legend.position = "none",
          strip.background = element_rect(colour = "black", fill = "white"),
          strip.text = element_text(size = title.size), 
          panel.background = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = text.size),
          axis.text.y = element_text(size = text.size),
          axis.title.x = element_text(size = title.size),
          axis.title.y = element_text(size = title.size))
  
  
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

 citation.plot
# 
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-citation.pdf"), 
#        height = 3, width = 5)
# 









# 13. Plot the relationship between p and flower color ----------------------------------------




# yellow > other
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.7.))

# yellow > blue
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.8.))

# yellow > white
mean(out_df$beta_p.1. > (out_df$beta_p.1. + out_df$beta_p.9.))




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



# Convert to long format
pred.p.col.long <- melt(pred.p.col[,6:9])

# Change column names
colnames(pred.p.col.long) <- c("color", "probability")

# Calcilate group means
mu <- ddply(pred.p.col.long, "color", summarise, 
            grp.mean=mean(probability))

head(mu)

# Arrange the colors in the order we want them displayed
pred.p.col.long$color <- factor(pred.p.col.long$color, levels = c("yellow", 
                                                                      "other",
                                                                      "blue", 
                                                                      "white"))

mu$color <- factor(mu$color, levels = c("yellow", 
                                         "other",
                                         "blue", 
                                         "white"))

# Create the plot
flow.col.plot.p <- ggplot(pred.p.col.long, aes(x = probability, y = color)) +
  geom_density_ridges(aes(fill = color)) +
  scale_fill_manual(values = pal_cols)+
  scale_color_manual(values = pal_cols)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=color),
             linetype="dashed")+
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

flow.shape.plot.p
# # Save the plot
# ggsave(paste0("./Figures/", date_folder, "/Bee-plant-detection-prob-V-shape.pdf"), 
#        height = 3, width = 5)











# 11. Combined detection figure ----------------------------------------





((strip.plot + bee.size.plot.p + 
    flow.col.plot.p + flow.shape.plot.p)/  citation.plot) + plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')


# Save the plot
ggsave(paste0(github_folder_path, "/Figures/", date_folder, "/Fig-2-Bee-plant-detection-prob.png")
       , height = 6, width = 15)



# End script
