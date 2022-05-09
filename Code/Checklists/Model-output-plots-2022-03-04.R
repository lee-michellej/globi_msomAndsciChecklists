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


# 




################################## 
########  Table of Contents ######
################################## 


# 1. Load libraries & set working directory
# 2. Load data
# 3. 


################################## 
################################## 
################################## 




# 1. Load libraries & set working directory -------------------------------------------------------



# Load libraries
library(tidyverse)
library(ggplot2)
library(jagsUI)
library(ggridges)


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Load data -------------------------------------------------------




# Load the data
load("./ModelOutput/globi-short plant list-no apis- 2022 03 04.rds")
  # object = out
  # JAGS object



# Upload the data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_03_04 - short plant list - no apis.rds")
  # object name = bee.plant.date.cite
  # 4-D array




# 3. Summarize the observed data, u, v, and z parameters/states ----------------------------------------




# Summarize the observed data

# We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(bee.plant.date.cite, c(1, 2), max, na.rm = TRUE)
y.bee.plant[y.bee.plant == "-Inf"] <- 0


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Observed number of plant species that each bee interacts with
obs.dat <- data.frame(obs = y.bee.plant)



# Summarize the u, v, and z parameters/states

# Extract the mean values for z = true bee, plant, by month interactions
# Look at the number of dimensions
dim(out$mean$z)

# Now, we want to sum the number of unique plants per bee per month
# To do this, first we will determine if the bee EVER interacts with the plant
bee.plant <- apply(out$mean$z, c(1, 2), max, na.rm = TRUE)
bee.plant[bee.plant == "-Inf"] <- NA

# And then, we will sum across plant IDs
bee.interactions <- apply(bee.plant, 1, sum, na.rm = TRUE)


# Repeat steps for 95% CI - lower & upper
bee.plant.lower <- apply(out$q2.5$z, c(1, 2), max, na.rm = TRUE)
bee.plant.lower[bee.plant.lower == "-Inf"] <- NA
bee.interactions.lower <- apply(bee.plant.lower, 1, sum, na.rm = TRUE)

bee.plant.upper <- apply(out$q97.5$z, c(1, 2), max, na.rm = TRUE)
bee.plant.upper[bee.plant.upper == "-Inf"] <- NA
bee.interactions.upper <- apply(bee.plant.upper, 1, sum, na.rm = TRUE)



# Combine the names, and model output
dat <- data.frame(names = c(paste(rownames(bee.plant.date.cite), "interact prob"),
                            paste(rownames(bee.plant.date.cite), "detect prob"),
                            rownames(bee.plant.date.cite)), 
                  obs = c(rep(NA, times = nrow(bee.plant.date.cite)),
                          rep(NA, times = nrow(bee.plant.date.cite)),
                          obs.dat$obs), 
                  mod.mean = c( out$mean$u,
                                out$mean$v,
                                bee.interactions), 
                  mod.q2.5 = c(out$q2.5$u,
                               out$q2.5$v,
                               bee.interactions.lower), 
                  mod.q97.5 = c(out$q97.5$u,
                                out$q97.5$v,
                                bee.interactions.upper))





# 4. Make the Bee - plant interaction probability plot ----------------------------------------





# Plot with probabilities
ggplot(dat[grep("interact", dat$names),], 
       aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean)) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Interaction estimates') +
  xlab("Species names") +
  ggtitle("Interaction estimates")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the plot
# ggsave("./Figures/Model_params_psi_2022_02_08.pdf", height = 15, width = 8)






# 5. Make the Bee - plant Detection probability plot ----------------------------------------





# Plot with probabilities
ggplot(dat[grep("detect", dat$names),], 
       aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean)) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Detection estimates') +
  xlab("Species names") +
  ggtitle("Detection estimates")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the plot
# ggsave("./Figures/Model_params_p_2022_02_08.pdf", height = 15, width = 8)






# 6. Plot the total number of true vs. observed bee-plant interactions ----------------------------------------




# Object with colors
cols <- c("Observation" = "red", "Estimated" = "black")

# Plot with number of bee-plant interactions
ggplot(dat[is.na(dat$obs) == FALSE,], 
       aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean, col = "Estimated")) +
  geom_point(size = 3, aes(x = names, y = obs, col = "Observation")) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Estimated number of plant interactions per bee') +
  xlab("Species names") +
  ggtitle("Number of bee-plant interactions")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the plot
# ggsave("./Figures/Num_interacting_2021_05_14.pdf", height = 15, width = 8)






# 7. Plot the relationship between psi and bee size ----------------------------------------




# Create a dataframe with the parameter estimates from each MCMC iteraction
param_df <- data.frame(beta_psi_size = out$sims.list$beta_psi_size, 
                       mu.psi = out$sims.list$mu.psi,
                       iteration = 1:length(out$sims.list$mu.psi))

## Add a column with the covariate
pred.psi <- expand_grid(param_df, 
                         tibble(Size.scaled = seq(-3, 3, length.out = 50)))


# Add predictions
pred.psi$pred <- plogis(pred.psi$mu.psi + 
                        pred.psi$beta_psi_size * pred.psi$Size.scaled)

# Look at head
head(pred.psi)


# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(param_df), 500, replace = FALSE)

# Subset the data
pred.psi.sub <- pred.psi[sub.samp,]



# Calculate the mean value for the relationship
mean_psi_size <- data.frame(Size.scaled = seq(-3, 3, length.out = 50))


mean_psi_size$pred <- plogis(out$mean$mu.psi +
                      out$mean$beta_psi_size + mean_psi_size$Size.scaled)


# Specify text size
text.size <- 15
title.size <- 17




# Create plot
ggplot() +
  geom_line(data = pred.psi.sub, aes(x = as.numeric(Size.scaled), 
                                     y = as.numeric(pred), 
                                     col = as.factor(iteration)), 
            alpha = .4) +
  geom_line(data = mean_psi_size, aes(x = Size.scaled,
                                      y = pred))+
  ylab("Bee-plant interaction probability")+
  xlab("Bee size standardized")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))

# Save the plot
ggsave("./Figures/Bee-plant-interaction-prob-V-bee-size.pdf", height = 6, width = 10)





# 8. Plot the relationship between p and strippiness ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_stripped = out$sims.list$beta_p_stripped, 
                     mu.p = out$sims.list$mu.p,
                     iteration = 1:length(out$sims.list$mu.psi))

## Add a column with the covariate
pred.p$stripped <- plogis(pred.p$mu.p + pred.p$beta_p_stripped) 
pred.p$notStripped <- plogis(pred.p$mu.p) 

# Convert to long format
pred.p.long <- melt(pred.p[,4:5])

# Change column names
colnames(pred.p.long) <- c("stripe", "probability")

# Change labels
pred.p.long$stripe <- ifelse(pred.p.long$stripe == "stripped",
                             "Stripped",
                             "Not stripped")

# Calcilate group means
mu <- ddply(pred.p.long, "stripe", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 15
title.size <- 17




# Create the plot
ggplot(pred.p.long, aes(x = probability, y = stripe)) +
  geom_density_ridges(aes(fill = stripe)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=stripe),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant detection probability")+
  ylab("Strippiness")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/Bee-plant-detection-prob-V-bee-stripes.pdf", height = 6, width = 10)









# 8. Plot the relationship between p and source ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_source = out$sims.list$beta_p_source, 
                     mu.p = out$sims.list$mu.p,
                     iteration = 1:length(out$sims.list$mu.psi))

## Add a column with the covariate
pred.p$Observation <- plogis(pred.p$mu.p + pred.p$beta_p_source) 
pred.p$Collection  <- plogis(pred.p$mu.p) 

# Convert to long format
pred.p.long <- melt(pred.p[,4:5])

# Change column names
colnames(pred.p.long) <- c("citation", "probability")

# Change labels
pred.p.long$citation <- ifelse(pred.p.long$citation == "Observation",
                             "Observation",
                             "Collection Specimen")

# Calcilate group means
mu <- ddply(pred.p.long, "citation", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 15
title.size <- 17




# Create the plot
ggplot(pred.p.long, aes(x = probability, y = citation)) +
  geom_density_ridges(aes(fill = citation)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=citation),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant detection probability")+
  ylab("Citation type")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/Bee-plant-detection-prob-V-citation-type.pdf", height = 6, width = 10)




# End script
