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
library(plyr)
library(coda)
library(reshape2)


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Load data -------------------------------------------------------




###################
# Determine if you are looking at the results with or without apis
###################



#------------------ WITH Apis


# Load the data
load("./ModelOutput/globi-short plant list- 2022 04 05 - all cov - apis - NIMBLE.rds")
  # object = out
  # MCMC object


## Load the entire nimble output
load("./ModelOutput/OUTPUT - globi-short plant list- 2022 04 05 - all cov - apis - NIMBLE.rds")
  # object = result
  # MCMC object



# Upload the data
 load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_05 - short plant list.rds")
 # object name = bee.plant.date.cite
 # 4-D array


# Load the possible bee-plant-interactions
  load("./Data/bee_plant_inter_2022_04_05 - short plant.rds")
  # object = bee.plant.inter
  # 2-D matrix
  





#------------------ WITHOUT Apis


  

  
 # Load the data
 load("./ModelOutput/globi-short plant list- 2022 04 05 - all cov - NO apis - NIMBLE.rds")
 # object = out
 # MCMC object
 
 
 ## Load the entire nimble output
# load("./ModelOutput/OUTPUT - globi-short plant list- 2022 04 05 - all cov - NO apis - NIMBLE.rds")
 # object = result
 # MCMC object
 
 
 
 # Upload the data
 load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_05 - short plant list - no apis.rds")
 # object name = bee.plant.date.cite
 # 4-D array
 
 
 # Load the possible bee-plant-interactions
 load("./Data/bee_plant_inter_2022_04_05 - short plant - no apis.rds")
 # object = bee.plant.inter
 # 2-D matrix
  



###################
###################
###################






# 3. Summarize the observed data, u, v, and z parameters/states ----------------------------------------




# Summarize the observed data

# We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(bee.plant.date.cite, c(1, 2), max, na.rm = TRUE)
y.bee.plant[y.bee.plant == "-Inf"] <- 0


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Observed number of plant species that each bee interacts with
obs.dat <- data.frame(obs = y.bee.plant)


# Determine how many MCMC iterations to keep
row.subset <- 200

# Summarize z
# # Latent state variables
out_z <- as.mcmc(data.frame(rbind(result[[1]]$samples2[row.subset,grep("z", colnames(result[[1]]$samples2))],
                                  result[[2]]$samples2[row.subset,grep("z", colnames(result[[1]]$samples2))],
                                  result[[3]]$samples2[row.subset,grep("z", colnames(result[[1]]$samples2))])))



# Extract the mean values for z = true bee, plant, by month interactions
# Look at the number of dimensions
dim(out_z[,grep("z", colnames(out_z))])

# Number of MCMC iterations
MCMC <- nrow(out_z)

# We will put those values in an array
out2_array <- array(out_z[,grep("z", colnames(out_z))], 
                    dim = c(MCMC, 
                            dim(bee.plant.date.cite)[1:3]))

# look at dimensions
dim(out2_array)


# Row names - MCMC iterations
rownames(out2_array) <- 1:MCMC
  
# column names - bee species
colnames(out2_array) <- rownames(bee.plant.date.cite)

# sheet names - plant species
dimnames(out2_array)[[3]] <- colnames(bee.plant.date.cite) 

# 4th dimension - 
dimnames(out2_array)[[4]] <- dimnames(bee.plant.date.cite)[[3]]


# # Check to make sure the array is filled in correctly
# bee.plant.inter[bee.plant.inter$beeID == 4 & 
#                   bee.plant.inter$plantID == 1,]
# 
# 
# out2_array[1:3, 1:4, 1:3, 3]


# Now, we want to sum the number of unique plants per bee per month
# First - we will calculate the mean 

# To do this, first we will determine if the bee EVER interacts with the plant
bee.plant.mean<- apply(out2_array, c(2, 3, 4), mean, na.rm = TRUE)
bee.plant <- apply(bee.plant.mean, c(1, 2), max, na.rm = TRUE)
bee.plant[bee.plant == "-Inf"] <- NA
# And then, we will sum across plant IDs
bee.interactions <- apply(bee.plant, 1, sum, na.rm = TRUE)


# Repeat steps for 95% CI - lower & upper
# Calculate the lower 95% CI across MCMC iterations
  # bee x plant x month
bee.plant.lower.95 <- apply(out2_array, c(2, 3, 4), function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[1])
  # bee x plant
bee.plant.lower <- apply(bee.plant.lower.95, c(1, 2), max, na.rm = TRUE)
bee.plant.lower[bee.plant.lower == "-Inf"] <- NA
  # bee
bee.interactions.lower <- apply(bee.plant.lower, 1, sum, na.rm = TRUE)

bee.plant.upper.95 <- apply(out2_array, c(2, 3, 4), function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[2])
bee.plant.upper <- apply(bee.plant.upper.95, c(1, 2), max, na.rm = TRUE)
bee.plant.upper[bee.plant.upper == "-Inf"] <- NA
bee.interactions.upper <- apply(bee.plant.upper, 1, sum, na.rm = TRUE)



# Summarize u

# # Latent state variables
 out_u <- as.mcmc(data.frame(rbind(result[[1]]$samples2[row.subset,grep("u", colnames(result[[1]]$samples2))],
                                   result[[2]]$samples2[row.subset,grep("u", colnames(result[[1]]$samples2))],
                                   result[[3]]$samples2[row.subset,grep("u", colnames(result[[1]]$samples2))])))
 


# Extract the mean values for z = true bee, plant, by month interactions
# Look at the number of dimensions
dim(out_u[,grep("u", colnames(out_u))])

# We will put those values in an array
out_u_array <- out_u[,grep("u", colnames(out_u))]

# Row names - MCMC iterations
rownames(out_u_array) <- 1:MCMC

# column names - bee species
colnames(out_u_array) <- rownames(bee.plant.date.cite)

# Now, we calculate the mean & 95% CI
# To do this, first we will determine if the bee EVER interacts with the plant
u.mean <- apply(out_u_array, c(2), mean, na.rm = TRUE)
u.lower <- apply(out_u_array, c(2), function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[1])
u.upper <- apply(out_u_array, c(2), function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[2])


# Summarize v

# # Latent state variables
 out_v <- as.mcmc(data.frame(rbind(result[[1]]$samples2[row.subset,grep("v", colnames(result[[1]]$samples2))],
                                   result[[2]]$samples2[row.subset,grep("v", colnames(result[[1]]$samples2))],
                                   result[[3]]$samples2[row.subset,grep("v", colnames(result[[1]]$samples2))])))
 


dim(out_v[,grep("v", colnames(out_v))])

# We will put those values in an array
out_v_array <- out_v[,grep("v", colnames(out_v))]

# Row names - MCMC iterations
rownames(out_v_array) <- 1:MCMC

# column names - bee species
colnames(out_v_array) <- rownames(bee.plant.date.cite)

# Now, we calculate the mean & 95% CI
# To do this, first we will determine if the bee EVER interacts with the plant
v.mean <- apply(out_v_array, c(2), mean, na.rm = TRUE)
v.lower <- apply(out_v_array, c(2), function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[1])
v.upper <- apply(out_v_array, c(2), function(x)quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)[2])







# Combine the names, and model output
dat <- data.frame(names = c(paste(rownames(bee.plant.date.cite), "interact prob"),
                            paste(rownames(bee.plant.date.cite), "detect prob"),
                            rownames(bee.plant.date.cite)), 
                  obs = c(rep(NA, times = nrow(bee.plant.date.cite)),
                          rep(NA, times = nrow(bee.plant.date.cite)),
                          obs.dat$obs), 
                  mod.mean = c( u.mean,
                                v.mean,
                                bee.interactions), 
                  mod.q2.5 = c(u.lower,
                               v.lower,
                               bee.interactions.lower), 
                  mod.q97.5 = c(u.upper,
                                v.upper,
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




out_df <- as.data.frame(out)


# Create a dataframe with the parameter estimates from each MCMC iteraction
param_df <- data.frame(beta_psi_B_size = out_df$beta_psi_B_size, 
                       mu.psi = out_df$mu.psi,
                       iteration = 1:length(out_df$mu.psi))

## Add a column with the covariate
pred.psi <- expand_grid(param_df, 
                         tibble(Size.scaled = seq(-3, 3, length.out = 50)))


# Add predictions
pred.psi$pred <- plogis(pred.psi$mu.psi + 
                        pred.psi$beta_psi_B_size * pred.psi$Size.scaled)

# Look at head
head(pred.psi)


# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(param_df), 500, replace = FALSE)

# Subset the data
pred.psi.sub <- pred.psi[pred.psi$iteration %in% sub.samp,]



# Calculate the mean value for the relationship
mean_psi_size <- data.frame(Size.scaled = seq(-3, 3, length.out = 50))


mean_psi_size$pred <- plogis(mean(out_df$mu.psi) +
                             mean(out_df$beta_psi_B_size) + mean_psi_size$Size.scaled)


# Specify text size
text.size <- 20
title.size <- 22




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
ggsave("./Figures/2022_04_05/APIS - Bee-plant-interaction-prob-V-bee-size.pdf", height = 10, width =11)








# 8. Plot the relationship between psi and sociality ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.soc <- data.frame(beta_psi_B_sociality = out_df$beta_psi_B_sociality, 
                         mu.psi = out_df$mu.psi,
                         iteration = 1:length(out_df$mu.psi))

## Add a column with the covariate
pred.p.soc$social <- plogis(pred.p.soc$mu.psi + pred.p.soc$beta_psi_B_sociality) 
pred.p.soc$notSocial <- plogis(pred.p.soc$mu.psi) 

# Convert to long format
pred.p.soc.long <- melt(pred.p.soc[,4:5])

# Change column names
colnames(pred.p.soc.long) <- c("social", "probability")

# Change labels
pred.p.soc.long$social <- ifelse(pred.p.soc.long$social == "social",
                                 "Social",
                                 "Not social")

# Calcilate group means
mu <- ddply(pred.p.soc.long, "social", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 20
title.size <- 22



# Create the plot
ggplot(pred.p.soc.long, aes(x = probability, y = social)) +
  geom_density_ridges(aes(fill = social)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=social),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant interaction probability")+
  ylab("Sociality")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-interaction-prob-V-bee-sociality.pdf", height = 10, width = 11)











# 8. Plot the relationship between psi and flower color ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.psi.col <- data.frame(beta_psi_F_color = out_df$beta_psi_F_color, 
                         mu.psi = out_df$mu.psi,
                         iteration = 1:length(out_df$mu.psi))

## Add a column with the covariate
pred.psi.col$yellow <- plogis(pred.psi.col$mu.psi + pred.psi.col$beta_psi_F_color) 
pred.psi.col$notYellow <- plogis(pred.psi.col$mu.psi) 

# Convert to long format
pred.psi.col.long <- melt(pred.psi.col[,4:5])

# Change column names
colnames(pred.psi.col.long) <- c("yellow", "probability")

# Change labels
pred.psi.col.long$yellow <- ifelse(pred.psi.col.long$yellow == "yellow",
                                 "Yellow",
                                 "Not Yellow")

# Calcilate group means
mu <- ddply(pred.psi.col.long, "yellow", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 20
title.size <- 22



# Create the plot
ggplot(pred.psi.col.long, aes(x = probability, y = yellow)) +
  geom_density_ridges(aes(fill = yellow)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=yellow),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant interaction probability")+
  ylab("Flower color")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-interaction-prob-V-flower-color.pdf", height = 10, width = 11)











# 8. Plot the relationship between psi and flower shape ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.psi.shape <- data.frame(beta_psi_F_shape = out_df$beta_psi_F_shape, 
                              mu.psi = out_df$mu.psi,
                              iteration = 1:length(out_df$mu.psi))

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


# Specify text size
text.size <- 20
title.size <- 22



# Create the plot
ggplot(pred.psi.shape.long, aes(x = probability, y = bowl)) +
  geom_density_ridges(aes(fill = bowl)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=bowl),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant interaction probability")+
  ylab("Flower shape")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-interaction-prob-V-flower-shape.pdf", height = 10, width = 11)
















# 8. Plot the relationship between p and strippiness ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_B_stripped = out_df$beta_p_B_stripped, 
                     mu.p = out_df$mu.p,
                     iteration = 1:length(out_df$mu.psi))

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
                             "Not stripped")

# Calcilate group means
mu <- ddply(pred.p.long, "stripe", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 20
title.size <- 22



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
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-detection-prob-V-bee-stripes.pdf", height = 10, width = 11)









# 8. Plot the relationship between p and source ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p <- data.frame(beta_p_source = out_df$beta_p_source, 
                     mu.p = out_df$mu.p,
                     iteration = 1:length(out_df$mu.psi))

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
mu <- ddply(pred.p.long, "citation", 
            summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 20
title.size <- 22




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
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-detection-prob-V-citation-type.pdf", height = 10, width = 11)












# 8. Plot the relationship between p and flower color ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.col <- data.frame(beta_p_F_color = out_df$beta_p_F_color, 
                           mu.p = out_df$mu.p,
                           iteration = 1:length(out_df$mu.p))

## Add a column with the covariate
pred.p.col$yellow <- plogis(pred.p.col$mu.p + pred.p.col$beta_p_F_color) 
pred.p.col$notYellow <- plogis(pred.p.col$mu.p) 


# Convert to long format
pred.p.col.long <- melt(pred.p.col[,4:5])

# Change column names
colnames(pred.p.col.long) <- c("yellow", "probability")

# Change labels
pred.p.col.long$yellow <- ifelse(pred.p.col.long$yellow == "yellow",
                                   "Yellow",
                                   "Not Yellow")

# Calcilate group means
mu <- ddply(pred.p.col.long, "yellow", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 20
title.size <- 22



# Create the plot
ggplot(pred.p.col.long, aes(x = probability, y = yellow)) +
  geom_density_ridges(aes(fill = yellow)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=yellow),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant detection probability")+
  ylab("Flower color")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-detection-prob-V-flower-color.pdf", height = 10, width = 11)











# 8. Plot the relationship between p and flower shape ----------------------------------------






# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.shape <- data.frame(beta_p_F_shape = out_df$beta_p_F_shape, 
                           mu.p = out_df$mu.p,
                           iteration = 1:length(out_df$mu.p))

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
                                   "Not Bowl")

# Calcilate group means
mu <- ddply(pred.p.shape.long, "bowl", summarise, 
            grp.mean=mean(probability))

head(mu)


# Specify text size
text.size <- 20
title.size <- 22



# Create the plot
ggplot(pred.p.shape.long, aes(x = probability, y = bowl)) +
  geom_density_ridges(aes(fill = bowl)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=bowl),
             linetype="dashed")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("Bee-plant detection probability")+
  ylab("Flower shape")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))


# Save the plot
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-detection-prob-V-flower-shape.pdf", height = 10, width = 11)











# 7. Plot the relationship between p and bee size ----------------------------------------



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.size <- data.frame(beta_p_B_size = out_df$beta_p_B_size, 
                       mu.p = out_df$mu.p,
                       iteration = 1:length(out_df$mu.p))

## Add a column with the covariate
pred.p.size <- expand_grid(pred.p.size, 
                        tibble(Size.scaled = seq(-3, 3, length.out = 50)))


# Add predictions
pred.p.size$pred <- plogis(pred.p.size$mu.p + 
                           pred.p.size$beta_p_B_size * 
                           pred.p.size$Size.scaled)


# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(param_df), 500, replace = FALSE)

# Subset the data
pred.p.size.sub <- pred.p.size[pred.p.size$iteration %in% sub.samp,]





# Calculate the mean value for the relationship
mean_p_size <- data.frame(Size.scaled = seq(-3, 3, length.out = 50),
                          mu.p = mean(out_df$mu.p),
                          beta_p_B_size = mean(out_df$beta_p_B_size))


mean_p_size$pred <- plogis(mean_p_size$mu.p +
                             mean_p_size$beta_p_B_size * mean_p_size$Size.scaled)


# Specify text size
text.size <- 20
title.size <- 22




# Create plot
ggplot() +
  geom_line(data = pred.p.size.sub, aes(x = as.numeric(Size.scaled), 
                                     y = as.numeric(pred), 
                                     col = as.factor(iteration)), 
            alpha = .4) +
  geom_line(data = mean_p_size, aes(x = Size.scaled,
                                      y = pred))+
  ylab("Bee-plant detection probability")+
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
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-detection-prob-V-bee-size.pdf", height = 10, width =11)











# 7. Plot the relationship between p and quadratic month ----------------------------------------



# Create a dataframe with the parameter estimates from each MCMC iteraction
pred.p.month <- data.frame(beta_p_month_1 = out_df$beta_p_month_1, 
                           beta_p_month_2 = out_df$beta_p_month_2, 
                            mu.p = out_df$mu.p,
                            iteration = 1:length(out_df$mu.p))

## Add a column with the covariate
pred.p.month <- expand_grid(pred.p.month, 
                           tibble(Month = 1:12))


# Add predictions
pred.p.month$pred <- plogis(pred.p.month$mu.p + 
                             pred.p.month$beta_p_month_1 * pred.p.month$Month +
                              pred.p.month$beta_p_month_2 * (pred.p.month$Month)^2
                             )


# Take a subsample of the iteractions
sub.samp <- sample(1:nrow(param_df), 500, replace = FALSE)

# Subset the data
pred.p.month.sub <- pred.p.month[pred.p.month$iteration %in% sub.samp,]





# Calculate the mean value for the relationship
mean_p_month <- data.frame(Month = 1:12,
                          mu.p = mean(out_df$mu.p),
                          beta_p_month_1 = mean(out_df$beta_p_month_1),
                          beta_p_month_2 = mean(out_df$beta_p_month_2))


mean_p_month$pred <- plogis(mean_p_month$mu.p +
                              mean_p_month$beta_p_month_1 * mean_p_month$Month +
                              mean_p_month$beta_p_month_2 * (mean_p_month$Month)^2)


# Specify text size
text.size <- 20
title.size <- 22




# Create plot
ggplot() +
  geom_line(data = pred.p.month.sub, aes(x = as.numeric(Month), 
                                        y = as.numeric(pred), 
                                        col = as.factor(iteration)), 
            alpha = .4) +
  geom_line(data = mean_p_month, aes(x = Month,
                                    y = pred))+
  ylab("Bee-plant detection probability")+
  xlab("Month")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))

# Save the plot
ggsave("./Figures/2022_04_05/NO APIS - Bee-plant-detection-prob-V-month.pdf", height = 10, width =11)







# End script
