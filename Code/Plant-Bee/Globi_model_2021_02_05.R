########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: grace.direnzo@gmail.com
########################################



##################################
######## Objective ###############
##################################


# To determine the number of plants that each bee genus interacts with while accounting for sampling bias


##################################
######## Data info ###############
##################################


# The data were downloaded from the Globi database.
# Observations are obtained from museum collections and research studies worldwide.
# Studies vary in terms of objectives, study design, etc.
# The Globi database consists of presence-only data.
# We subsetted the data to only include bee genera and instances of bees interacting with plants (either as the "source" or "target").

# We do not explicitly model space
# Our questions relate to species 


# Databases are plagued by 2 problems:
  # 1. sampling bias
      # Particular species may be sampled more frequently than others because more is known about them or inference is desired on that species
  # 2. detection bias
      # Species detectability changes over time and space as a result of observers or number of surveys
      # Number of observers, quality of observers, length of survey, survey conditions

# As a result, patterns may be masked (or there are false patterns) because of observation effort

# We propose a solution to the problem of biased databases.





##################################
######## Data clarifications #####
##################################


# 1. Double counting studies? In the target and source

# 2. How to deal with meta-analyses? 
  # What does it mean for the analysis if 2 studies independently visited the same museum collection and recorded an interaction?

# 3. 



##################################
######## Model info ##############
##################################

# To do this, we will analyze the globi data using an N-mixture model

  # N = Number of plants that each bee genus interacts with
      # Covariates:
        # bee genus size

  # p = the probability that a study documents a bee-plant interaction
      # Covariates:
        # bee genus size



##################################
######## Content generated #######
##################################



# This code will generate: 



########################################
########### Model assumptions ##########
########################################


# Model assumptions:
# (1)

# (2)

# (3)

# (4)

# To use the globi dataset, we are assuming that 


########################################
######## Table of Contents #############
########################################


# 1. Load libraries & set working directory
# 6. Write the model in JAGS
# 7. Bundle the data
# 8. MCMC settings
# 9. Run the models 
# 10. Look at model outputs
# 11. Compare model outputs to truth


########################################
########################################
########################################






# 1. Load libraries & set working directory -------------------------------------------------------




# Load libraries
library(jagsUI)
library(ggplot2)
library(tidyverse)
library("PerformanceAnalytics")


# Set working directory
setwd("~/globi_tritrophic_networks/")



# 2. Load data -------------------------------------------------------




# Read in count data
  # This will load object: globi.dat
load("./Data/globi_data_formatted_bee_plant_2020_12_04.rds")

# Save as a data frame
globi.dat <- as.data.frame(globi.dat)

# Look at the dimensions
  # 288 rows = number of bee genera
  # 38 columns = number of studies
dim(globi.dat)

# Read in size data
size <- read.csv("./Data/body_length_jan21.csv")

# Look at data structure
  # There are 162 rows
  # 14 columns - all with different size variables
str(size)





# 3. Format the data -------------------------------------------------------



# We need to make sure that the globi.data (count data) are in the same order as the size data

# Match the genera names in the globi.dat object to the genera names in the size object
  # 127 bee genera in globi.dat match the size genera
globi.dat[,1][globi.dat[,1]  %in% size$genera]

# Which names in globi.dat do not match the names in the size object?
  # 161 bee genera in globi.dat do NOT match the size genera
globi.dat[,1][globi.dat[,1]  %in% size$genera == FALSE]

# 127 genera do match between the datasets + 
  # 161 genera do NOT match between the datasets = 
    # 288 (the total number of rows in globi.dat)

# Save that information
#obj <- data.frame(in.globi.not.size = globi.dat[,1][globi.dat[,1]  %in% size$genera == FALSE])
#
#write.csv(obj, file = "./Data/Do-not-match.csv")
#


# Moving forward, need to subset the data and make sure they are in the same order
# Keep only the rows that match in globi to size
globi.dat <- globi.dat[globi.dat[,1]  %in% size$genera, ]

# Need to remove the rows in size that do not match globi
size <- size[size$genera %in% globi.dat[,1], ]

# total = 127 rows

# Now re-order the rows to make sure they match
# Make sure that the rows are in alphabetical order
size <- size[order(size$genera),]

globi.dat <- globi.dat[order(globi.dat[,1]),]

# Quick visual inspection
data.frame(size.genera = size$genera,
           globi.genera = globi.dat[,1])


# Remove crazy column names
colnames(globi.dat) <- 1:ncol(globi.dat)


# Convert globi data to matrix
globi.dat1 <- matrix(as.numeric(unlist(globi.dat[,-1])), 
                     nrow = nrow(globi.dat),
                     ncol = ncol(globi.dat)-1,
                     byrow = FALSE)

# Standardize the size variable
std.size <- (size$total_av - mean(size$total_av, na.rm = TRUE)) / sd(size$total_av, na.rm = TRUE)



# 4. Explore size -------------------------------------------------------


# Look to see if there are correlations in size data
  # Last 2 columns removed because something funky was happening
chart.Correlation(size[,c(4:12)], histogram=TRUE, pch=19)

# Lots of correlations
  # total_av correlated 0.75 or higher with all other metrics




# 4. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod_count_size.txt")
cat("
model{

# Priors
for(i in 1:n.bee){

 # Average number of plant families each bee genus interacts with
  epsilon[i] ~ dnorm(0, tau.lambda)

  # Probability of documenting interaction
  eta[i] ~ dnorm(0, tau.p)

}

# Intercept
alpha.lambda ~ dnorm(0, 0.01)
# Slope
beta.size ~ dnorm(0, 0.01)
# Random effect
tau.lambda <- 1 / (sd.lambda * sd.lambda)
sd.lambda ~ dgamma(0.01, 0.01)

# Intercept
alpha.p ~ dnorm(0, 0.01)
beta.detect ~ dnorm(0, 0.01)

# Random effect
tau.p <- 1 / (sd.p * sd.p)
sd.p ~ dgamma(0.01, 0.01)

# Overdispersion parameter
  # In poisson = 1
# r ~ dunif(0, 50)

# Model
for(i in 1:n.bee){   # For each bee species


  # N is the true number of plant families that each bee genus interacts with
  # lambda is the average number of plants that each bee genus interacts with
  N[i] ~ dpois(lambda[i])
  
  # Negative binomial
    # p = success parameter
    # r = overdispersion parameter
 #N[i] ~ dnegbin(p[i], r)
 #
 #p[i] <- r/(r+lambda[i])
  
  # Relating 
  log(lambda[i]) <- alpha.lambda #+ beta.size * size[i] #+ epsilon[i]

  for(k in 1:n.study){
  
      # y = the number of plants that bee genus i is observed interacting with in each study k
      # p = the probability that each study detected a plant-bee interaction
      y[i, k] ~ dbinom(p.eff[i,k], N[i])
  
     logit(p.eff[i,k]) <- alpha.p + beta.detect * size[i] #+ eta[i]

  }

}
    


}
",fill=TRUE)
sink()





# 5. Bundle the data ------------------------------------------------





# Put all the data together
jags.data <- list(
  n.bee = dim(globi.dat1)[1],
  n.study = dim(globi.dat1)[2],
  y = globi.dat1,
  size = std.size)



# Initial values
Ninit <- apply(jags.data$y, 1, max, na.rm = TRUE) + 10

inits <- function() {list(
  
  N = Ninit,
  
  alpha.lambda = runif(1, min = 2, max = 4),
  
  beta.size = runif(1, min = 0, max = 3),
  
  alpha.p = runif(1, min = -4, max = -2),
  beta.detect = runif(1, min = 0, max = 3),
  
  #sd.lambda = runif(1, min = 0, max = 3),
  #sd.p = runif(1, min = 0, max = 3),
  
  r = runif(1, min = 0, max = 1)
  
)}


# List parameters to monitor
params <- c("alpha.lambda",
            "beta.size",
            "sd.lambda",
            "r",
            
            "alpha.p", 
            "beta.detect",
            "sd.p",

            "N")




# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 25000
na <- 10000
nb <- 5000
nt <- 10
nc <- 3




# 7. Run the models --------------------------------------------------------





# Run the model
out <- jags(data = jags.data, 
            inits = inits, 
            parameters.to.save = params, 
            model = "./Model/globi_mod_count_size.txt", 
            n.chains = nc, 
            n.thin = nt, 
            n.iter = ni, 
            n.burnin = nb, 
            n.adapt = na,
            parallel = TRUE)


# Beep when done
beepr::beep(2)




# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Make sure all parameters have converged
plot(out)


# Save the output
save(out, file = "./ModelOutput/out_2020_12_04.rda")

out$mean$beta.size
out$q2.5$beta.size
out$q97.5$beta.size




# 9. Visualize the output ----------------------------------------




# We want to visulaize the model estimates 


# Using the detection-nondetection data
# Combine the names, truth, and model output
mod.est <- data.frame(
  names = 1:jags.data$n.bee, 
  mod.mean = c( out$mean$N), 
  mod.q2.5 = c(out$q2.5$N), 
  mod.q97.5 = c(out$q97.5$N),
  obs = apply(jags.data$y, 1, max, na.rm = TRUE))



# Set colors for truth and estimates
cols <- c("Estimated" = "black", "Observed" = "red")


## Make the plots
# Plot the number of parasites per bee species
 ggplot(mod.est, aes(x = names, y = mod.mean, ymin = mod.q2.5, ymax = mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean, col = "Estimated")) +
   geom_point(size = 3, aes(x = names, y = obs, col = "Observed")) +
   scale_colour_manual("Values", values=cols)+
  coord_flip() + 
  ylab('Estimated plant interactions') +
  xlab("Bee species") +
  ggtitle("")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


# Save the output
ggsave("./Figures/2021_02_05/Globi_data_count_2021_02_05.pdf", width = 10, height = 12)







# 10. # of interactions ~ Bee size  ----------------------------------------



# Intercept
alpha <- out$sims.list$alpha.lambda
mean.alpha <- mean(out$sims.list$alpha.lambda)

# slope
beta <- out$sims.list$beta.size
mean.beta <- mean(out$sims.list$beta.size)


## Need to obtain predictions for all sizes
pred <- expand_grid(data.frame(alpha = alpha,
                               beta = beta), 
                    tibble(size = seq(-3, 3, length.out = 50)))

# Add predictions
pred$pred.vals <- exp(pred$alpha + pred$beta * pred$size)


# add iteration number
pred$iter <- rep(1:length(alpha), each = nrow(pred)/length(alpha))

# Mean predictions
mean.pred <- expand.grid(size = seq(-3, 3, length.out = 50))

# Add mean predictions
mean.pred$pred <- exp(mean.alpha + mean.beta * mean.pred$size)

pred <- as.data.frame(pred)

title.size <- 14
text.size <- 12

# Create plot
ggplot() +
  geom_line(data = pred, aes(x = size, 
                         y = pred.vals, 
                         col = as.factor(iter)), alpha = .1) +
  geom_line(data = mean.pred, aes(x = as.numeric(size), y = as.numeric(pred)), size = 1) +
  ylab(expression(paste("Number of bee-plant interactions")))+
  xlab("Average total size of bee-genus")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))

# Save the plot
ggsave("./Figures/2021_02_05/Number of interactions v size.pdf", 
       height = 6, width = 10)







# 10. Detection probability ~ Bee size  ----------------------------------------




# Intercept
alpha.p <- out$sims.list$alpha.p
mean.alpha.p <- mean(out$sims.list$alpha.p)

# slope
beta.p <- out$sims.list$beta.detect
mean.beta.p <- mean(out$sims.list$beta.detect)


## Need to obtain predictions for all sizes
pred.p <- expand_grid(data.frame(alpha = alpha.p,
                               beta = beta.p), 
                    tibble(size = seq(-3, 3, length.out = 50)))

# Add predictions
pred.p$pred.vals <- plogis(pred.p$alpha + pred.p$beta * pred.p$size)


# add iteration number
pred.p$iter <- rep(1:length(alpha.p), each = nrow(pred.p)/length(alpha.p))

# Mean predictions
mean.pred.p <- expand.grid(size = seq(-3, 3, length.out = 50))

# Add mean predictions
mean.pred.p$pred <- plogis(mean.alpha.p + mean.beta.p * mean.pred.p$size)

pred.p <- as.data.frame(pred.p)

title.size <- 14
text.size <- 12

# Create plot
ggplot() +
  geom_line(data = pred.p, aes(x = size, 
                             y = pred.vals, 
                             col = as.factor(iter)), alpha = .1) +
  geom_line(data = mean.pred.p, aes(x = as.numeric(size), y = as.numeric(pred)), size = 1) +
  ylab(expression(paste("Detection probability "), italic(p)))+
  xlab("Average total size of bee-genus")+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = title.size), 
        panel.background = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = text.size),
        axis.text.y = element_text(size = text.size),
        axis.title.x = element_text(size = title.size),
        axis.title.y = element_text(size = title.size))

# Save the plot
ggsave("./Figures/2021_02_05/Detection v size.pdf", 
       height = 6, width = 10)





# 10. Other plots  ----------------------------------------




# Plot the number of plants per bee species
ggplot(mod.est, aes(x = obs, y = mod.mean, ymin = mod.q2.5, ymax = mod.q97.5))+ 
  geom_pointrange(position = position_jitter()) + 
  geom_abline(slope = 1, intercept = 0, lty = 2)+
  scale_colour_manual("Values", values=cols)+
  ylab('Estimated number of plant interactions') +
  xlab("Observed number of plant interactions") +
  ggtitle("")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the output
ggsave("./Figures/Globi_data_estimate_v_observed_2020_12_04.pdf", width = 13, height = 12)



# Stack the data
hist.dat <- data.frame(
    Estimate = rep(c("observed", "estimate"), each = length(mod.est$obs)),
    value = c(mod.est$obs, 
    mod.est$mod.mean))

# Make histograms

ggplot(hist.dat, aes(x = value,  fill = Estimate))+
  facet_wrap(~ Estimate, scale = "free")+
  geom_histogram(aes(y=..density..), col = "black")+
  geom_density(alpha=.2)+
  ylab("Density")+
  xlab("Number of plant interactions")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


# Save the output
ggsave("./Figures/Globi_data_histogram_2020_12_04.pdf", width = 13, height = 12)


# End script