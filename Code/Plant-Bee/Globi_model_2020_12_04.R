########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: grace.direnzo@gmail.com
########################################



##################################
######## Objective ###############
##################################


# To determine the number of plants that each bee genus interacts with while accounting for sampling bias


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






# 4. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod_count.txt")
cat("
model{

# Priors
for(i in 1:n.bee){

 # Average number of plant families each bee genus interacts with
  alpha.lambda[i] ~ dnorm(0, 0.01)

  # Probability of documenting interaction
 # p[i] ~ dnorm(mu.p, tau.p)

}

p ~ dnorm(0, 0.368)

#mu.lambda ~ dnorm(0, 0.01)
#tau.lambda <- 1 / (sd.lambda * sd.lambda)
#sd.lambda ~ dgamma(0.01, 0.01)

mu.p ~ dnorm(0, 0.01)
tau.p <- 1 / (sd.p * sd.p)
sd.p ~ dgamma(0.01, 0.01)


# Model
for(i in 1:n.bee){   # For each bee species


  # N is the true number of plant families that each bee genus interacts with
  # lambda is the average number of parasites across all bee species
  N[i] ~ dpois(lambda.eff[i])
  
  log(lambda.eff[i]) <- alpha.lambda[i]

  for(k in 1:n.study){
  
      # y = the number of plants that bee genus i is observed interacting with in each study k
      # p = the probability that each study detected a plant-bee interaction
      y[i,k] ~ dbinom(p.eff[i,k], N[i])
  
      logit(p.eff[i,k]) <- p

 }

}
    


}
",fill=TRUE)
sink()





# 5. Bundle the data ------------------------------------------------



# Remove crazy column names
colnames(globi.dat) <- 1:ncol(globi.dat)


# Convert globi data to matrix
globi.dat1 <- matrix(as.numeric(unlist(globi.dat[,-1])), 
                    nrow = nrow(globi.dat),
                    ncol = ncol(globi.dat)-1,
                    byrow = FALSE)


# Put all the data together
jags.data <- list(
  n.bee = dim(globi.dat1)[1],
  n.study = dim(globi.dat1)[2],
  y = globi.dat1)



# Initial values
Ninit <- apply(jags.data$y, 1, max, na.rm = TRUE)

inits <- function() {list(
  N = Ninit,
  
  mu.lambda = runif(1, min = -2, max = 2),
  mu.p = runif(1, min = -2, max = 2),
  
  sd.lambda = runif(1, min = 0, max = 1),
  sd.p = runif(1, min = 0, max = 1)
)}


# List parameters to monitor
params <- c("mu.lambda", 
            "sd.lambda",
            
            "mu.p",
            "sd.p",
            
            "alpha.lambda",
            "p",
            "N")




# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 100000
na <- 25000
nb <- 50000
nt <- 10
nc <- 3




# 7. Run the models --------------------------------------------------------





# Run the model
out <- jags(data = jags.data, 
            inits = inits, 
            parameters.to.save = params, 
            model = "./Model/globi_mod_count.txt", 
            n.chains = nc, 
            n.thin = nt, 
            n.iter = ni, 
            n.burnin = nb, 
            n.adapt = na,
            parallel = TRUE)


# Beep when done
beepr::beep(2)

plogis(out$mean$p)
plogis(out$q2.5$p)
plogis(out$q97.5$p)

# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Make sure all parameters have converged
plot(out)


# Save the output
save(out, file = "./ModelOutput/out_2020_12_04.rda")




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
ggsave("./Figures/Globi_data_count_2020_12_04.pdf", width = 10, height = 12)


# Plot the number of parasites per bee species
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