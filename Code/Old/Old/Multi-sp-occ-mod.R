########################################
########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



########################################
####### Code Objective #################
########################################


# The objective of the analysis is to determine the number of plants that each bee species interacts with while accounting for sampling bias

# We will be using a multi-species occupancy model for the analysis

# We will estimate:
  # psi = The probability a bee species interacts with a plant species
  # p = The probability that a study documented the bee-plant interaction


########################################
####### Code Output ####################
########################################



# This code generates the following files:
  # No files saved
  # 2 plots are created at the end to compare the truth & model estimates




########################################
######## Table of Contents #############
########################################


# 1. Set up libraries & wd
# 2. Load data
# 3. Summarize data 
# 4. Calculate derived quantities of interest
# 5. Bundle the data
# 6. MCMC settings
# 7. Run the models
# 8. Look at model outputs
# 9. Compare model outputs to truth


########################################
########################################
########################################





# 0. Load libraries & set working directory -------------------------------------------------------




# Load libraries
library(jagsUI)
library(reshape2)
library(ggplot2)
library(cowplot)


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 1. Set simulated data design ------------------------------------------------



# Upload the data
load("./Data/globi_data_formatted_bee_plant_2021_05_14.rds")
  # object name = bee.plant.cite2





# 2. Calculate derived quantities of interest ------------------------------------------------




# In this section, we will determine the observed number of plant species that each bee species interacts with


# First, we will determine if the bee-plant interaction occurs across any study
# We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(bee.plant.cite2, c(1, 2), max, na.rm = TRUE)


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Observed number of plant species that each bee interacts with
obs.dat <- data.frame(obs = y.bee.plant)

# Visualize the data
ggplot(data = obs.dat, aes(obs)) + geom_histogram()




# 5. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod.txt")
cat("
model{

# Priors
for(i in 1:n.bee){ # For each bee species

# We assign a prior from 0 to 1 for psi
# psi = The probability a bee species interacts with a plant species

  alpha_psi[i] ~ dnorm(mu.psi, tau.psi)

# The detection probability = the probability a study documented a bee-plant interaction
  alpha_p[i] ~ dnorm(mu.p, tau.p)

}

mu.psi ~ dnorm(0, 0.01)
mu.p ~ dnorm(0, 0.01)

tau.psi <- 1/(sd.psi * sd.psi)
sd.psi ~ dgamma(0.01, 0.01)

tau.p <- 1/(sd.p * sd.p)
sd.p ~ dgamma(0.01, 0.01)


# Model
  # Note the parrelels between the model code here and how we generated the data above

for(i in 1:n.bee){


  for(j in 1:n.plant){
  
    # True bee-plant interaction
    
    z[i, j] ~ dbern(psi[i, j])
 
   logit(psi[i, j]) <- alpha_psi[i]

      for(k in 1:n.study){

      # Observed bee-plant interaction
      
        y[i,j,k] ~ dbern(p.eff[i,j,k])

        p.eff[i,j,k] <- p[i,j,k] * z[i,j]
        
        logit(p[i,j,k]) <- alpha_p[i]
        
    }
    
  }

}



# Derived quantities
# Determine the total number of plants that each bee interacts with
  # All we do is sum across the 2nd dimension of the z matrix (which represents plant species)

for(i in 1:n.bee){

    # To determine the total number of plant interactions per bee species, we will sum across the plants
    z.bee.plant[i] <- sum(z[i, ])

  }


}
",fill=TRUE)
sink()





# 5. Bundle the data ------------------------------------------------




# Bundle all the data together in a list
jags.data <- list(
  n.bee = dim(bee.plant.cite2)[1],
  n.plant = dim(bee.plant.cite2)[2],
  n.study = dim(bee.plant.cite2)[3],
  y = bee.plant.cite2)


# Initial values - to give the model reasonable starting values for the parameters and latent states
zinit <- apply(jags.data$y, c(1, 2), max, na.rm = TRUE) 
zinit[zinit == "-Inf"] <- 0

inits <- function() {list(
  # Latent states
  z = zinit,
  
  # Parameters
  mu.psi = runif(1, -3 , 3),
  sd.psi = runif(1, 0 , 1),
  mu.p = runif(1, -3 , 3),
  sd.p = runif(1, 0 , 1)
  
)}


# List parameters to monitor
params <- c("alpha_psi", "mu.psi", "sd.psi",
            "alpha_p", "mu.p", "sd.p",
            "z.bee.plant")





# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 25000
na <- 20000
nb <- 5000
nt <- 5
nc <- 3





# 7. Run the models --------------------------------------------------------





# Run the model
# Takes ~ 0.09 minutes
out <- jags(data = jags.data, 
            inits = inits, 
            parameters.to.save = params, 
            model = "./Model/globi_mod.txt", 
            n.chains = nc, 
            n.thin = nt, 
            n.iter = ni, 
            n.burnin = nb, 
            n.adapt = na,
            parallel = TRUE)


# Beep when done
beepr::beep(4)

out$Rhat$alpha_p



# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Look at the traceplots
# These should look like caterpillars on the left
# And on the right it should be a nice smooth (not lumpy) distribution
plot(out)




# 9. Compare model outputs to truth ----------------------------------------






# Combine the names, truth, and model output
dat <- data.frame(names = c(paste(rownames(bee.plant.cite2), "interact prob"),
                            paste(rownames(bee.plant.cite2), "detect prob"),
                            rownames(bee.plant.cite2)), 
                  obs = c(rep(NA, times = nrow(bee.plant.cite2)),
                          rep(NA, times = nrow(bee.plant.cite2)),
                          y.bee.plant), 
                  mod.mean = c( out$mean$alpha_psi,
                                out$mean$alpha_p,
                                out$mean$z.bee.plant), 
                  mod.q2.5 = c(out$q2.5$alpha_psi,
                               out$q2.5$alpha_p,
                               out$q2.5$z.bee.plant), 
                  mod.q97.5 = c(out$q97.5$alpha_psi,
                                out$q97.5$alpha_p,
                                out$q97.5$z.bee.plant))


# Make detection probability the last entry
#dat$names <- factor(dat$names,
#                    levels = c(sort(rownames(bee.plant.cite2)),
#                               "Detection prob"))


## Make the plots

# Plot with probabilities
ggplot(dat[1:(jags.data$n.bee+1),], aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean)) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Parameter estimates') +
  xlab("Species names") +
  ggtitle("Parameter estimates")+
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
ggsave("./Figures/Model_params_2021_05_14.pdf", height = 15, width = 8)


# Object with colors
cols <- c("Observation" = "red", "Estimated" = "black")

# Plot with number of bee-plant interactions
ggplot(dat[c((jags.data$n.bee+2):(jags.data$n.bee * 2)),], 
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
ggsave("./Figures/Num_interacting_2021_05_14.pdf", height = 15, width = 8)


# End script
