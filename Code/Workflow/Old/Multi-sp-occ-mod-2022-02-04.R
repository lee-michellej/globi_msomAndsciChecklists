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
library(rjags)
library(jagsUI)
library(reshape2)
library(ggplot2)
library(cowplot)


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 1. Set simulated data design ------------------------------------------------



# Upload the data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_02_01.rds")
  # object name = bee.plant.date.cite
    # 4-D array


# Load the possible bee-plant-interactions
load("./Data/bee_plant_inter_2022_02_01.rds")
  # object = bee.plant.inter
  # 2-D matrix

# Load the observed bee-plant-month by source citation interactions
  # object = bee.plant.obs
  # 2-D matrix
load("./Data/bee_plant_obs_2022_02_01.rds")




# 2. Calculate derived quantities of interest ------------------------------------------------




# In this section, we will determine the observed number of plant species that each bee species interacts with


# First, we will determine if the bee-plant interaction occurs across any study
# We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(bee.plant.date.cite, c(1, 2), max, na.rm = TRUE)
y.bee.plant[y.bee.plant == "-Inf"] <- 0


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Observed number of plant species that each bee interacts with
obs.dat <- data.frame(obs = y.bee.plant)

# Visualize the data
ggplot(data = obs.dat, aes(obs)) + 
  geom_histogram()+
  xlab("Number of plant interactions per bee species")+
  ylab("Total counts")+
  theme_bw()




# 3. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod.txt")
cat("
model{

# Priors
for(i in 1:n.bee){ # For each bee species

# psi = The probability a bee species interacts with a plant species

  u[i] ~ dnorm(mu.psi, tau.psi)

# p = The detection probability = the probability a study documented a bee-plant interaction

  v[i] ~ dnorm(mu.p, tau.p)

}

mu.psi ~ dnorm(0, 0.01)
mu.p ~ dnorm(0, 0.01)

tau.psi <- 1/(sigma.psi * sigma.psi)
sigma.psi ~ dgamma(0.01, 0.01)

tau.p <- 1/(sigma.p * sigma.p)
sigma.p ~ dgamma(0.01, 0.01)


# Ecological model
  # For each possible bee-plant interaction when they are interacting *using the bee.plant.inter matrix
for(i in 1:n.row.inter){

    # True bee-plant interaction during month t
    
    z[n.bee.inter[i], n.plant.inter[i], n.month.inter[i]] ~ dbern(psi[n.bee.inter[i], n.plant.inter[i], n.month.inter[i]])
 
      logit(psi[n.bee.inter[i], n.plant.inter[i], n.month.inter[i]]) <- u[n.bee.inter[i]]
  
}


# Observation model
  # For each bee-plant interaction during the months that each source citation was in the field
  # We don't want to penalize (or assign a non-detection) for months that the source citation was NOT in the field
for(i in 1:n.row.obs){

        # Observed bee-plant interaction by month and by source citation
        
          y[n.bee.obs[i], n.plant.obs[i], n.month.obs[i], n.citation.obs[i]] ~ dbern(p.eff[n.bee.obs[i], n.plant.obs[i], n.month.obs[i], n.citation.obs[i]])

          p.eff[n.bee.obs[i], n.plant.obs[i], n.month.obs[i], n.citation.obs[i]] <- 
          p[n.bee.obs[i], n.plant.obs[i], n.month.obs[i], n.citation.obs[i]] * 
          z[n.bee.obs[i], n.plant.obs[i], n.month.obs[i]]
          
          logit(p[n.bee.obs[i], n.plant.obs[i], n.month.obs[i], n.citation.obs[i]]) <- v[n.bee.obs[i]]

}



# Derived quantities
# Determine the total number of plants that each bee interacts with
  # All we do is sum across the 2nd dimension of the z matrix (which represents plant species)

for(i in n.bee.inter){

  for(t in n.month.inter){
  
    # To determine the total number of plant interactions per bee species per month, we will sum across the plants
      # outside of the model - we will collapse across months
    z.bee.plant.month[i, t] <- sum(z[i, , t])
    
  }
  
}


}
",fill=TRUE)
sink()





# 4. Bundle the data ------------------------------------------------




# Subset to the first 100 plants
bee.plant.inter2 <- bee.plant.inter[bee.plant.inter$plantID < 101,]
bee.plant.obs2 <- bee.plant.obs[bee.plant.obs$plantID < 101,]
bee.plant.date.cite2 <- bee.plant.date.cite[, 1:100, , ]


# Bundle all the data together in a list
jags.data <- list(
  
  # Number of bee species
  n.bee = dim(bee.plant.date.cite)[1],
  
 # Number of true bee-plant interactions by month
 n.row.inter = nrow(bee.plant.inter2),
 
 # True bee-plant interactions by month for Ecological model
     n.bee.inter   = bee.plant.inter2$beeID,
     n.plant.inter = bee.plant.inter2$plantID,
     n.month.inter = bee.plant.inter2$monthID,
     
   # Number of possible observations
   n.row.obs = nrow(bee.plant.obs2), 
   
   # Observed 
     n.bee.obs =      bee.plant.obs2$beeID,
     n.plant.obs =    bee.plant.obs2$plantID,
     n.month.obs =    bee.plant.obs2$monthID,
     n.citation.obs = bee.plant.obs2$sourceID,

  # Observation data
  y = bee.plant.date.cite2)

# Look at the data structure
str(jags.data)


# Initial values - to give the model reasonable starting values for the parameters and latent states
  # collapse observations across 4th dimension (souce citations)
zinit <- apply(jags.data$y, c(1, 2, 3), max, na.rm = TRUE) 
zinit[zinit == "-Inf"] <- NA

inits <- function() {list(
  # Latent states
  z = zinit,
  
  # Parameters
  # Occupancy
  mu.psi = runif(1, -3 , 3),
  sigma.psi = runif(1, 0 , 1),
  
  # Detection
  mu.p = runif(1, -3 , 3),
  sigma.p = runif(1, 0 , 1)
  
)}


  

# List parameters to monitor
params <- c( "mu.psi", "sigma.psi",
             "mu.p", "sigma.p",
             "u",
             "v",
            "z.bee.plant.month")





# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 100
na <- 50
nb <- 20
nt <- 1
nc <- 3


# Will 578 plant species
# 2 iterations = 5 minutes
# 100 iterations = 100 * 5/2 = 250 minutes


# With 100 plant species
# 2 iterations = 0.37 sec
# 100 iterations = 100 * 0.37/2 * 1/60 =  minutes



# 7. Run the models --------------------------------------------------------





# Run the model
start.time <- Sys.time()

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

end.time <- Sys.time()
beepr::beep(2)

# How long did the loop take?
end.time - start.time




# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Look at the traceplots
# These should look like caterpillars on the left
# And on the right it should be a nice smooth (not lumpy) distribution
plot(out)





# Summarize the z output

out$sims.list$z.bee.plant.month






# 9. Compare model outputs to truth ----------------------------------------






# Combine the names, truth, and model output
dat <- data.frame(names = c(paste(rownames(bee.plant.date.cite), "interact prob"),
                            paste(rownames(bee.plant.date.cite), "detect prob")), 
                  obs = c(rep(NA, times = nrow(bee.plant.date.cite)),
                          rep(NA, times = nrow(bee.plant.date.cite))), 
                  mod.mean = c( out$mean$u,
                                out$mean$v), 
                  mod.q2.5 = c(out$q2.5$u,
                               out$q2.5$v), 
                  mod.q97.5 = c(out$q97.5$u,
                                out$q97.5$v))


## Make the plots

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
