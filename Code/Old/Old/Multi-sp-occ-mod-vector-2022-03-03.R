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





# 1. Load libraries & set working directory -------------------------------------------------------




# Load libraries
library(rjags)
library(jagsUI)
library(reshape2)
library(ggplot2)
library(cowplot)


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 2. Read in data ------------------------------------------------






# Load the possible bee-plant-interactions
load("./Data/bee_plant_inter_2022_03_04 - short plant - no apis.rds")
  # object = bee.plant.inter
  # 2-D matrix




# Load the observed bee-plant-month by source citation interactions
  # object = bee.plant.obs
  # 2-D matrix
#load("./Data/bee_plant_obs_2022_02_18.rds")
bee.plant.obs <- read.csv("./Data/bee-plant-obs-long-format 2022 03 04 - short plant list - no apis.csv")



# Upload the data
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_03_04 - short plant list - no apis.rds")
  # object name = bee.plant.date.cite
    # 4-D array



# Load covariates
load("~/Dropbox/Globi/Data/model_covariates.rds")








# 3. Calculate derived quantities of interest ------------------------------------------------




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







# 4. Write the model in JAGS ------------------------------------------------





# A previous version of the model was written with nested for loops
# In an effort to speed up the model run - we will try working with a 2-D matrix instead



sink("./Model/globi_mod_vector_covariates.txt")
cat("
model{

# Priors
for(i in 1:n.bee){ # For each bee species

# Species-specific random effect for psi
# psi = The probability a bee species interacts with a plant species

  u[i] ~ dnorm(mu.psi, tau.psi)

# Species-specific random effect for p
# p = The detection probability = the probability a study documented a bee-plant interaction

  v[i] ~ dnorm(mu.p, tau.p)

}


# Mean bee-plant interaction probability
mu.psi ~ dnorm(0, 0.01)

# Precision and sd values for psi
tau.psi <- 1/(sigma.psi * sigma.psi)
sigma.psi ~ dgamma(0.01, 0.01)


# Mean bee-plant detection probability
  # This is the prior that worked in the null model: mu.p ~ dnorm(0, 0.75)
mu.p ~ dnorm(0, 0.75)

# Precision and sd values for p
  # This is the prior that worked in the null model: sigma.p ~ dgamma(1, 1)
tau.p <- 1/(sigma.p * sigma.p)
sigma.p ~ dgamma(1, 1)


# Priors for covariates
beta_psi_size ~ dnorm(0, 0.368)
beta_psi_sociality ~ dnorm(0, 0.368)
beta_p_stripped ~ dnorm(0, 0.368)
beta_p_source ~ dnorm(0, 0.368)



# Ecological model
  # For each possible bee-plant interaction when they are interacting *using the bee.plant.inter matrix
for(i in 1:n.row.true){

    # True bee-plant interaction during month t
    
    z[bee.ID.true[i], plant.ID.true[i], month.ID.true[i]] ~ dbern(psi[bee.ID.true[i], plant.ID.true[i], month.ID.true[i]])
 
      logit(psi[bee.ID.true[i], plant.ID.true[i], month.ID.true[i]]) <- u[bee.ID.true[i]] +
                                                                        # Bee size
                                                                        beta_psi_size * size[bee.ID.true[i]] + 
                                                                        # Bee sociality
                                                                        beta_psi_sociality * sociality[bee.ID.true[i]]
                                                                        # flower color
  
}


# Observation model
  # For each bee-plant interaction during the months that each source citation was in the field
  # We don't want to penalize (or assign a non-detection) for months that the source citation was NOT in the field
for(i in 1:n.row.obs){

        # Observed bee-plant interaction by month and by source citation
        
          y[i] ~ dbern(p.eff[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]])

          p.eff[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]] <- 
          p[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]] * 
          z[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i]]
          
          logit(p[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]]) <- v[bee.ID.obs[i]] +
                                                                                           # Strippiness
                                                                                           beta_p_stripped * stripped[bee.ID.obs[i]] +
                                                                                           # source
                                                                                           beta_p_source * citation.code[citation.ID.obs[i]]
                                                                                           # bee sociality or size

}


}
",fill=TRUE)
sink()





# 4. Bundle the data ------------------------------------------------



# Standardize size data & replace NAs
size_std <- (covariates$bee.covariates$size - mean(covariates$bee.covariates$size, na.rm = TRUE))/sd (covariates$bee.covariates$size, na.rm = TRUE)
size_std <- ifelse(is.na(size_std) == TRUE, 0, size_std)



# Bundle all the data together in a list
jags.data <- list(
  
  # Number of bee species
  n.bee = max(bee.plant.inter$beeID),

  # Number of plant species
  n.plant = max(bee.plant.inter$plantID),
  
  # Number of citations
  n.citations = max(bee.plant.obs$sourceID),

  # Number of true bee-plant interactions by month
  n.row.true = nrow(bee.plant.inter),
  
  # True bee-plant interactions by month for Ecological model
  bee.ID.true   = bee.plant.inter$beeID,
  plant.ID.true = bee.plant.inter$plantID,
  month.ID.true = bee.plant.inter$monthID,
  
  # Number of possible observable bee-plant interactions by month & citation 
  n.row.obs = nrow(bee.plant.obs),
  
  # Observed bee-plant interactions by month & citation for Observation model
  bee.ID.obs = bee.plant.obs$beeID,
  plant.ID.obs = bee.plant.obs$plantID,
  month.ID.obs = bee.plant.obs$monthID,
  citation.ID.obs = bee.plant.obs$sourceID,
  
  # Observed data
  y = bee.plant.obs$y,
  
  # Covariates
  stripped = covariates$bee.covariates$striped,
  size = size_std,
  sociality = covariates$bee.covariates$solitary,
  citation.code = covariates$citation.covariates$citation.code

  )





# Look at the data structure
str(jags.data)


# Initial values - to give the model reasonable starting values for the parameters and latent states
  # collapse observations across 4th dimension (souce citations)
#zinit <- apply(jags.data$y, c(1, 2, 3), max, na.rm = TRUE) 
#zinit[zinit == "-Inf"] <- NA

zinit <- apply(bee.plant.date.cite, c(1, 2, 3), max, na.rm = TRUE) 
zinit[zinit == "-Inf"] <- NA

dim(zinit)

inits <- function() {list(
  # Latent states
   z = zinit,
  
  # Parameters
  # Occupancy
  mu.psi = runif(1, -10 , -1),
  sigma.psi = runif(1, 0 , 1),
  beta_psi_size = runif(1, -3 , -3),
  beta_psi_sociality = runif(1, -3 , -3),
  
  # Detection
  mu.p = runif(1, -10 , -1),
  sigma.p = runif(1, 0 , 1),
  beta_p_stripped = runif(1, -3 , -3),
  beta_p_source = runif(1, -3, 3)
  
)}


  

# List parameters to monitor
params <- c( "mu.psi", "sigma.psi",
             "mu.p", "sigma.p",
             "beta_psi_size",
             "beta_psi_sociality",
             "beta_p_stripped",
             "beta_p_source"
             #"u",
             #"v",
             #"z"
             )





# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 50000
na <- 20000
nb <- 10000
nt <- 10
nc <- 3


# With 126 plant species - with size & strippiness as covariates
  # 2 iterations = 58.8356 secs
  # 200 iterations (100 na, 10 nb, 1 nt) = 1.556847 minutes
  # 2000 iterations (1000 na, 100 nb, 10 nt)= 6.893629 minutes
  # 20000 iterations (10000 na, 1000 nb, 10 nt)=  1.021767 hours
  # 50000 iterations (20000 na, 10000 nb, 10 nt)=  2.452664 hours



# With 126 plant species - with size, socilaity, source, & strippiness as covariates
  # 5 iterations = 1.065841 mins
  # 200 iterations (100 na, 10 nb, 1 nt) = 
  # 2000 iterations (1000 na, 100 nb, 10 nt)= 
  # 20000 iterations (10000 na, 1000 nb, 10 nt)=  
  # 50000 iterations (20000 na, 10000 nb, 10 nt)= 4.311324 hours





# 7. Run the models --------------------------------------------------------





# Run the model
start.time <- Sys.time()

out <- jags(data = jags.data, 
            inits = inits, 
            parameters.to.save = params, 
            model = "./Model/globi_mod_vector_covariates.txt", 
            n.chains = nc, 
            n.thin = nt, 
            n.iter = ni, 
            n.burnin = nb, 
            n.adapt = na,
            parallel = TRUE)

end.time <- Sys.time()
beepr::beep(2)

# How long did the model take?
end.time - start.time


# temp - code when node conflict between z and y


#jags.data$y[71199]
#bee.plant.obs[71199,]
#
#zinit[bee.plant.obs$beeID[71199], 
#      bee.plant.obs$plantID[71199],
#      bee.plant.obs$monthID[71199]]
#

#zinit[2, 31, 1]
#
#
#bee.plant.obs[bee.plant.obs$beeID == 2 &  
#              bee.plant.obs$plantID == 31 , ]
#
#
#bee.plant.inter[bee.plant.inter$beeID == 2 &  
#                bee.plant.inter$plantID == 31 , ]
#
#


# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Look at the traceplots
# These should look like caterpillars on the left
# And on the right it should be a nice smooth (not lumpy) distribution
plot(out)


# Save the model output
save(out, file = "./ModelOutput/globi-short plant list-no apis- 2022 03 04 - size - stripes.rds")



# End script
