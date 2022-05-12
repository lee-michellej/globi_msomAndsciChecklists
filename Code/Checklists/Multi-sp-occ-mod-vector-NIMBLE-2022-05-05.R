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
library(reshape2)
library(ggplot2)
library(cowplot)
library(nimble)
library(doParallel) 
library(MCMCvis)
library(mcmcOutput)
library(coda)




# Set working directory
setwd("~/Github/globi_tritrophic_networks/")





# 2. Load data ------------------------------------------------




################
# Determine if the run will be WITH or WITHOUT Apis mellifera
################






######### WITH Apis melifera



# # Upload the data
# load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_05 - short plant list.rds")
# # object name = bee.plant.date.cite
# # 4-D array
# 



######### WITHOUT Apis melifera


# Upload the data
  # object name = bee.plant.date.cite
  # 4-D array
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_11 - short plant list - no apis.rds")



################
################
################






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


# ggsave(file = "./Figures/2022_04_05/Observations - with Apis.pdf", 
#        height = 6,
#        width = 8)







# 4. Write function with everything needed ------------------------------------------------




# Create a function with all the needed code
run_MCMC_allcode <- function(seed){
  
  # Load the library
  library(nimble)
  
  
  ################
  # Determine if the run will be WITH or WITHOUT Apis mellifera
  ################
  
  
  
  
  
  
  ######### WITH Apis melifera
  
#  # List with Apis mellifera: bee_plant_inter_2022_02_28 - short plant
#  load("./Data/bee_plant_inter_2022_04_05 - short plant.rds")
#  # object = bee.plant.inter
#  # 2-D matrix
#  
#  # Load the observed bee-plant-month by source citation interactions
#  bee.plant.obs <- read.csv( "./Data/bee-plant-obs-long-format 2022 04 05 - short plant list.csv")
#  # object = bee.plant.obs
#  # 2-D matrix
#  
#  
#  # Upload the data
#  load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_05 - short plant list#.rds")
#  # object name = bee.plant.date.cite
#  # 4-D array
#  
#  
#  # Load covariates
#  load("~/Dropbox/Globi/Data/model_covariates - 2022 04 05.rds")
#  
  
  
  
  
  
  ######### WITHOUT Apis melifera
  
  
  
  
 # Load the possible bee-plant-interactions
 # object = bee.plant.inter
 # 2-D matrix
 load("./Data/bee_plant_inter_2022_04_11 - short plant - no apis.rds")
 
 
 # Load the observed bee-plant-month by source citation interactions
 # object = bee.plant.obs
 # 2-D matrix
  bee.plant.obs <- read.csv( "./Data/bee-plant-obs-long-format 2022 04 11 - short plant list - no apis.csv")

  # Upload the data
  # object name = bee.plant.date.cite
  # 4-D array
  load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2022_04_11 - short plant list - no apis.rds")
 
 
  # Load covariates
  load("~/Dropbox/Globi/Data/model_covariates - 2022 04 21 - no apis.rds")
 
 
  
  ################
  ################
  ################
  
  
  
  
  
  # Write the model
  MEcode <- nimbleCode({
    
    # Priors
    for(i in 1:n.bee){ # For each bee species
      
      # Species-specific random effect for psi
      # psi = The probability a bee species interacts with a plant species 
        # The bee-plant interaction probability is the SAME for all months that they interact
      u[i] ~ dnorm(mu.psi, tau.psi)
      
      # Species-specific random effect for p
      # p = The detection probability = the probability a source citation documented a bee-plant interaction
        # Detection probabilities are the same across all plants
      v[i] ~ dnorm(mu.p, tau.p)
      
    }
    
    
    # Mean bee-plant interaction probability
    mu.psi ~ dnorm(-0.25, 2.0)
      # dnorm(-0.25, 2.0)
      # dnorm(0, 2.5)
      # dnorm(0, 2)
      # dnorm(0, 0.75)
      # dnorm(0, 0.10)
    
    # Precision and sd values for psi
    tau.psi <- 1/(sigma.psi * sigma.psi)
    sigma.psi ~ T(dnorm(3.75, sd = sqrt(1/2)), 0, 10)
      # dnorm(3.75, 2)T(0,)
      # dgamma(1, 1)
      # dgamma(0.1, 0.1)
      # x ∼ T(dnorm(0, sd = 10), 0, a),
    
    
    # Mean bee-plant detection probability
    # This is the prior that worked in the null model: mu.p ~ dnorm(0, 0.75)
    mu.p ~ dnorm(-2, 2.0)
      # dnorm(-2, 2.0)
      # dnorm(0, 2.5)
      # dnorm(0, 2)
      # dnorm(0, 0.10)
      # dnorm(0, 0.75)
    
    # Precision and sd values for p
    # This is the prior that worked in the null model: sigma.p ~ dgamma(1, 1)
    tau.p <- 1/(sigma.p * sigma.p)
    sigma.p ~ T(dnorm(2.0, sd = sqrt(1/2)), 0, 10)
      # dnorm(2, 2)T(0,)
      # dgamma(1, 1)
      # dgamma(0.1, 0.1)
    
    
    # Priors for covariates
    
    # Psi
    sd_psi ~ T(dnorm(1.0, sd = sqrt(1/2)), 0, 10)
      # dnorm(1, 2)T(0,)
      # dgamma(1, 1)
      # dgamma(0.1, 0.1)
    tau_in_psi <- pow(sd_psi, -2)
    tau_psi[1] <- tau_in_psi            # coef effectively zero
    tau_psi[2] <- tau_in_psi / 1000     # nonzero coef
    p_ind_psi[1] <- 1/2
    p_ind_psi[2] <- 1 - p_ind_psi[1]
    
    for (j in 1:ncov_psi){
      
      indA_psi[j] ~ dcat(p_ind_psi[1:2]) # returns 1 or 2
      gamma_psi[j] <- indA_psi[j] - 1    # returns 0 or 1
      beta_psi[j] ~ dnorm(0, tau_psi[indA_psi[j]])
    
    }
    
    
    # P
    sd_p ~ T(dnorm(0.10, sd = sqrt(1/2)), 0, 10)
      # dnorm(0.1, 2)T(0,)
      # dgamma(1, 1)
      # dgamma(0.1, 0.1)
    tau_in_p <- pow(sd_p, -2)
    tau_p[1] <- tau_in_p            # coef effectively zero
    tau_p[2] <- tau_in_p / 1000     # nonzero coef
    p_ind_p[1] <- 1/2
    p_ind_p[2] <- 1 - p_ind_p[1]
    
    for (j in 1:ncov_p){
      
      indA_p[j] ~ dcat(p_ind_p[1:2]) # returns 1 or 2
      gamma_p[j] <- indA_p[j] - 1    # returns 0 or 1
      beta_p[j] ~ dnorm(0, tau_p[indA_p[j]])
      
    }
    
    
    # Ecological model
    # For each possible bee-plant interaction when they are interacting using the bee.plant.inter matrix
    for(i in 1:n.row.true){
      
      # True bee-plant interaction during month t
      z[bee.ID.true[i], plant.ID.true[i], month.ID.true[i]] ~ dbern(psi[bee.ID.true[i], plant.ID.true[i], month.ID.true[i]])
      
      # Make the bee-plant interaction probability of function of variables
      logit(psi[bee.ID.true[i], plant.ID.true[i], month.ID.true[i]]) <- # Bee species-specific random effect/intercept
                                                                        u[bee.ID.true[i]] +
                                                                        # Bee size
                                                                        beta_psi[1] * size[bee.ID.true[i]]+ 
                                                                        # Bee sociality
                                                                        beta_psi[2] * sociality[bee.ID.true[i]]+
                                                                        # Flower color
                                                                        beta_psi[3] * flower_color[plant.ID.true[i]]+
                                                                        # Flower shape
                                                                        beta_psi[4] * flower_shape[plant.ID.true[i]]
                      
                                                                        
                                                                        
    }
    
    
    
    
    
    # Observation model
    # For each bee-plant interaction during the months that each source citation was in the field
    # We don't want to penalize (or assign a non-detection) for months that the source citation was NOT in the field
    for(i in 1:n.row.obs){
      
      # Observed bee-plant interaction by month and by source citation
      
      y[i] ~ dbern(p.eff[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]])
      
      p.eff[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]] <- p[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]] * 
                                                                                    z[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i]]
      
      logit(p[bee.ID.obs[i], plant.ID.obs[i], month.ID.obs[i], citation.ID.obs[i]]) <- # Bee species-specific random effect/intercept
                                                                                        v[bee.ID.obs[i]] +
                                                                                        # Bee Strippiness
                                                                                        beta_p[1] * stripped[bee.ID.obs[i]]+
                                                                                        # Bee size
                                                                                        beta_p[2] * size[bee.ID.obs[i]]+
                                                                                        # Quadratic term for month
                                                                                        beta_p[3] * month.ID.obs[i]+
                                                                                        beta_p[4] * pow(month.ID.obs[i], 2)+
                                                                                        # source
                                                                                        beta_p[5] * citation.code[citation.ID.obs[i]]+
                                                                                        # Flower color
                                                                                        beta_p[6] * flower_color[plant.ID.obs[i]]+
                                                                                        # Flower shape
                                                                                        beta_p[7] * flower_shape[plant.ID.obs[i]]+
                                                                                        # Plant family
                                                                                        beta_p[8] * plant_family[plant.ID.obs[i]]
      
    }
    
  })
  
  
  
  # Bundle all the values that remain constant in the model
  MEconsts <- list(
    
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
    
    # Covariates
    stripped      = covariates$bee.covariates$striped,
    size          = covariates$bee.covariates$size_std,
    sociality     = covariates$bee.covariates$solitary,
    citation.code = covariates$citation.covariates$citation.code,
    flower_color  = covariates$plant.covariates$yellow,
    flower_shape  = covariates$plant.covariates$bowl,
    plant_family  = covariates$plant.covariates$aster,
    
    # Number of covariates
    ncov_psi = 4,
    ncov_p = 8
    
  )
  
  
  
  
  # List the data
  MEdata <- list(y = bee.plant.obs$y)
  
  
  # Initial values for the z array - latent state variable
  zinit <- apply(bee.plant.date.cite, c(1, 2, 3), max, na.rm = TRUE) 
  zinit[zinit == "-Inf"] <- NA
  
  
  # Bundle the initial values
  MEinits <- function() {list(
    # Latent states
    z = zinit,
    
    # Parameters
    # Occupancy
    mu.psi = runif(1, -10 , -1),
    sigma.psi = runif(1, 0 , 1),
    beta_psi = runif(MEconsts$ncov_psi, -3 , -3),
    
    # Detection
    mu.p = runif(1, -10 , -1),
    sigma.p = runif(1, 0 , 1),
    beta_p = runif(MEconsts$ncov_p, -3 , -3)
    
  )}
  
  
  
  
  # List parameters to monitor
  MEmons <- c( "mu.psi", "sigma.psi",
               "mu.p", "sigma.p",
               
               # Psi covariates
               "gamma_psi",
               "beta_psi",
               "sd_psi",
               
               # P covariates
               "gamma_p",
               "beta_p",
               "sd_p"
  )
  
  MElatent <- c("u",
                "v",
                "z")
  
  
  # Start creating/compiling the nimble model
  MEmodel <- nimbleModel(MEcode, 
                         MEconsts, 
                         MEdata, 
                         MEinits(), 
                         calculate = F, 
                         check = F)
  
  # Compile
  cMEmodel <- compileNimble(MEmodel)
  
  # MCMC
  MEconf <- configureMCMC(MEmodel, 
                          monitors = MEmons,
                          monitors2 = MElatent
  )
  
  # Build
  MEmcmc <- buildMCMC(MEconf)
  
  # Compile
  cMEmcmc <- compileNimble(MEmcmc, 
                           project = cMEmodel, 
                           resetFunctions = T)
  
  ## Run MCMC
  results <-  runMCMC(cMEmcmc, 
                      nchains = 1, 
                      niter = 150000, 
                      nburnin = 50000, 
                      thin = 10, 
                      thin2 = 10,
                      setSeed = seed)
  
  
 ## Run MCMC
 #results <-  runMCMC(cMEmcmc, 
 #                    nchains = 1, 
 #                    niter = 5, 
 #                    nburnin = 2, 
 #                    thin = 1, 
 #                    thin2 = 1,
 #                    setSeed = seed)
 #
  # Return MCMC results
  return(results)
  
  
}





# 7. Run the models --------------------------------------------------------





start.time <- Sys.time()

# Number of cores available on this machine
detectCores()  

# Number to cores to use
ncore <- 3     

# Create the cluster
cl <- makeCluster(ncore)

registerDoParallel(cl)

# set seeds
seeds <- 1:ncore


# Run the model using dopar 
start.time <- Sys.time()

result <- foreach(x = seeds, 
                  .packages="nimble") %dopar% {
  
                    # function
                    run_MCMC_allcode(seed = seeds[x])
                    
                  }

stopCluster(cl)

end.time <- Sys.time()

beepr::beep(2)

# How long did the model take?
end.time - start.time
    





# 8. Look at model outputs -------------------------------------------------





# # Row bind all of the chains together - coefficents
out <- as.mcmc(data.frame(rbind(result[[1]]$samples,
                                 result[[2]]$samples,
                                 result[[3]]$samples)))

simp_list <- list()

simp_list[[1]] <- as.mcmc(result[[1]]$samples)
simp_list[[2]] <- as.mcmc(result[[2]]$samples)
simp_list[[3]] <- as.mcmc(result[[3]]$samples)

MCMClist <- mcmc.list(simp_list)

MCMCsummary(MCMClist)


# Traceplots
ggs_BYMeco <- ggs(MCMClist) 

# Sd and mu parameters
ggs_BYMeco %>% filter(Parameter %in% c( "mu.psi", "sigma.psi",
                                        "mu.p", "sigma.p",
                                        "sd_psi", "sd_p")) %>% 
  ggs_traceplot() + theme_bw()

# Beta_psi
ggs_BYMeco %>% filter(Parameter %in% c( paste("beta_psi[", 1:4, "]", sep = ""))) %>% 
  ggs_traceplot() + theme_bw()

# Beta_p
ggs_BYMeco %>% filter(Parameter %in% c( paste("beta_p[", 1:8, "]", sep = ""))) %>% 
  ggs_traceplot() + theme_bw()





# Save the model output
save(out, 
     file = "~/Dropbox/Globi/ModelOutput/globi-short plant list- 2022 05 06 - all cov - NO apis - NIMBLE - IVS.rds")

save(result, 
     file = "~/Dropbox/Globi/ModelOutput/OUTPUT - globi-short plant list- 2022 05 06 - all cov - NO apis - NIMBLE - IVS.rds")

save(MCMClist,
     file = "~/Dropbox/Globi/ModelOutput/MCMClist- globi-short plant list- 2022 05 06 - all cov - NO apis - NIMBLE - IVS.rds")


# End script
