########################################
########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



########################################
####### Code Objective #################
########################################


# The objective of this code:
  # to write 6 different variations of the model:
    # No bee/plant specification
    # bee-specific intercepts
    # plant-specific intercepts
    # bee family intercepts
    # plant family intercepts
    # bee and plant family intercepts


########################################
####### Code Output ####################
########################################


# Functions to be used in another script


########################################
######## Table of Contents #############
########################################


# 1. No bee/plant specification
# 2. bee-specific intercepts
# 3. plant-specific intercepts
# 4. bee family intercepts
# 5. plant family intercepts
# 6. bee and plant family intercepts

########################################
########################################
########################################





# 1. No bee/plant specification -------------------------------------------------------



# Create a function with all the needed code
no_bee_plant_specification <- function(seed, 
                                       n.iter, 
                                       n.burn,
                                       n.thin1, 
                                       n.thin2){
  
  # Load the library
  library(nimble)
  library(reshape2)
  
  
  # Upload the data
  # object name = bee.plant.cite
  # 3-D array
  load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2024_07_11 - short plant list - no apis.rds")
  
  
  # Load covariates
  load("./Data/model_covariates - 2025 01 07 - no apis.rds")
  
  
  # Flatten the array
  dat_long <- melt(bee.plant.cite) 
  colnames(dat_long) <- c("bee_ID", "plant_ID", "cite_ID", "observation")
  
  
  # Write the model
  MEcode <- nimbleCode({
    
    # Priors for covariates
    
    for (j in 1:ncov_psi){
      
      beta_psi[j] ~ dnorm(0, sd = sqrt(1/0.368))
      
    }
    
    for (j in 1:ncov_p){
      
      beta_p[j] ~ dnorm(0, sd = sqrt(1/0.368))
      
    }
    
    
    # Ecological model
    for(i in 1:n_bee_plant){
      
      # True bee-plant interaction
      z[bee_ID[i], plant_ID[i]] ~ dbern(psi[bee_ID[i], plant_ID[i]])
      
      # Make the bee-plant interaction probability of function of variables
      logit(psi[bee_ID[i], plant_ID[i]]) <- 
        
        # Intercept
          # Average size bee
          # Not solitary bee
          # Other flower color
          # Not bowl
        beta_psi[1] +
        
        # Bee size
        beta_psi[2] * size[bee_ID[i]]+ 
        
        # Bee solitary (1 = yes; 0 = no)
        beta_psi[3] * solitary[bee_ID[i]]+ 
        
        # Flower color
        # Different bee genus have different probabilities of interacting with different plant colors
        beta_psi[4] * yellow_flower_color[plant_ID[i]]+
        beta_psi[5] * blue_flower_color[plant_ID[i]]+
        beta_psi[6] * white_flower_color[plant_ID[i]]+
        
        # Flower shape (== bowl)
        beta_psi[7] * flower_shape[plant_ID[i]]
      
    }
    
    # Observation model
    for(i in 1:n_bee_plant_cite){
      
      # Observed bee-plant interaction by source citation
      y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
      
      p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
      
      logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <-  
        
        # Intercept
          # Not striped
          # Averge sized bee (= 0)
          # Aggregated data type
          # Flower color = other
          # Flower shape = no bowl
        beta_p[1] +
        
        # Bee Strippiness (1 = yes stripped; 0 = no stripped)
        beta_p[2] * striped[bee_ID[i]]+
        
        # Bee size
        beta_p[3] * size[bee_ID[i]]+
        
        # sourceCitation type
        # Literature (1 = yes; 0 = no)
        beta_p[4] * citation_lit[cite_ID[i]]+
        # Collection (1 = yes; 0 = no)
        beta_p[5] * citation_col[cite_ID[i]]+
        # Observation (1 = yes; 0 = no)
        beta_p[6] * citation_obs[cite_ID[i]]+
        
        # Flower color (1 = yes yellow; 0 = no yellow)
        beta_p[7] * yellow_flower_color[plant_ID[i]]+
        # blue
        beta_p[8] * blue_flower_color[plant_ID[i]]+
        # white
        beta_p[9] * white_flower_color[plant_ID[i]]+            
        
        # Flower shape (1 = yes bowl; 0 = no bowl)
        beta_p[10] * flower_shape[plant_ID[i]]
      
      
      
      # Create simulated dataset to calculate the Bayesian p-value
      y.sim[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
      
      d[i]<-  abs(y[i] - p.eff[bee_ID[i], plant_ID[i], cite_ID[i]]) 
      
      dnew[i]<- abs(y.sim[i] - p.eff[bee_ID[i], plant_ID[i], cite_ID[i]]) 
      
      d2[i]<- pow(d[i], 2)  
      
      dnew2[i]<- pow(dnew[i], 2) 
      
    }   
    
    # Calculate the discrepancy measure, defined as the mean(p.fit > p.fitnew) 
    p.fit <- sum(d2[1:n_bee_plant_cite]) 
    p.fitnew <- sum(dnew2[1:n_bee_plant_cite])
    
    p.diff <- nimStep(p.fit - p.fitnew)
    # step function at 0 = function returns 0 if 𝑥 < 0, 1 if 𝑥 >= 0
    
    # Calculate the total number of plants that each bee interacts with
    for(i in 1:n_bee){
      
      n_plants_per_bee[i] <- sum(z[i,])
      
    }
    
  })
  
  
  # Bundle all the values that remain constant in the model
  MEconsts <- list(
    
    # Total number of bees
    n_bee = dim(bee.plant.cite)[1],
    
    # bee * plant total
    n_bee_plant  = dim(bee.plant.cite)[1] * dim(bee.plant.cite)[2],
    
    n_bee_plant_cite  = dim(bee.plant.cite)[1] * dim(bee.plant.cite)[2] * dim(bee.plant.cite)[3],
    
    # Bee Covariates
    striped      = covariates$bee.covariates$striped,
    size          = covariates$bee.covariates$size_std,
    solitary      = covariates$bee.covariates$solitary,
    
    # sourceCitation covariates
    citation_lit = covariates$citation.covariates$literature, 
    citation_col = covariates$citation.covariates$collection, 
    citation_obs = covariates$citation.covariates$observation, 
    
    # Flower covariates - color
    yellow_flower_color  = covariates$plant.covariates$yellow,
    blue_flower_color  = covariates$plant.covariates$blue.new,
    white_flower_color  = covariates$plant.covariates$white,
    
    # Flower covariates - shape
    flower_shape  = covariates$plant.covariates$bowl,
    
    # Number of covariates
    ncov_psi = 7,
    ncov_p = 10,
    
    # ID index
    bee_ID = dat_long$bee_ID,
    plant_ID = dat_long$plant_ID,
    cite_ID = dat_long$cite_ID
    
  )
  
  
  # List the data
  MEdata <- list(y = dat_long$observation)
  
  
  # Initial values for the z array - latent state variable
  zinit <- apply(bee.plant.cite, c(1, 2), max, na.rm = TRUE) 
  zinit[zinit == "-Inf"] <- NA
  
  
  # Bundle the initial values
  MEinits <- function() {list(
    
    # Latent states
    z = zinit,
    
    # Parameters
    # Occupancy
    beta_psi = runif(MEconsts$ncov_psi, -3 , -3),
    
    # Detection
    beta_p = runif(MEconsts$ncov_p, -3 , -3)
    
  )}
  
  # List parameters to monitor
  MEmons <- c( 
    # Psi covariates
    "beta_psi",
    
    # P covariates
    "beta_p"
  )
  
  # Latent variables to monitor:
  MElatent <- c("n_plants_per_bee", "z", "u", "v",
                "p.fit", "p.fitnew", "p.diff")
  
  
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
                        niter = n.iter, 
                        nburnin = n.burn,
                        thin = n.thin1, 
                        thin2 = n.thin2,
                        setSeed = seed)
    
  # Return MCMC results
  return(results)
  
  
}
