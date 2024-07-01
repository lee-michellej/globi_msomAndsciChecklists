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
  # p = The probability that a sourceCitation documented the bee-plant interaction


########################################
####### Code Output ####################
########################################



# This code generates the following files:
  # Table 1 in the manuscript
  # Generates model output - which is saved and processed in other files



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
setwd("~/globi_tritrophic_networks/")





# 2. Load data ------------------------------------------------




# Upload the data
  # object name = bee.plant.date.cite
  # 4-D array
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2024_07_01 - short plant list - no apis.rds")









# 3. Calculate derived quantities of interest ------------------------------------------------




# In this section, we will determine the observed number of plant species that each bee species interacts with


# First, we will determine if the bee-plant interaction occurs across any study
# We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(bee.plant.cite, c(1, 2), max, na.rm = TRUE)
y.bee.plant[y.bee.plant == "-Inf"] <- 0


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Observed number of plant species that each bee interacts with
obs.dat <- data.frame(obs = y.bee.plant)

# Visualize the data
ggplot(data = obs.dat, aes(obs)) + 
  geom_histogram(col = "black")+
  xlab("Number of plant interactions per bee species")+
  ylab("Total counts")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold")) 


 #ggsave(file = "~/Github/globi_tritrophic_networks/Figures/2022_05_12/Bee-plant-Observation-Histogram.pdf", 
 #       height = 4,
 #       width = 6)







# 4. Write function with everything needed ------------------------------------------------




# Enable automatic differentiation
nimbleOptions(enableDerivs = TRUE)



# Create a function with all the needed code
run_MCMC_allcode <- function(seed){
  
  # Load the library
  library(nimble)
  library(reshape2)
  
  
  # Upload the data
  # object name = bee.plant.date.cite
  # 3-D array
  load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2024_07_01 - short plant list - no apis.rds")
 
 
  # Load covariates
  load("./Data/model_covariates - 2024 07 01 - no apis.rds")
  
  # Flatten the array
  dat_long <- melt(bee.plant.cite) 
  colnames(dat_long) <- c("bee_ID", "plant_ID", "cite_ID", "observation")
  
  
  # Write the model
  MEcode <- nimbleCode({
    
    # Priors
    for(i in 1:n_bee_plant){ # For each bee species
      
      # Species-specific random effect for psi
        # psi = The bee-plant interaction probability
      u[bee_ID[i], plant_ID[i]] ~ dnorm(mu_psi, tau_psi)
      
      # Species-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
      v[bee_ID[i], plant_ID[i]] ~ dnorm(mu_p, tau_p)
      
      
    }
    
    
    # Mean bee-plant interaction probability
    mu_psi ~ dnorm(0, 0.368)

    # Precision and sd values for psi
    tau_psi <- 1/(sigma_psi * sigma_psi)
    sigma_psi ~ T(dnorm(0, sd = sqrt(1/2)), 0, 10)

    
    # Mean bee-plant detection probability
    mu_p ~ dnorm(0, 0.368)
    
    # Precision and sd values for p
    tau_p <- 1/(sigma_p * sigma_p)
    sigma_p ~ T(dnorm(0, sd = sqrt(1/2)), 0, 10)

    
    
    # Priors for covariates
    
    for (j in 1:ncov_psi){

     beta_psi[j] ~ dnorm(0, 0.368)
      
    }
    
    for (j in 1:ncov_p){
      
      beta_p[j] ~ dnorm(0, 0.368)
      
    }
    
    
    # Ecological model
    for(i in 1:n_bee_plant){
      
      # True bee-plant interaction
      z[bee_ID[i], plant_ID[i]] ~ dbern(psi[bee_ID[i], plant_ID[i]])
      
      # Make the bee-plant interaction probability of function of variables
      logit(psi[bee_ID[i], plant_ID[i]]) <- 
                          # Bee-plant species-specific random effect
                          u[bee_ID[i], plant_ID[i]] +
        
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
                            # Bee-plant species-specific random effect
                            v[bee_ID[i], plant_ID[i]] +
        
                            # Intercept
                              # Not stripped
                              # Averge sized bee (= 0)
                              # Aggregated data type
                              # Flower color = other
                              # Flower shape = no bowl
                              beta_p[1] +
        
                            # Bee Strippiness (1 = yes stripped; 0 = no stripped)
                              beta_p[2] * stripped[bee_ID[i]]+
        
                            # Bee size
                              beta_p[3] * size[bee_ID[i]]+
        
                            # sourceCitation type
                              # Literature (1 = yes; 0 = no)
                              beta_p[4] * citation_lit[cite_ID[i]]+
                              # Collection (1 = yes; 0 = no)
                              beta_p[5] * citation_col[cite_ID[i]]+
        
                            # Flower color (1 = yes yellow; 0 = no yellow)
                              beta_p[6] * yellow_flower_color[plant_ID[i]]+
                              # blue
                              beta_p[7] * blue_flower_color[plant_ID[i]]+
                              # white
                              beta_p[8] * white_flower_color[plant_ID[i]]+            
        
                            # Flower shape (1 = yes bowl; 0 = no bowl)
                              beta_p[9] * flower_shape[plant_ID[i]]
  
         
    }   
    
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
    stripped      = covariates$bee.covariates$striped,
    size          = covariates$bee.covariates$size_std,
    solitary      = covariates$bee.covariates$solitary,
    
    # sourceCitation covariates
    citation_lit = covariates$citation.covariates$literature, 
    citation_col = covariates$citation.covariates$collection, 
    
    # Flower covariates - color
    yellow_flower_color  = covariates$plant.covariates$yellow,
    blue_flower_color  = covariates$plant.covariates$blue,
    white_flower_color  = covariates$plant.covariates$white,
    
    # Flower covariates - shape
    flower_shape  = covariates$plant.covariates$bowl,
    
    # Number of covariates
    ncov_psi = 7,
    ncov_p = 9,
    
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
    mu_psi = runif(1, -10 , -1),
    sigma_psi = runif(1, 0 , 1),
    beta_psi = runif(MEconsts$ncov_psi, -3 , -3),
    
    # Detection
    mu_p = runif(1, -10 , -1),
    sigma_p = runif(1, 0 , 1),
    beta_p = runif(MEconsts$ncov_p, -3 , -3)
    
  )}
  
  
  
  
  # List parameters to monitor
  MEmons <- c( "mu_psi", "sigma_psi",
               "mu_p", "sigma_p",
               
               # Psi covariates
               "beta_psi",

               # P covariates
               "beta_p"
  )
  
  MElatent <- c("n_plants_per_bee")
  
  
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
 # results <-  runMCMC(cMEmcmc, 
 #                     nchains = 1, 
 #                     niter = 150000, 
 #                     nburnin = 50000, 
 #                     thin = 10, 
 #                     thin2 = 10,
 #                     setSeed = seed)
 # 
  
# ## Run MCMC
    ## Used for debugging code
 results <-  runMCMC(cMEmcmc, 
                     nchains = 1, 
                     niter = 5, 
                     nburnin = 2, 
                     thin = 1, 
                     thin2 = 1,
                     setSeed = seed)
 
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
  # Model takes 2 - 4 hrs to run
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
    

# Started at:
  # 12:57 - all data with 5 iterations






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

# Save MCMC output as table

write.csv(MCMCsummary(MCMClist),
          file = "./Tables/Table-1-MCMC-output-SSVS.csv")




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
     file = "./ModelOutput/globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")

save(result, 
     file = "./ModelOutput/OUTPUT - globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")

save(MCMClist,
     file = "./ModelOutput/MCMClist- globi-short plant list- 2022 05 12 - all cov - NO apis - NIMBLE - SSVS.rds")


# End script
