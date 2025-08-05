########################################
########################################
# This code was written by: G. V. DiRenzo & M. J. Lee
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



##################################
######## Code objective ##########
##################################


# To check model convergence




##################################
######## Output of Code ##########
##################################

# Identify which prior combiantion to retain for each model type


################################## 
################################## 
################################## 



# Set library path
.libPaths(c(.libPaths, library_paths))


# Load libraries
library(iterators, lib.loc = library_paths) 
library(foreach, lib.loc = library_paths)
library(dplyr, lib.loc = library_paths) 
library(tidyr, lib.loc = library_paths) 
library(reshape2, lib.loc = library_paths)
library(ggplot2, lib.loc = library_paths)
library(cowplot, lib.loc = library_paths)
library(nimble, lib.loc = library_paths)
library(parallel, lib.loc = library_paths)
library(doParallel, lib.loc = library_paths) 
library(MCMCvis, lib.loc = library_paths)
library(mcmcOutput, lib.loc = library_paths)
library(coda, lib.loc = library_paths)
library(parallel, lib.loc = library_paths)
library(ggmcmc, lib.loc = library_paths)



# six model names you gave
model_names <- c("bee_species", "plant_species",
                 "bee_family", "plant_family",
                 "bee_plant_family", "no_bee_plant")

n_fits <- 6                     # how many replicate fits per model

# containers: list-of-lists for summaries, list-of-numeric for max-R̂
mod_summaries <- setNames(vector("list", length(model_names)), model_names)
Rhat_max      <- setNames(vector("list", length(model_names)), model_names)
mod_with_Rhat_1.1   <- setNames(vector("list", length(model_names)), model_names)

for (mod in model_names) {
  
  mod_summaries[[mod]]     <- vector("list", n_fits)
  Rhat_max[[mod]]          <- numeric(n_fits)
  mod_with_Rhat_1.1[[mod]] <- numeric(n_fits)
  
  for (i in seq_len(n_fits)) {
    file     <- sprintf("./MCMClist-%s-with-priors-%d-NIMBLE.rds", mod, i)
    obj_name <- load(file)          # returns "MCMClist"
    mcmc     <- get(obj_name)
    
    summ <- MCMCsummary(mcmc)
    
    mod_summaries[[mod]][[i]] <- summ
    Rhat_max[[mod]][i]        <- max(summ$Rhat, na.rm = TRUE)
  }
  
  mod_with_Rhat_1.1[[mod]] <- which(Rhat_max[[mod]] < 1.1)
  
}

# Print the names of the models that converged (with Rhat < 1.1)
mod_with_Rhat_1.1

# Reminder of what the priors were:
prior_table <- expand.grid(prior_cov = c(1, 3, 5),
                           prior_sd = c(2, 3))



# We will move forward with the following models:

# $bee_species
# [1] 1
  #prior_cov prior_sd
  #         1        2

# $bee_family
# [1] 6
  #prior_cov prior_sd
  #        5        3

# $bee_plant_family
# [1] 5
  #prior_cov prior_sd
  #        3        3


# $plant_species
# $plant_family
# $no_bee_plant
# All models convereged - will use 6
  #prior_cov prior_sd
  #        5        3

# End script