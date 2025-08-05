
# Set library paths:
library_paths <- c( .libPaths(),
                    "/home/gdirenzo/R/x86_64-redhat-linux-gnu-library/4.2",
                    "/home/software/hovenweep/arc/apps/R/library/4.2/GNU/12.1",
                    "/opt/cray/pe/R/4.2.1.2/lib64/R/library"
)

#.libPaths(c(.libPaths, library_paths))


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


# Set working directory
setwd(paste0("/Volumes/DIRENZO/globi20250717-MCMC-ModOutput/MCMC/"))


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







#----- Code to subset the results object

load("./2025 07 02/result-bee_species-with-priors-1-NIMBLE.rds")

n.MCMC.samples <- nrow(result[[1]]$samples)
sub.set.half <- sample(1:n.MCMC.samples, n.MCMC.samples/2)
sub.set.quarter <- sample(1:n.MCMC.samples, n.MCMC.samples/4)

str(result)

subset_result_half <- list()
subset_result_half_chain_1 <- list()
subset_result_half_chain_2 <- list()
subset_result_half_chain_3 <- list()
subset_result_quarter <- list()
subset_result_quarter_chain_1 <- list()
subset_result_quarter_chain_2 <- list()
subset_result_quarter_chain_3 <- list()


#---- Subset to half of the MCMC runs
subset_result_half_chain_1[[1]] <- result[[1]]$samples[sub.set.half, ]
subset_result_half_chain_1[[2]] <- result[[1]]$samples2[sub.set.half, ]

subset_result_half_chain_2[[1]] <- result[[2]]$samples[sub.set.half, ]
subset_result_half_chain_2[[2]] <- result[[2]]$samples2[sub.set.half, ]

subset_result_half_chain_3[[1]] <- result[[3]]$samples[sub.set.half, ]
subset_result_half_chain_3[[2]] <- result[[3]]$samples2[sub.set.half, ]

names(subset_result_half_chain_1) <-
  names(subset_result_half_chain_2) <-
    names(subset_result_half_chain_3) <-c("samples", "samples2")

subset_result_half[[1]] <- subset_result_half_chain_1
subset_result_half[[2]] <- subset_result_half_chain_2
subset_result_half[[3]] <- subset_result_half_chain_3


#---- Subset to a quarter of the MCMC runs
subset_result_quarter_chain_1[[1]] <- result[[1]]$samples[sub.set.quarter, ]
subset_result_quarter_chain_1[[2]] <- result[[1]]$samples2[sub.set.quarter, ]

subset_result_quarter_chain_2[[1]] <- result[[2]]$samples[sub.set.quarter, ]
subset_result_quarter_chain_2[[2]] <- result[[2]]$samples2[sub.set.quarter, ]

subset_result_quarter_chain_3[[1]] <- result[[3]]$samples[sub.set.quarter, ]
subset_result_quarter_chain_3[[2]] <- result[[3]]$samples2[sub.set.quarter, ]

names(subset_result_quarter_chain_1) <-
  names(subset_result_quarter_chain_2) <-
    names(subset_result_quarter_chain_3) <-c("samples", "samples2")

subset_result_quarter[[1]] <- subset_result_quarter_chain_1
subset_result_quarter[[2]] <- subset_result_quarter_chain_2
subset_result_quarter[[3]] <- subset_result_quarter_chain_3
