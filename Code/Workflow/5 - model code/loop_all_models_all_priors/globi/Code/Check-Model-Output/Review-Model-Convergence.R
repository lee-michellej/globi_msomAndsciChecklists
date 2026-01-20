
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
setwd(paste0("/Volumes/DIRENZO/globi20250715-ModOutput/"))

load("2025 07 02/MCMClist-bee_species-with-priors-1-NIMBLE.rds")


# Print MCMC summary
priors_1 <- MCMCsummary(MCMClist)


load("2025 07 02/MCMClist-bee_species-with-priors-2-NIMBLE.rds")

# Print MCMC summary
priors_2 <- MCMCsummary(MCMClist)

max(priors_2$Rhat)




####---- DID NOT CONVERGE


load("2025 07 02/MCMClist-bee_species-with-priors-3-NIMBLE.rds")

# Print MCMC summary
priors_3 <- MCMCsummary(MCMClist)

####---- DID NOT CONVERGE

load("2025 07 02/MCMClist-bee_species-with-priors-4-NIMBLE.rds")

# Print MCMC summary
priors_4 <- MCMCsummary(MCMClist)


# Compare the SD estimate across models

load("/Volumes/DIRENZO/globi20250210/gdirenzo/globi/ModelOutput/2025 01 26/MCMClist-bee_family-NIMBLE.rds")
MCMC_bee_family <- MCMCsummary(MCMClist)

load("/Volumes/DIRENZO/globi20250210/gdirenzo/globi/ModelOutput/2025 01 26/MCMClist-bee_plant_family-NIMBLE.rds")
MCMC_bee_plant_family <- MCMCsummary(MCMClist)

load("/Volumes/DIRENZO/globi20250210/gdirenzo/globi/ModelOutput/2025 01 26/MCMClist-plant_family-NIMBLE.rds")
MCMC_plant_family <- MCMCsummary(MCMClist)




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
