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


# set working directory for Hovenweep (HPC):
setwd("/home/gdirenzo/globi/")






# 2. Set up model name from shell script ----------------------------



# Get the model name from command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No model name provided")
}

# Save model name as the first argument
model_name <- args[1]

# Save the type of model run
model_run_type <- args[2]


# Stop if no arguments are passed
if (length(args) == 0) {
  stop("No model name provided")
}

# Print when the model has started running:
cat("Running model:", model_name, "\n")


# date object for folder
date <- "2025 01 26"



# 4. Call in function with everything needed ------------------------------------------------



# Enable automatic differentiation
nimbleOptions(enableDerivs = TRUE)


# Call nimble models
source("/home/gdirenzo/globi/Code/model-code-2024-01-07.R")

# There is 1 function where you can specify the type of model:
  # 1. No bee/plant specification = no_bee_plant
  # 2. bee-specific intercepts = bee_species
  # 3. plant-specific intercepts = plant_species
  # 4. bee family intercepts = bee_family
  # 5. plant family intercepts = plant_family
  # 6. bee and plant family intercepts = bee_plant_family


# For debugging use: model_run_type = test

if(model_run_type == "test"){
  n.iter = 2 
  n.burn = 1
  n.thin1 = 1
  n.thin2 = 1
}

# For full run use: model_run_type = full

if(model_run_type == "full"){
    n.iter = 250000
    n.burn = 50000 
    n.thin1 = 10
    n.thin2 = 10
}

# Print the type of model run type it is:
print(paste0("Model run type = ", model_run_type))


# 7. Run the models --------------------------------------------------------



# Number of cores available on this machine
detectCores()  

# Number of cores to use
ncore <- 3     

# Make a PSOCK cluster explicitly:
cl <- parallel::makeCluster(ncore, type = "PSOCK")

# Register the doParallel backend
registerDoParallel(cl)

# Load the nimble package and source the model code on each worker node
clusterEvalQ(cl, {
  # Specify the library location
  .libPaths( c(
    "/home/gdirenzo/R/x86_64-redhat-linux-gnu-library/4.2",
    "/home/software/hovenweep/arc/apps/R/library/4.2/GNU/12.1",
    "/opt/cray/pe/R/4.2.1.2/lib64/R/library"
  ))
  # Load necessary packages
  library(nimble)  # Ensure nimble is loaded on all nodes
  library(reshape2)  # Ensure reshape2 is loaded on all nodes
  # Source the model code to ensure that the required functions are available on each worker
  source("/home/gdirenzo/globi/Code/model-code-2024-01-07.R")
})

# Export the necessary global variables to each worker node (no need to export again in foreach)
clusterExport(cl, list("occ_model", 
                       "model_name", 
                       "n.iter", 
                       "n.burn", 
                       "n.thin1", 
                       "n.thin2",
                       "library_paths"), envir = .GlobalEnv)

# set seeds for each worker
seeds <- 1:ncore

# Save start time
start.time <- Sys.time()

 # Run the model using dopar 
  result <- foreach(x = seeds, 
                   .packages = c("nimble", "reshape2")
                   ) %dopar% {
                     
   # Run the model function
   occ_model(seed = seeds[x],
             n.iter = n.iter, 
             n.burn = n.burn,
             n.thin1 = n.thin1, 
             n.thin2 = n.thin2,
             model = model_name)
 }

# Run one chain of the model to determine if the parallel processing is the issue - this runs fine. The HPC runs continue. 
# result <- occ_model(seed = seeds[x],
#             n.iter = n.iter, 
#             n.burn = n.burn,
#             n.thin1 = n.thin1, 
#             n.thin2 = n.thin2,
#             model = model_name)


# Stop the cluster after use
stopCluster(cl)

# End time
end.time <- Sys.time()

# How long did the model take?
end.time - start.time







# 8. Look at model outputs -------------------------------------------------



# Print out structure of results:
print(paste0("results object structure:", str(result)))



# # Row bind all of the chains together - coefficents
out <- as.mcmc(data.frame(rbind(result[[1]]$samples,
                                result[[2]]$samples,
                                result[[3]]$samples)))


# Print out structure of out:
print(paste0("out object structure:", str(out)))


# Make a list and MCMC
simp_list <- list()
simp_list[[1]] <- as.mcmc(result[[1]]$samples)
simp_list[[2]] <- as.mcmc(result[[2]]$samples)
simp_list[[3]] <- as.mcmc(result[[3]]$samples)


# Print out structure of out:
print(paste0("simpl_list object structure:", str(simp_list)))


# Make MCMC list
MCMClist <- mcmc.list(simp_list)

# Print MCMC summary
MCMCsummary(MCMClist)


# Save MCMC output as table
write.csv(MCMCsummary(MCMClist),
          file = paste0("./Tables/", date, "/Table-", model_name, "-MCMC-output.csv"))


# Save the model output
save(out, 
     file = paste0("./ModelOutput/", date, "/out-", model_name, "-NIMBLE.rds"))

save(result, 
     file = paste0("./ModelOutput/", date, "/result-", model_name, "-NIMBLE.rds"))

save(MCMClist,
     file = paste0("./ModelOutput/", date, "/MCMClist-", model_name,"-NIMBLE.rds"))


# Print when the model has finished running
cat("Finished running model:", model_name, "\n")


# End script
