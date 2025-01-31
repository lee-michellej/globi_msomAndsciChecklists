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
library(parallel)
library(ggmcmc)


# local computer
setwd("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/")


# set working directory for Hovenweep (HPC):
# setwd("")






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


# Print when the model has started running:
cat("Running model:", model_name, "\n")


# date object for folder
date <- "2025 01 26"



# 4. Call in function with everything needed ------------------------------------------------



# Enable automatic differentiation
nimbleOptions(enableDerivs = TRUE)


# Call nimble models
source("./Code/Workflow/6.75-shell/model-code-2024-01-07.R")

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
    n.iter = 150000
    n.burn = 50000 
    n.thin1 = 10
    n.thin2 = 10
}


# 7. Run the models --------------------------------------------------------




# Number of cores available on this machine
detectCores()  

# Number to cores to use
ncore <- 3     

# Create the cluster
cl <- makeCluster(ncore)
registerDoParallel(cl)

# set seeds
seeds <- 1:ncore

# Save start time
start.time <- Sys.time()


# Run the model using dopar 
result <- foreach(x = seeds, 
                  .packages="nimble") %dopar% {
                    
                    # function
                    occ_model(seed = seeds[x],
                              n.iter = n.iter, 
                              n.burn = n.burn,
                              n.thin1 = n.thin1, 
                              n.thin2 = n.thin2,
                              model = model_name)
                  }

stopCluster(cl)

end.time <- Sys.time()

# How long did the model take?
end.time - start.time




# 8. Look at model outputs -------------------------------------------------




# # Row bind all of the chains together - coefficents
out <- as.mcmc(data.frame(rbind(result[[1]],
                                result[[2]],
                                result[[3]])))

# Make a list and MCMC
simp_list <- list()
simp_list[[1]] <- as.mcmc(result[[1]])
simp_list[[2]] <- as.mcmc(result[[2]])
simp_list[[3]] <- as.mcmc(result[[3]])

MCMClist <- mcmc.list(simp_list)
MCMClist2 <- mcmc.list(out)

MCMCsummary(MCMClist)

# Save MCMC output as table

write.csv(MCMCsummary(MCMClist),
          file = paste0("./Tables/", date,"Table-", model_name, "-MCMC-output.csv"))


# Traceplots
ggs_BYMeco <- ggs(MCMClist) 

# Sd and mu parameters
ggs_BYMeco %>% filter(Parameter %in% c( "mu.psi", "sigma.psi",
                                        "mu.p", "sigma.p",
                                        "sd_psi", "sd_p")) %>% 
  ggs_traceplot() + theme_bw()

# Beta_psi
ggs_BYMeco %>% 
  filter(Parameter %in% c( paste("beta_psi[", 1:4, "]", sep = ""))) %>% 
  ggs_traceplot() + 
  theme_bw()

ggsave(paste0("./Figures/", date, "traceplots-psi-", model_name, ".png"))


# Beta_p
ggs_BYMeco %>% 
  filter(Parameter %in% c( paste("beta_p[", 1:8, "]", sep = ""))) %>% 
  ggs_traceplot() + 
  theme_bw()

ggsave(paste0("./Figures/", date, "traceplots-p-", model_name, ".png"))


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
