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




# Set working directory
setwd("~/globi_tritrophic_networks/")


# Temp
setwd("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/")



# 2. Load data ------------------------------------------------




# Upload the data
# object name = bee.plant.cite
# 3-D array
load("./Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")


# Load covariates
load("./Data/model_covariates - 2025 01 22 - no apis.rds")


# Flatten the array
dat_long <- melt(bee.plant.cite) 
colnames(dat_long) <- c("bee_ID", "plant_ID", "cite_ID", "observation")




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







# 4. Call in function with everything needed ------------------------------------------------


setwd("/Users/gdirenzo/Documents/GitHub/globi_msomAndsciChecklists/")

# Enable automatic differentiation
nimbleOptions(enableDerivs = TRUE)


# Call nimble models
source("./Code/Workflow/6.5 - model-code-2024-01-07.R")

# There is 1 function where you can specify the type of model:
  # 1. No bee/plant specification = no_bee_plant
  # 2. bee-specific intercepts = bee_species
  # 3. plant-specific intercepts = plant_species
  # 4. bee family intercepts = bee_family
  # 5. plant family intercepts = plant_family
  # 6. bee and plant family intercepts = bee_plant_family


# For debugging use:
    # n.iter = 2, 
    # n.burn = 1,
    # n.thin1 = 1, 
    # n.thin2 = 1

# For full run use:
    # n.iter = 250000, 
    # n.burn = 50000, 
    # n.thin1 = 10, 
    # n.thin2 = 10



# 7. Run the models --------------------------------------------------------


# 150,000 iterations = XXX hours
# 250,000 iterations = XXX days





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
                              n.iter = 150000, 
                              n.burn = 50000,
                              n.thin1 = 10, 
                              n.thin2 = 10,
                              model = "bee_family")
                  }

stopCluster(cl)

end.time <- Sys.time()

beepr::beep(2)

# How long did the model take?
end.time - start.time



# Models checked:
  # bee_species
    # 15 hrs
      # n.iter = 150000, 
      # n.burn = 50000,
      # n.thin1 = 10, 
      # n.thin2 = 10,
  # bee_family
    # X hrs -- started Sat Jan 26 at 2 pm 
      # n.iter = 150000, 
      # n.burn = 50000,
      # n.thin1 = 10, 
      # n.thin2 = 10,  
  # no_bee_plant ~ 20 min
  # plant_species ~ 20 min
  # plant_family -- 55 min
  # bee_plant_family - 50.01325 mins




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
          file = "./Tables/Table-MCMC-output-2025 01 26- bee intercept.csv")




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



# date object for folder
date <- "2025 01 26"

# Model name
mod_name <- "bee-species-mod"

setwd("/Users/gdirenzo/OneDrive - University of Massachusetts/Dropbox_transfer/Globi/")


# Save the model output
save(out, 
     file = paste0("./ModelOutput/", date, "/globi-short plant list-", date, "_"
     , mod_name, "- all cov - NO apis - NIMBLE.rds"))

save(result, 
     file = paste0("./ModelOutput/", date, "/OUTPUT - globi-short plant list-", date, "_"
                   , mod_name, "- all cov - NO apis - NIMBLE.rds"))

save(MCMClist,
     file = paste0("./ModelOutput/", date, "/MCMClist- globi-short plant list-", date, "_"
                   , mod_name,"- all cov - NO apis - NIMBLE.rds"))


# End script
