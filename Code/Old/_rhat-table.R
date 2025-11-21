
globi_out_folder <- "/Volumes/DIRENZO/globi20250717-MCMC-ModOutput/MCMC/"

#---- bee_species

# Model name
mod_name <- "bee_species"

# Load the model output - out
load(paste0(globi_out_folder, "/MCMClist-bee_species-with-priors-1-NIMBLE.rds"))

# Save the out object with a new model specific name
MCMClist_bee_species <- MCMClist






#---- bee_family

# Model name
mod_name <- "bee_family"

# Load the model output
load(paste0(globi_out_folder, "/MCMClist-bee_family-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
MCMClist_bee_family <- MCMClist



#---- plant_species

# Model name
mod_name <- "plant_species"

# Load the model output
load(paste0(globi_out_folder, "/MCMClist-plant_species-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
MCMClist_plant_species <- MCMClist




#---- plant_family

# Model name
mod_name <- "plant_family"

# Load the model output
load(paste0(globi_out_folder, "/MCMClist-plant_family-with-priors-6-NIMBLE.rds"))

# Save the out object with a new model specific name
MCMClist_plant_family <- MCMClist




#---- bee_plant_family

# Model name
mod_name <- "bee_plant_family"

# Load the model output
load(paste0(globi_out_folder, "/MCMClist-bee_plant_family-with-priors-5-NIMBLE.rds"))


# Save the out object with a new model specific name
MCMClist_bee_plant_family <- MCMClist




#---- no_bee_plant

# Model name
mod_name <- "no_bee_plant"

# Load the model output
load(paste0(globi_out_folder, "/MCMClist-no_bee_plant-with-priors-6-NIMBLE.rds"))


# Save the out object with a new model specific name
MCMClist_no_bee_plant <- MCMClist


#--------------------

library(ggmcmc)
library(MCMCvis)
library(mcmcOutput)


rhat_table <- data.frame(
  model = c(rep("bee species", times = nrow(MCMCsummary(MCMClist_bee_species))), 
            rep("bee family", times = nrow(MCMCsummary(MCMClist_bee_family))),
            
            rep("plant species", times = nrow(MCMCsummary(MCMClist_plant_species))),
            rep("plant family", times = nrow(MCMCsummary(MCMClist_plant_family))),
            
            rep("bee & plant family", times = nrow(MCMCsummary(MCMClist_bee_plant_family))),
            rep("no bee or plant", times = nrow(MCMCsummary(MCMClist_no_bee_plant)))),
  
  parameter = c(rownames(MCMCsummary(MCMClist_bee_species)),
                rownames(MCMCsummary(MCMClist_bee_family)),
                
                rownames(MCMCsummary(MCMClist_plant_species)),
                rownames(MCMCsummary(MCMClist_plant_family)),
                
                rownames(MCMCsummary(MCMClist_bee_plant_family)),
                rownames(MCMCsummary(MCMClist_no_bee_plant))),
  
  rbind(round(MCMCsummary(MCMClist_bee_species), dig = 2)[,-ncol(MCMCsummary(MCMClist_bee_species))],
        round(MCMCsummary(MCMClist_bee_family), dig = 2)[,-ncol(MCMCsummary(MCMClist_bee_family))],
        
        round(MCMCsummary(MCMClist_plant_species), dig = 2)[,-ncol(MCMCsummary(MCMClist_plant_species))],
        round(MCMCsummary(MCMClist_plant_family), dig = 2)[,-ncol(MCMCsummary(MCMClist_plant_family))],
        
        round(MCMCsummary(MCMClist_bee_plant_family), dig = 2)[,-ncol(MCMCsummary(MCMClist_bee_plant_family))],
        round(MCMCsummary(MCMClist_no_bee_plant), dig = 2)[,-ncol(MCMCsummary(MCMClist_no_bee_plant))])
)

# remove the beta from the parameter column
rhat_table$parameter <- gsub("beta_", "", rhat_table$parameter)

# Add a second table, theme : vanilla table
library(flextable)

set_flextable_defaults(font.size = 8)
rhat <- flextable(rhat_table)

save_as_docx(
  rhat,
  path = paste0(github_folder_path, "/Tables/rhat-table.docx"))

# Beta_psi
ggs_BYMeco %>% filter(Parameter %in% c( paste("beta_psi[", 1:4, "]", sep = ""))) %>% 
  ggs_traceplot() + theme_bw()

# Beta_p
ggs_BYMeco %>% filter(Parameter %in% c( paste("beta_p[", 1:8, "]", sep = ""))) %>% 
  ggs_traceplot() + theme_bw()


