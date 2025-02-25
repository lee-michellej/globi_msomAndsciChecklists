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
occ_model <- function(seed, 
                      n.iter, 
                      n.burn,
                      n.thin1, 
                      n.thin2,
                      model # 6 models: 
                               # no_bee_plant
                               # bee_species
                               # plant_species
                               # bee_family
                               # plant_family
                               # bee_plant_family
                                       ){
 

#------------ Load the library
  library(nimble, lib.loc = library_paths)
  library(reshape2, lib.loc = library_paths)
  
  
#------------  Upload the data & format
  # object name = bee.plant.cite
  # 3-D array
  load("/home/gdirenzo/globi/Data/data_summary/globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds")
  
  
  # Load covariates
  load("/home/gdirenzo/globi/Data/model_covariates - 2025 01 22 - no apis.rds")
  
  
  # Flatten the array
  dat_long <- melt(bee.plant.cite) 
  colnames(dat_long) <- c("bee_ID", "plant_ID", "cite_ID", "observation")
  

  # Create unique bee-plant pairs for the ecological model
  dat_pairs <- unique(subset(dat_long, select = c("bee_ID","plant_ID"))) 
  colnames(dat_pairs) <- c("bee_species", "plant_species")
                      
                      
# ------------  No bee or plant intercept model
  
  if(model == "no_bee_plant"){
    
  # Write the model
  MEcode <- nimbleCode({
    
    # Priors for covariates
    
    for (j in 1:ncov_psi){
      beta_psi[j] ~ dnorm(0, sd = 5)
    }
    
    for (j in 1:ncov_p){
      beta_p[j] ~ dnorm(0, sd = 5)
    }
    
    # Ecological model
    for(i in 1:n_bee_plant){
      
      # True bee-plant interaction
      z[bee_species[i], plant_species[i]] ~ dbern(psi[bee_species[i], plant_species[i]])
      
      # Make the bee-plant interaction probability of function of variables
      logit(psi[bee_species[i], plant_species[i]]) <- 
        
        # Intercept
          # Average size bee
          # Social bee
          # Yellow flower color
          # Not bowl
        beta_psi[1] +
        
        # Bee size
        beta_psi[2] * size[bee_species[i]]+ 
        
        # Bee solitary (1 = yes; 0 = no)
        beta_psi[3] * solitary[bee_species[i]]+ 
        
        # Flower color
        # Different bee genus have different probabilities of interacting with different plant colors
        beta_psi[4] * other_flower_color[plant_species[i]]+
        beta_psi[5] * blue_flower_color[plant_species[i]]+
        beta_psi[6] * white_flower_color[plant_species[i]]+
        
        # Flower shape (1 = yes bowl; 0 = no bowl)
        beta_psi[7] * flower_shape[plant_species[i]]
      
    }
    
    # Observation model
    for(i in 1:n_bee_plant_cite){
      
      # Observed bee-plant interaction by source citation
      y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
      
      p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
      
      logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <-  
        
        # Intercept
          # Not striped
          # Average sized bee (= 0)
          # Observation data type
          # Flower color = yellow
          # Flower shape = no bowl
        beta_p[1] +
        
        # Bee Strippiness (1 = yes striped; 0 = no striped)
        beta_p[2] * striped[bee_ID[i]]+
        
        # Bee size
        beta_p[3] * size[bee_ID[i]]+
        
        # sourceCitation type
        # Literature (1 = yes; 0 = no)
        beta_p[4] * citation_lit[cite_ID[i]]+
        # Collection (1 = yes; 0 = no)
        beta_p[5] * citation_col[cite_ID[i]]+
        # Aggregated (1 = yes; 0 = no)
        beta_p[6] * citation_agg[cite_ID[i]]+
        
        # Flower color (1 = yes other; 0 = no other)
        beta_p[7] * other_flower_color[plant_ID[i]]+
        # blue
        beta_p[8] * blue_flower_color[plant_ID[i]]+
        # white
        beta_p[9] * white_flower_color[plant_ID[i]]+            
        
        # Flower shape (1 = yes bowl; 0 = no bowl)
        beta_p[10] * flower_shape[plant_ID[i]]
           
    }   
        
    # Calculate the total number of plants that each bee interacts with
    for(i in 1:n_bee){
      
      n_plants_per_bee[i] <- sum(z[i,])
      
    }
    
  })
  
  }
  
  # bee_species
  if(model == "bee_species"){
    
    # Write the model
    MEcode <- nimbleCode({
      
      # Priors
      for(i in 1:n_bee){ # For each bee species
        
        # Bee species-specific random effect for psi
        # psi = The bee-plant interaction probability
        u[i] ~ dnorm(0, sd = sigma_psi)
        
        # Bee species-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
        v[i] ~ dnorm(0, sd = sigma_p)
        
      }

      # sd values for psi
      sigma_psi ~ T(dnorm(0.0, sd = 3.0), 0,  )
      
      # sd values for p
      sigma_p ~  T(dnorm(0.0, sd = 3.0), 0,  )
      
      # Priors for covariates
      for (j in 1:ncov_psi){
        beta_psi[j] ~ dnorm(0,  sd = 5)
      }
      
      for (j in 1:ncov_p){
        beta_p[j] ~ dnorm(0,  sd = 5)
      }
      
      
      # Ecological model
      for(i in 1:n_bee_plant){
        
        # True bee-plant interaction
        z[bee_species[i], plant_species[i]] ~ dbern(psi[bee_species[i], plant_species[i]])
        
        # Make the bee-plant interaction probability of function of variables
        logit(psi[bee_species[i], plant_species[i]]) <-
          
          # Bee species-specific random effect
          u[bee_species[i]] +
          
          # Intercept
          # Average size bee
          # Not solitary bee
          # Yellow flower color
          # Not bowl
          beta_psi[1] +
          
          # Bee size
          beta_psi[2] * size[bee_species[i]]+ 
          
         	  # Bee solitary (1 = yes; 0 = no)
          beta_psi[3] * solitary[bee_species[i]]+ 
          
          # Flower color
          # Different bee genus have different probabilities of interacting with different plant colors
          beta_psi[4] * other_flower_color[plant_species[i]]+
          beta_psi[5] * blue_flower_color[plant_species[i]]+
          beta_psi[6] * white_flower_color[plant_species[i]]+
          
          # Flower shape (== bowl)
          beta_psi[7] * flower_shape[plant_species[i]]
      }
      
      # Observation model
      for(i in 1:n_bee_plant_cite){
        
        # Observed bee-plant interaction by source citation
        y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
        
        p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
        
        logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <- 
          
          # Bee species-specific random effect
          v[bee_ID[i]] +
          
          # Intercept
            # Not striped
            # Averge sized bee (= 0)
            # Observation data type
            # Flower color = yellow
            # Flower shape = no bowl
          beta_p[1] +
          
          # Bee Strippiness (1 = yes striped; 0 = no striped)
          beta_p[2] * striped[bee_ID[i]]+
          
          # Bee size
          beta_p[3] * size[bee_ID[i]]+
          
          # sourceCitation type
          # Literature (1 = yes; 0 = no)
          beta_p[4] * citation_lit[cite_ID[i]]+
          # Collection (1 = yes; 0 = no)
          beta_p[5] * citation_col[cite_ID[i]]+
          # Aggregated (1 = yes; 0 = no)
          beta_p[6] * citation_agg[cite_ID[i]]+
          
          # Flower color (1 = yes other; 0 = no other)
          beta_p[7] * other_flower_color[plant_ID[i]]+
          # blue
          beta_p[8] * blue_flower_color[plant_ID[i]]+
          # white
          beta_p[9] * white_flower_color[plant_ID[i]]+            
          
          # Flower shape (1 = yes bowl; 0 = no bowl)
          beta_p[10] * flower_shape[plant_ID[i]]
       
        
      }   
            
      # Calculate the total number of plants that each bee interacts with
      for(i in 1:n_bee){
        
        n_plants_per_bee[i] <- sum(z[i,])
        
      }
      
    })
    
  }
  
  # plant_species
  if(model == "plant_species"){
    
    # Write the model
    MEcode <- nimbleCode({
      
      # Priors
      for(i in 1:n_plant){ # For each bee species
        
        # Plant species-specific random effect for psi
        # psi = The bee-plant interaction probability
        u[i] ~ dnorm(0, sd = sigma_psi)
        
        # Plant species-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
        v[i] ~ dnorm(0, sd = sigma_p)
        
      }

      # sd values for psi
      sigma_psi ~ T(dnorm(0.0, sd = 3.0), 0,  )
      
      # sd values for p
      sigma_p ~  T(dnorm(0.0, sd = 3.0), 0,  )
      
      # Priors for covariates
      for (j in 1:ncov_psi){
        beta_psi[j] ~ dnorm(0, sd = 5)
      }
      
      for (j in 1:ncov_p){
        beta_p[j] ~ dnorm(0, sd = 5)
      }
      
      # Ecological model
      for(i in 1:n_bee_plant){
        
        # True bee-plant interaction
        z[bee_species[i], plant_species[i]] ~ dbern(psi[bee_species[i], plant_species[i]])
        
        # Make the bee-plant interaction probability of function of variables
        logit(psi[bee_species[i], plant_species[i]]) <- 
          
          # Plant species-specific random effect
          u[plant_species[i]] +
          
          # Intercept
          # Average size bee
          # Not solitary bee
          # Yellow flower color
          # Not bowl
          beta_psi[1] +
          
          # Bee size
          beta_psi[2] * size[bee_species[i]]+ 
          
          # Bee solitary (1 = yes; 0 = no)
          beta_psi[3] * solitary[bee_species[i]]+ 
          
          # Flower color
          # Different bee genus have different probabilities of interacting with different plant colors
          beta_psi[4] * other_flower_color[plant_species[i]]+
          beta_psi[5] * blue_flower_color[plant_species[i]]+
          beta_psi[6] * white_flower_color[plant_species[i]]+
          
          # Flower shape (== bowl)
          beta_psi[7] * flower_shape[plant_species[i]]
      }
      
      # Observation model
      for(i in 1:n_bee_plant_cite){
        
        # Observed bee-plant interaction by source citation
        y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
        
        p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
        
        logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <- 
          
          # Plant species-specific random effect
          v[plant_ID[i]] +
          
          # Intercept
            # Not striped
            # Averge sized bee (= 0)
            # Observation data type
            # Flower color = yellow
            # Flower shape = no bowl
          beta_p[1] +
          
          # Bee Strippiness (1 = yes striped; 0 = no striped)
          beta_p[2] * striped[bee_ID[i]]+
          
          # Bee size
          beta_p[3] * size[bee_ID[i]]+
          
          # sourceCitation type
          # Literature (1 = yes; 0 = no)
          beta_p[4] * citation_lit[cite_ID[i]]+
          # Collection (1 = yes; 0 = no)
          beta_p[5] * citation_col[cite_ID[i]]+
          # Aggregated (1 = yes; 0 = no)
          beta_p[6] * citation_agg[cite_ID[i]]+
          
          # Flower color (1 = yes other; 0 = no other)
          beta_p[7] * other_flower_color[plant_ID[i]]+
          # blue
          beta_p[8] * blue_flower_color[plant_ID[i]]+
          # white
          beta_p[9] * white_flower_color[plant_ID[i]]+            
          
          # Flower shape (1 = yes bowl; 0 = no bowl)
          beta_p[10] * flower_shape[plant_ID[i]]
                
      }   
            
      # Calculate the total number of plants that each bee interacts with
      for(i in 1:n_bee){
        
        n_plants_per_bee[i] <- sum(z[i,])
        
      }
      
    })
    
    
  }
  
  # bee_family
  if(model == "bee_family"){

    # Write the model
    MEcode <- nimbleCode({
      
      # Priors
      for(i in 1:n_bee_fam){ # For each bee species
        
        # Bee species-specific random effect for psi
        # psi = The bee-plant interaction probability
        u[i] ~ dnorm(0, sd = sigma_psi)
        
        # Bee species-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
        v[i] ~ dnorm(0, sd = sigma_p)
        
      }
      
      # sd values for psi
      sigma_psi ~  T(dnorm(0.0, sd = 3.0), 0,  )
      
      # sd values for p
      sigma_p ~   T(dnorm(0.0, sd = 3.0), 0,  )
      
      # Priors for covariates
      for (j in 1:ncov_psi){
        beta_psi[j] ~ dnorm(0, sd = 5)
      }
      
      for (j in 1:ncov_p){
        beta_p[j] ~ dnorm(0, sd = 5)
      }
      
      
      # Ecological model
      for(i in 1:n_bee_plant){
        
        # True bee-plant interaction
        z[bee_species[i], plant_species[i]] ~ dbern(psi[bee_species[i], plant_species[i]])
        
        # Make the bee-plant interaction probability of function of variables
        logit(psi[bee_species[i], plant_species[i]]) <-
          
          # Bee family-specific random effect
          u[bee_family[bee_species[i]]] +
          
          # Intercept
          # Average size bee
          # Not solitary bee
          # Yellow flower color
          # Not bowl
          beta_psi[1] +
          
          # Bee size
          beta_psi[2] * size[bee_species[i]]+ 
          
          # Bee solitary (1 = yes; 0 = no)
          beta_psi[3] * solitary[bee_species[i]]+ 
          
          # Flower color
          # Different bee genus have different probabilities of interacting with different plant colors
          beta_psi[4] * other_flower_color[plant_species[i]]+
          beta_psi[5] * blue_flower_color[plant_species[i]]+
          beta_psi[6] * white_flower_color[plant_species[i]]+
          
          # Flower shape (== bowl)
          beta_psi[7] * flower_shape[plant_species[i]]
      }
      
      # Observation model
      for(i in 1:n_bee_plant_cite){
        
        # Observed bee-plant interaction by source citation
        y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
        
        p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
        
        logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <- 
          
          # Bee family-specific random effect
          v[bee_family[bee_ID[i]]] +
          
          # Intercept
            # Not striped
            # Averge sized bee (= 0)
            # Observation data type
            # Flower color = yellow
            # Flower shape = no bowl
          beta_p[1] +
          
          # Bee Strippiness (1 = yes striped; 0 = no striped)
          beta_p[2] * striped[bee_ID[i]]+
          
          # Bee size
          beta_p[3] * size[bee_ID[i]]+
          
          # sourceCitation type
          # Literature (1 = yes; 0 = no)
          beta_p[4] * citation_lit[cite_ID[i]]+
          # Collection (1 = yes; 0 = no)
          beta_p[5] * citation_col[cite_ID[i]]+
          # Aggregated (1 = yes; 0 = no)
          beta_p[6] * citation_agg[cite_ID[i]]+
          
          # Flower color (1 = yes other; 0 = no other)
          beta_p[7] * other_flower_color[plant_ID[i]]+
          # blue
          beta_p[8] * blue_flower_color[plant_ID[i]]+
          # white
          beta_p[9] * white_flower_color[plant_ID[i]]+            
          
          # Flower shape (1 = yes bowl; 0 = no bowl)
          beta_p[10] * flower_shape[plant_ID[i]]
                
      }   
      
      # Calculate the total number of plants that each bee interacts with
      for(i in 1:n_bee){
        
        n_plants_per_bee[i] <- sum(z[i,])
        
      }
      
    })
    
  }
  
  # plant_family
  if(model == "plant_family"){
    
    # Write the model
    MEcode <- nimbleCode({
      
      # Priors
      for(i in 1:n_plant_fam){ # For each bee species
        
        # Plant family-specific random effect for psi
        # psi = The bee-plant interaction probability
        u[i] ~ dnorm(0, sd = sigma_psi)
        
        # Plant family-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
        v[i] ~ dnorm(0, sd = sigma_p)
        
      }
      
      # sd values for psi
      sigma_psi ~  T(dnorm(0.0, sd = 3.0), 0,  )
      
      # sd values for p
      sigma_p ~  T(dnorm(0.0, sd = 3.0), 0,  )
      
      # Priors for covariates
      for (j in 1:ncov_psi){
        beta_psi[j] ~ dnorm(0, sd = 5)
      }
      
      for (j in 1:ncov_p){
        beta_p[j] ~ dnorm(0, sd = 5)
      }
      
      # Ecological model
      for(i in 1:n_bee_plant){
        
        # True bee-plant interaction
        z[bee_species[i], plant_species[i]] ~ dbern(psi[bee_species[i], plant_species[i]])
        
        # Make the bee-plant interaction probability of function of variables
        logit(psi[bee_species[i], plant_species[i]]) <- 
          
          # Plant family-specific random effect
          u[plant_family[plant_species[i]]] +
          
          # Intercept
          # Average size bee
          # Not solitary bee
          # Yellow flower color
          # Not bowl
          beta_psi[1] +
          
          # Bee size
          beta_psi[2] * size[bee_species[i]]+ 
          
          # Bee solitary (1 = yes; 0 = no)
          beta_psi[3] * solitary[bee_species[i]]+ 
          
          # Flower color
          # Different bee genus have different probabilities of interacting with different plant colors
          beta_psi[4] * other_flower_color[plant_species[i]]+
          beta_psi[5] * blue_flower_color[plant_species[i]]+
          beta_psi[6] * white_flower_color[plant_species[i]]+
          
          # Flower shape (== bowl)
          beta_psi[7] * flower_shape[plant_species[i]]
      }
      
      # Observation model
      for(i in 1:n_bee_plant_cite){
        
        # Observed bee-plant interaction by source citation
        y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
        
        p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
        
        logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <- 
          
          # Plant family-specific random effect
          v[plant_family[plant_ID[i]]] +
          
          # Intercept
          # Not striped
          # Average sized bee (= 0)
          # Observation data type
          # Flower color = other
          # Flower shape = no bowl
          beta_p[1] +
          
          # Bee Stripiness (1 = yes striped; 0 = no striped)
          beta_p[2] * striped[bee_ID[i]]+
          
          # Bee size
          beta_p[3] * size[bee_ID[i]]+
          
          # sourceCitation type
          # Literature (1 = yes; 0 = no)
          beta_p[4] * citation_lit[cite_ID[i]]+
          # Collection (1 = yes; 0 = no)
          beta_p[5] * citation_col[cite_ID[i]]+
          # Aggregated (1 = yes; 0 = no)
          beta_p[6] * citation_agg[cite_ID[i]]+
          
          # Flower color (1 = yes other; 0 = no other)
          beta_p[7] * other_flower_color[plant_ID[i]]+
          # blue
          beta_p[8] * blue_flower_color[plant_ID[i]]+
          # white
          beta_p[9] * white_flower_color[plant_ID[i]]+            
          
          # Flower shape (1 = yes bowl; 0 = no bowl)
          beta_p[10] * flower_shape[plant_ID[i]]
                
      }   
            
      # Calculate the total number of plants that each bee interacts with
      for(i in 1:n_bee){
        
        n_plants_per_bee[i] <- sum(z[i,])
        
      }
      
    }) 
  }
  
  # bee_plant_family
  if(model == "bee_plant_family"){
    
    # Write the model
    MEcode <- nimbleCode({
      
      # Priors
      
      # Plant family
      for(i in 1:n_plant_fam){
        
        # Plant family-specific random effect for psi
        # psi = The bee-plant interaction probability
        u[i] ~ dnorm(0, sd = sigma_psi_u)
        
        # Plant family-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
        v[i] ~ dnorm(0, sd = sigma_p_v)
        
      }
      
      # sd values for psi
      sigma_psi_u ~ T(dnorm(0.0, sd = 3.0), 0,  )
      sigma_psi_g ~  T(dnorm(0.0, sd = 3.0), 0,  )
      
      # sd values for p
      sigma_p_v ~   T(dnorm(0.0, sd = 3.0), 0,  )
      sigma_p_d ~   T(dnorm(0.0, sd = 3.0), 0,  )
      
      # Bee family
      for(i in 1:n_bee_fam){ # For each bee species
        
        # Plant family-specific random effect for psi
        # psi = The bee-plant interaction probability
        g[i] ~ dnorm(0, sd = sigma_psi_g)
        
        # Plant family-specific random effect for p
        # p = The detection probability of documented a bee-plant interaction
        d[i] ~ dnorm(0, sd = sigma_p_d)
        
      }
      
      # Priors for covariates
      for (j in 1:ncov_psi){
        beta_psi[j] ~ dnorm(0,  sd = 5)
      }
      
      for (j in 1:ncov_p){
        beta_p[j] ~ dnorm(0, sd = 5)
      }
      
      # Ecological model
      for(i in 1:n_bee_plant){
        
        # True bee-plant interaction
        z[bee_species[i], plant_species[i]] ~ dbern(psi[bee_species[i], plant_species[i]])
        
        # Make the bee-plant interaction probability of function of variables
        logit(psi[bee_species[i], plant_species[i]]) <- 
          
          # Plant family-specific random effect
          u[plant_family[plant_species[i]]] +
          
          # Bee family-specific random effect
          g[bee_family[bee_species[i]]] +
          
          # Intercept
          # Average size bee
          # Not solitary bee
          # Yellow flower color
          # Not bowl
          beta_psi[1] +
          
          # Bee size
          beta_psi[2] * size[bee_species[i]]+ 
          
          # Bee solitary (1 = yes; 0 = no)
          beta_psi[3] * solitary[bee_species[i]]+ 
          
          # Flower color
          # Different bee genus have different probabilities of interacting with different plant colors
          beta_psi[4] * other_flower_color[plant_species[i]]+
          beta_psi[5] * blue_flower_color[plant_species[i]]+
          beta_psi[6] * white_flower_color[plant_species[i]]+
          
          # Flower shape (== bowl)
          beta_psi[7] * flower_shape[plant_species[i]]
        
        
      }
      
      # Observation model
      for(i in 1:n_bee_plant_cite){
        
        # Observed bee-plant interaction by source citation
        y[i] ~ dbern(p.eff[bee_ID[i], plant_ID[i], cite_ID[i]])
        
        p.eff[bee_ID[i], plant_ID[i], cite_ID[i]] <- p[bee_ID[i], plant_ID[i], cite_ID[i]] * z[bee_ID[i], plant_ID[i]]
        
        logit(p[bee_ID[i], plant_ID[i], cite_ID[i]]) <- 
          
          # Plant family-specific random effect
          v[plant_family[plant_ID[i]]] +
          
          # Bee family-specific random effect
          d[bee_family[bee_ID[i]]] +    
            
          # Intercept
          # Not striped
          # Average sized bee (= 0)
          # Observation data type
          # Flower color = other
          # Flower shape = no bowl
          beta_p[1] +
          
          # Bee Stripiness (1 = yes striped; 0 = no striped)
          beta_p[2] * striped[bee_ID[i]]+
          
          # Bee size
          beta_p[3] * size[bee_ID[i]]+
          
          # sourceCitation type
          # Literature (1 = yes; 0 = no)
          beta_p[4] * citation_lit[cite_ID[i]]+
          # Collection (1 = yes; 0 = no)
          beta_p[5] * citation_col[cite_ID[i]]+
          # Aggregated (1 = yes; 0 = no)
          beta_p[6] * citation_agg[cite_ID[i]]+
          
          # Flower color (1 = yes other; 0 = no other)
          beta_p[7] * other_flower_color[plant_ID[i]]+
          # blue
          beta_p[8] * blue_flower_color[plant_ID[i]]+
          # white
          beta_p[9] * white_flower_color[plant_ID[i]]+            
          
          # Flower shape (1 = yes bowl; 0 = no bowl)
          beta_p[10] * flower_shape[plant_ID[i]]
                
      }   
            
      # Calculate the total number of plants that each bee interacts with
      for(i in 1:n_bee){
        
        n_plants_per_bee[i] <- sum(z[i,])
        
      }
      
    }) 
  }
  
  # Bundle all the values that remain constant in the model
  MEconsts <- list(
    
    # Total number of bee species
    n_bee = dim(bee.plant.cite)[1],
    
    # Total number of plants species
    n_plant = dim(bee.plant.cite)[2],
    
    # Total number of unique citations,
    n_citations = dim(bee.plant.cite)[3],
    
    # Total number of bee families
    n_bee_fam = max(covariates$bee.covariates$family_num),
    
    # Total number of plant families
    n_plant_fam = max(covariates$plant.covariates$family_num),
    
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
    # citation_obs = covariates$citation.covariates$observation, 
    citation_agg = covariates$citation.covariates$aggregated, 

    # Flower covariates - color
    # yellow_flower_color  = covariates$plant.covariates$yellow,
    blue_flower_color  = covariates$plant.covariates$blue.new,
    white_flower_color  = covariates$plant.covariates$white,
    other_flower_color  = covariates$plant.covariates$other,

    # Flower covariates - shape
    flower_shape  = covariates$plant.covariates$bowl,
    
    # Number of covariates
    ncov_psi = 7,
    ncov_p = 10,
    
    # Species index for ecological model
    bee_species = dat_pairs$bee_species,
    plant_species = dat_pairs$plant_species,
    
    # ID index for observation model
    bee_ID = dat_long$bee_ID,
    plant_ID = dat_long$plant_ID,
    cite_ID = dat_long$cite_ID,
    
    # bee and plant family IDs (as numeric)
    bee_family = covariates$bee.covariates$family_num,
    plant_family = covariates$plant.covariates$family_num
  )
  
  # List the data
  MEdata <- list(y = dat_long$observation)
  
  # Initial values for the z array - latent state variable
  zinit <- apply(bee.plant.cite, c(1, 2), max, na.rm = TRUE) 
  zinit[zinit == -Inf] <- NA
  
  
  # Bundle the initial values
  MEinits <- function() {list(
    
    # Latent states
    z = zinit,
    
    # Parameters
    # Occupancy
    beta_psi = runif(MEconsts$ncov_psi, -3 , 3),
    
    # Detection
    beta_p = runif(MEconsts$ncov_p, -3 , 3)
    
  )}
  
  # List parameters to monitor
  # Coefficents
  if(model == "no_bee_plant"){
  MEmons <- c( 
    # Psi covariates
    "beta_psi",
    
    # P covariates
    "beta_p"
  )
  }
  
  if(model == "bee_species" | model == "plant_species" |
     model == "bee_family" | model == "plant_family"){
    MEmons <- c( 
      # Psi covariates
      "beta_psi",
      
      # P covariates
      "beta_p",
      
      "sigma_psi",
      "sigma_p"
    )
     
  }
  
  if(model == "bee_plant_family"){
    MEmons <- c( 
      # Psi covariates
      "beta_psi",
      
      # P covariates
      "beta_p",
      
      "sigma_psi_u",
      "sigma_psi_g",
      "sigma_p_v",
      "sigma_p_d"
    )
    
  }
  
  # latent state parameters
  if(model == "no_bee_plant"){
    # Latent variables to monitor:
    MElatent <- c("n_plants_per_bee", "z")
  }
  
  if(model == "bee_species" | model == "plant_species" |
     model == "bee_family" | model == "plant_family"){
      # Latent variables to monitor:
      MElatent <- c("n_plants_per_bee", "z", "u", "v")
  }
  
  if(model == "bee_plant_family"){
    # Latent variables to monitor:
    MElatent <- c("n_plants_per_bee", "z", "u", "v", "g", "d")
  }
  
  
  # Start creating/compiling the nimble model
   MEmodel <- nimbleModel(code = MEcode, 
                         constants = MEconsts, 
                         data = MEdata, 
                         inits = MEinits(), 
                         calculate = F, 
                         check = F)
  
  
  # Compile model
   cMEmodel <- compileNimble(MEmodel)

  # MCMC - set which parameters to monitor
   MEconf <- configureMCMC(cMEmodel,
  			  monitors = MEmons,
  			  monitors2 = MElatent
   			)
  
  # Build MCMC
   MEmcmc <- buildMCMC(MEconf)
  
  # Compile MCMC
   cMEmcmc <- compileNimble(MEmcmc)
  
  ## Run model:
   results <- runMCMC(
    mcmc = cMEmcmc, 
    niter = n.iter,
    nburnin = n.burn,
    thin = n.thin1,
    thin2 = n.thin2,
    nchain = 1
  )
    
  # Return MCMC results
   return(results)
  
}
