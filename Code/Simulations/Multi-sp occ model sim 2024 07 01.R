########################################
########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



########################################
####### Code Objective #################
########################################


# To simulate and analyze data for a multi-species occupancy model

# We will estimate:
  # psi = The probability of a bee-plant interact
  # p = The probability that a study documented the bee-plant interaction


########################################
####### Code Output ####################
########################################



# This code generates the following files:
  # No files saved
  # 2 plots are created at the end to compare the truth & model estimates



########################################
########## Notes #######################
########################################



# We are interested in determining if social bees are more generalist (and visit more flowers) than solitary bees.
  # But there is sampling bias in our dataset (i.e., Apidae is over-represented with many more records for this family than others).
  # So, we need to account for species non-detections in the database across bee species

# To do this, we are using the Globi interactions database in combination with site checklists.
  # We have compiled bee & flower checklists for X number of sites, and subsetted the Globi database to those species.
  # Note that the Globi data does not need to geographically match with the bee & flower checklists of our sites.
  # Our questions relate to determining possible bee-flower interactions (but not specific to a site).
  # We use the checklists to retroactively assign species non-detections (similar to the Kery et al. 2009 paper where they ad hoc assigned bird non-detections using site checklists).
  # We are using different collections/source citations as the 'repeated survey' for each interaction.
    # In globi this will be, ecological studies (published papers), meta-analyses, different museums, and singleton observations from websites like iNaturalist.


# Things to think about:
  # How do we feel about retroactively assigning a bee-plant non-detection if a paper or collection never looked for that interaction? Or do we assume that they COULD have encountered it, so they COULD have documented it. It was within the releam of possibilities. 
    # What are the similaries and differences between what we are doing and what Kery et al. 2009 did with the bird checklists?

  # How do you feel about the fact that the Globi data and the site-specific checklists do not necessary match in terms of geographic extent?
    # What kinds of questions come up for you?
    # Do you feel like this is misleading? How so?



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





# 0. Load libraries & set working directory -------------------------------------------------------




# Load libraries
library(jagsUI)
library(reshape2)
library(ggplot2)
library(cowplot)


# Set working directory
setwd("~/globi_tritrophic_networks/")





# 1. Set simulated data design ------------------------------------------------




# Number of bee species
n.bee <- 5

# Number of plant species
n.plant <- 10

# Number of studies
n.study <- 15


# Empty matrices to hold data

# z = Latent state variable = the true interactions between bee and plant species
  # Rows = bee species
  # Columns = plant species
z <- array(NA, dim  = c(n.bee, n.plant))

# Add column and row names
colnames(z) <- paste("Plant", 1:n.plant)
rownames(z) <- paste("Bee", 1:n.bee)

# Look at the matrix we just made
  # This will be filled in with 0's and 1's to denote true bee-plant interaction
z

# y = observed data = detection of bee-plant interactions across studies
  # Rows = bee species
  # Columns = plant species
  # Sheet = different studies
y <- array(NA, dim  =c(n.bee, n.plant, n.study))

# Add column, row, and sheet names
colnames(y) <- paste("Plant", 1:n.plant)
rownames(y) <- paste("Bee", 1:n.bee)
dimnames(y)[[3]] <- paste("Study", 1:n.study)

# Look at the array we just made
  # This will be filled in with 0's and 1's to denote observatiosn of bee-plant interactions
y






# 2. Set parameter values ------------------------------------------------




# We will estimate 2 quantities:
  # The probability of a bee-plant interaction
  # The probability that a study documents the bee-plant interaction



# psi = The probability a bee species interacts with a plant species
  # Generalist bee species will have higher psi values, whereas specialist bee species will have lower values
  # We need obtain 1 psi value per bee species
psi <- matrix(runif(n.bee*n.plant, min = 0, max = 1),
              nrow = n.bee,
              ncol = n.plant)

# Looking at the values
  # There is 1 value per bee-plant combination, and it represents the probability of a bee-plant interaction
psi

# p = The probability that a study documents a bee-plant interaction
  # we are assuming it is constant across all sourceCitations
p <- matrix(runif(n.bee*n.plant, min = 0, max = 1),
            nrow = n.bee,
            ncol = n.plant)





# 3. Simulate data ------------------------------------------------



# First, we will simulate the true patterns of bee-plant interactions
  # To do this, we will go through each bee-plant interaction using a nested loop and generate bee-plant interactions using a bernoulli distribution (note that the bernoulli is a special case of the binomal where sample size = 1; the second value in the function below)
  # Each bee species has probability psi[i] that it interacts with a plant species
for(i in 1:n.bee){
  for(j in 1:n.plant){
    z[i,j] <- rbinom(1, 1, psi[i,j])
  }
}

# Look at the z matrix
  # The rows represent bee species
  # The columns represent plant species
  # 1 = that the bee and plant interact (truly)
  # 0 = the bee and plant do NOT interact
z


# Next, we will generate the observation process and determine if the bee * plant interaction was documented in a study
  # Observations are conditional on true bee + plant interactions (i.e., a bee-plant interaction can't be observed if it doesn't truly occur)- so we multiply p by z (the true pattern of bee and plant interactions)

#alpha <- 0.5
#beta <- 1
#pilosity <- runif(n.bee, 0, 1)
#
# p <- plogis(alpha + beta * pilosity)

for(i in 1:n.bee){
  for(j in 1:n.plant){
    for(k in 1:n.study){
      y[i,j,k] <- rbinom(1, 1, p[i,j] * z[i,j])
    }
  }
}


# Look at the observation array
  # The rows represent bee species
  # The columns represent plant species
  # The sheets represent different studies
  # 1 = the study observed a bee and plant interaction
  # 0 = the study did NOT observed a bee and plant interaction
y


# Let's make sure we understand the dimensions
# Show me if study 1 found an interaction between bee species 3 and plant species 5:

# Show me all of the bee-plant interactions documented by study 10:






# 4. Calculate derived quantities of interest ------------------------------------------------




# In this section, we will determine the true and observed number of plant species that each bee species interacts with

# For the truth, we just sum across the plant species for each bee species
z.bee.plant <- apply(z, c(1), sum)

z.bee.plant

# Now, let's see what the values are in our observed dataset:
  # First, we will determine if the bee-plant interaction occurs across any study
  # We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(y, c(1, 2), max, na.rm = TRUE)


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Compare the truth to observed
# True number of plant species that each bee interacts with
z.bee.plant

# Observed number of plant species that each bee interacts with
y.bee.plant

# If there are few plant species and high detection probabilities, then the true and observed values will be more similar. Divergence occurs as the number of plant species (and bees) increases, and as detection probability decreases.





# 5. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod.txt")
cat("
model{

# Priors
for(i in 1:n.bee){ # For each bee species
for(j in 1:n.plant){

    # psi = The probability of a bee-plant interact
      psi[i,j] ~ dunif(0, 1)
    
    # The detection probability = the probability a study documented a bee-plant interaction
      p[i,j] ~ dunif(0, 1)
      
  }
}



# Model
  # Note the parrelels between the model code here and how we generated the data above

for(i in 1:n.bee){

  for(j in 1:n.plant){
  
    # True bee-plant interaction
    
    z[i, j] ~ dbern(psi[i,j])
  
      for(k in 1:n.study){

      # Observed bee-plant interaction
      
        y[i,j,k] ~ dbern(p.eff[i,j,k])

        p.eff[i,j,k] <- p[i,j] * z[i,j]
        
    }
    
  }

}



# Derived quantities
# Determine the total number of plants that each bee interacts with
  # All we do is sum across the 2nd dimension of the z matrix (which represents plant species)

for(i in 1:n.bee){

    # To determine the total number of plant interactions per bee species, we will sum across the plants
    z.bee.plant[i] <- sum(z[i, ])

  }


}
",fill=TRUE)
sink()





# 5. Bundle the data ------------------------------------------------




# Bundle all the data together in a list
jags.data <- list(
  n.bee = dim(y)[1],
  n.plant = dim(y)[2],
  n.study = dim(y)[3],
  y = y)


# Initial values - to give the model reasonable starting values for the parameters and latent states
zinit <- apply(jags.data$y, c(1, 2), max, na.rm = TRUE) 
zinit[zinit == "-Inf"] <- 0

inits <- function() {list(
  # Latent states
  z = zinit,
  
  # Parameters
  psi = runif(jags.data$n.bee*jags.data$n.plant, 0 , 1),
  p = runif(jags.data$n.bee*jags.data$n.plant, 0 , 1)
  
)}


# List parameters to monitor
params <- c("psi", "p", "z.bee.plant")





# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 10000
na <- 10000
nb <- 3000
nt <- 5
nc <- 3





# 7. Run the models --------------------------------------------------------





# Run the model
  # Takes ~ 0.09 minutes
out <- jags(data = jags.data, 
            inits = inits, 
            parameters.to.save = params, 
            model = "./Model/globi_mod.txt", 
            n.chains = nc, 
            n.thin = nt, 
            n.iter = ni, 
            n.burnin = nb, 
            n.adapt = na,
            parallel = TRUE)


# Beep when done
beepr::beep(4)





# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Look at the traceplots
  # These should look like caterpillars on the left
  # And on the right it should be a nice smooth (not lumpy) distribution
plot(out)




# 9. Compare model outputs to truth ----------------------------------------






# Combine the names, truth, and model output
dat <- data.frame(names = c(paste("Bee sp ", 1:n.bee, " interaction prob", sep = ""),
                                   "Detection prob",
                            paste("Number of plants that Bee sp ", 1:n.bee, " interaction with", sep = "")), 
                  true = c(psi,
                           p,
                           z.bee.plant), 
                  mod.mean = c( out$mean$psi,
                                out$mean$p,
                                out$mean$z.bee.plant), 
                  mod.q2.5 = c(out$q2.5$psi,
                               out$q2.5$p,
                               out$q2.5$z.bee.plant), 
                  mod.q97.5 = c(out$q97.5$psi,
                                out$q97.5$p,
                                out$q97.5$z.bee.plant))


# Set colors for truth and estimates
cols <- c("Truth" = "red", "Estimated" = "black")

## Make the plots

# Plot with probabilities
ggplot(dat[1:6,], aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean, col = "Estimated")) +
  geom_point(size = 3, aes(x = names, y = true, col = "Truth")) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Parameter estimates') +
  xlab("Parameter names") +
  ggtitle("Parameter estimates")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Plot with number of bee-plant interactions
ggplot(dat[c(7:nrow(dat)),], aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean, col = "Estimated")) +
  geom_point(size = 3, aes(x = names, y = true, col = "Truth")) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Parameter estimates') +
  xlab("Parameter names") +
  ggtitle("Number of bee-plant interactions")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 



# End script
