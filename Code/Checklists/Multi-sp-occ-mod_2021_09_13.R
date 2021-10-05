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
  # p = The probability that a study documented the bee-plant interaction


########################################
####### Code Output ####################
########################################



# This code generates the following files:
  # No files saved
  # 2 plots are created at the end to compare the truth & model estimates




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



# Upload the data
load("./Data/globi_data_formatted_bee_plant_2021_09_13.rds")
  # object name = bee.plant.cite2


# Upload bee size
bee.size <- read.csv("~/Dropbox/Globi/Data/Bee-size_2021_09_13.csv")

# look at data structure
str(bee.size)



# Match bee names with 
bee.match <- data.frame(rownames(bee.plant.cite2), 
           bee.size$Species,
           rownames(bee.plant.cite2) %in% bee.size$Species)

bee.match[which(bee.match$rownames.bee.plant.cite2...in..bee.size.Species == FALSE),]




# Read in file with type of citation
citation <- read.csv("./Data/citation-list-type.csv")



# 2. Calculate derived quantities of interest ------------------------------------------------




# In this section, we will determine the observed number of plant species that each bee species interacts with


# First, we will determine if the bee-plant interaction occurs across any study
# We will take the max (0 or 1) value across the 3rd dimension
y.bee.plant <- apply(bee.plant.cite2, c(1, 2), max, na.rm = TRUE)


# Then, we will sum the number of plant species that each bee species interacts with
y.bee.plant <- apply(y.bee.plant, 1, sum, na.rm = TRUE)


# Observed number of plant species that each bee interacts with
obs.dat <- data.frame(obs = y.bee.plant)

# Visualize the data
ggplot(data = obs.dat, aes(obs)) + 
  geom_histogram() 
  xlab("Number of observations")+
    ylab("counts")


  
  
  
# 3. Look at bee size variable------------------------------------------------


  
## Functions for pairs correlation plot
  panel.hist <- function(x, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "skyblue1", ...)
  }
  
  
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = "complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    text(0.5, 0.5, txt, cex = 1.5)
  }
  
  panel.lower <- function(x, y, ...){
    points(x, y, col = rgb(0,0,0, .5), pch = 19)
  }
  
  
  # Pairs correlation plot 

pdf("./Figures/2021_09_13/Bee-size-corr-2021_09_13.pdf")
pairs(bee.size[, 2:7], 
        upper.panel = panel.cor, 
        diag.panel = panel.hist)
dev.off()
  


# Determine the number of NA's
length(which(is.na(bee.size$MaleMinMM) == TRUE))
length(which(is.na(bee.size$MaleMaxMM) == TRUE))
length(which(is.na(bee.size$FemaleMinMM) == TRUE))
length(which(is.na(bee.size$FemaleMaxMM) == TRUE))


# Standardize the bee size
bee.size$FemaleMinMM_std <- (bee.size$FemaleMinMM - mean(bee.size$FemaleMinMM, na.rm = TRUE))/ sd(bee.size$FemaleMinMM, na.rm = TRUE)

# Replace NA with 0 (which is the mean once standardized)
bee.size[which(is.na(bee.size$FemaleMinMM) == TRUE),]$FemaleMinMM_std  <- 0

  




# 4. Format citation type variable------------------------------------------------



#cite.match <- citation[citation[,3] %in% dimnames(bee.plant.cite2)[[3]][6] ,]
#cite.match
#
#str(cite.match)
#
#levels(citation$referenceType)

cite.type <- c("Collection Specimen",
               "Collection Specimen",
               "Observation",
               "Collection Specimen",
               "Literature",
               "Collection Specimen")

# Convert covariate to 0/1
collection <- ifelse(cite.type == "Collection Specimen", 1, 0)
lit <- ifelse(cite.type == "Literature", 1, 0)
obs <- ifelse(cite.type == "Observation", 1, 0)



# 5. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod.txt")
cat("
model{

# Priors
for(i in 1:n.bee){ # For each bee species

# We assign a prior from 0 to 1 for psi

# The occupancy probability = The probability a bee species interacts with a plant species
  alpha_psi[i] ~ dnorm(mu.psi, tau.psi)
    # mu.psi = the average probability a bee species interacts with a plant species
    # tau.psi = the variation of the probability a bee species interacts with a plant species
    # alpha_psi[i] = the probability that bee species i interacts with a plant

}

for(k in 1:n.study){

# The detection probability = the probability a study documented a bee-plant interaction
  alpha_p[k] ~ dnorm(mu.p, tau.p)
    # mu.p = the average probability a study documented a bee-plant interaction
    # tau.p = the variation of the probability a study documented a bee-plant interaction
    # alpha_p[k] = the probability of a bee-plant detection for the kth study
}

mu.psi ~ dnorm(0, 0.01)
mu.p ~ dnorm(0, 0.01)

tau.psi <- 1/(sd.psi * sd.psi)
sd.psi ~ dgamma(0.01, 0.01)

tau.p <- 1/(sd.p * sd.p)
sd.p ~ dgamma(0.01, 0.01)

# Covaraite priors
beta_size_psi ~ dnorm(0, 0.368)
beta_size_p ~ dnorm(0, 0.368)
alpha_col ~ dnorm(0, 0.368)
alpha_lit ~ dnorm(0, 0.368)
alpha_obs ~ dnorm(0, 0.368)



# Model
  # Note the parrelels between the model code here and how we generated the data above

for(i in 1:n.bee){


  for(j in 1:n.plant){
  
    # True bee-plant interaction
    
    z[i, j] ~ dbern(psi[i, j])
 
    logit(psi[i, j]) <- alpha_psi[i] + beta_size_psi * bee.size[i]

      for(k in 1:n.study){

      # Observed bee-plant interaction
      
        y[i,j,k] ~ dbern(p.eff[i,j,k])

        p.eff[i,j,k] <- p[i,j,k] * z[i,j]
        
        logit(p[i,j,k]) <- alpha_p[k] + 
                           alpha_col * collect[k] + 
                           alpha_lit * lit[k] + 
                           alpha_obs * obs[k] + 
                           beta_size_p * bee.size[i]
        
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
  n.bee = dim(bee.plant.cite2)[1],
  n.plant = dim(bee.plant.cite2)[2],
  n.study = dim(bee.plant.cite2)[3],
  bee.size = bee.size$FemaleMinMM_std,
  collect = collection,
  lit = lit,
  obs = obs,
  y = bee.plant.cite2)


# Initial values - to give the model reasonable starting values for the parameters and latent states
zinit <- apply(jags.data$y, c(1, 2), max, na.rm = TRUE) 
zinit[zinit == "-Inf"] <- 0

inits <- function() {list(
  # Latent states
  z = zinit,
  
  # Parameters
  mu.psi = runif(1, -3 , 3),
  sd.psi = runif(1, 0 , 1),
  mu.p = runif(1, -3 , 3),
  sd.p = runif(1, 0 , 1),
  beta_size_psi = runif(1, -3, 3),
  beta_size_p = runif(1, -3, 3),
  alpha_col = runif(1, -3, 3),
  alpha_lit = runif(1, -3, 3),
  alpha_obs = runif(1, -3, 3)
  
)}


# List parameters to monitor
params <- c("mu.psi", "sd.psi",
             "mu.p", "sd.p",

           "beta_size_psi",
           "beta_size_p",
           "alpha_col",
           "alpha_lit",
           "alpha_obs",
           
           "alpha_psi", 
           "alpha_p",
            "z.bee.plant")





# 6. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 10000
na <- 5000
nb <- 2000
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

out$Rhat$alpha_p



# 8. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Look at the traceplots
# These should look like caterpillars on the left
# And on the right it should be a nice smooth (not lumpy) distribution
plot(out)




# 9. Examine model outputs - covaraites ----------------------------------------




dat.cov <- data.frame(names = c("mu.psi", "sd.psi",
                                "mu.p", "sd.p",
                                "beta_size_psi",
                                "beta_size_p",
                                "alpha_col",
                                "alpha_lit",
                                "alpha_obs"),
                      mod.mean = c(out$mean$mu.psi,
                                   out$mean$sd.psi,
                                   out$mean$mu.p,
                                   out$mean$sd.p,
                                   out$mean$beta_size_psi,
                                   out$mean$beta_size_p,
                                   out$mean$alpha_col,
                                   out$mean$alpha_lit,
                                   out$mean$alpha_obs),
                      mod.q2.5 = c(out$q2.5$mu.psi,
                                   out$q2.5$sd.psi,
                                   out$q2.5$mu.p,
                                   out$q2.5$sd.p,
                                   out$q2.5$beta_size_psi,
                                   out$q2.5$beta_size_p,
                                   out$q2.5$alpha_col,
                                   out$q2.5$alpha_lit,
                                   out$q2.5$alpha_obs),
                      mod.q97.5 = c(out$q97.5$mu.psi,
                                    out$q97.5$sd.psi,
                                    out$q97.5$mu.p,
                                    out$q97.5$sd.p,
                                    out$q97.5$beta_size_psi,
                                    out$q97.5$beta_size_p,
                                    out$q97.5$alpha_col,
                                    out$q97.5$alpha_lit,
                                    out$q97.5$alpha_obs))




# Plot the covariates
ggplot(dat.cov, aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean)) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Parameter estimates') +
  xlab("Parameter names") +
  ggtitle("Covariate estimates")+
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

# Save the plot
ggsave("./Figures/2021_09_13/Model_coviates_2021_09_13.pdf", height = 10, width = 8)





# 9. Examine model outputs - number of interaction ----------------------------------------





# Combine the names, truth, and model output
dat <- data.frame(names = c(paste(rownames(bee.plant.cite2), "interact prob"),
                            paste(1:6, "detect prob"),
                            rownames(bee.plant.cite2)), 
                  obs = c(rep(NA, times = nrow(bee.plant.cite2)),
                          rep(NA, times = 6),
                          y.bee.plant), 
                  mod.mean = c( out$mean$alpha_psi,
                                out$mean$alpha_p,
                                out$mean$z.bee.plant), 
                  mod.q2.5 = c(out$q2.5$alpha_psi,
                               out$q2.5$alpha_p,
                               out$q2.5$z.bee.plant), 
                  mod.q97.5 = c(out$q97.5$alpha_psi,
                                out$q97.5$alpha_p,
                                out$q97.5$z.bee.plant))


# Make detection probability the last entry
#dat$names <- factor(dat$names,
#                    levels = c(sort(rownames(bee.plant.cite2)),
#                               "Detection prob"))


## Make the plots

# Plot with probabilities
ggplot(dat[1:(jags.data$n.bee),], aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean)) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Parameter estimates') +
  xlab("Species names") +
  ggtitle("Parameter estimates")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the plot
ggsave("./Figures/2021_09_13/Model_params_2021_09_13.pdf", height = 15, width = 8)


# Object with colors
cols <- c("Observation" = "red", "Estimated" = "black")

# Plot with number of bee-plant interactions
ggplot(dat[c((jags.data$n.bee+7):(jags.data$n.bee * 2)),], 
       aes(x= names, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean, col = "Estimated")) +
  geom_point(size = 3, aes(x = names, y = obs, col = "Observation")) +
  scale_colour_manual("Values", values=cols)+
  geom_hline(yintercept = 0, lty=2) +
  coord_flip() + ylab('Estimated number of plant interactions per bee') +
  xlab("Species names") +
  ggtitle("Number of bee-plant interactions")+
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the plot
ggsave("./Figures/2021_09_13/Num_interacting_2021_09_13.pdf", height = 15, width = 8)




# Plot observations vs. predicted
ggplot(dat[c((jags.data$n.bee+7):(jags.data$n.bee * 2)),], 
       aes(x= obs, y=mod.mean, ymin=mod.q2.5, ymax=mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, lty=2) +
  ylab('Model estimated number of plant interactions per bee') +
  xlab("Observed number of interactions") +
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        plot.title = element_text(size = 25, color = "black", face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Save the plot
ggsave("./Figures/2021_09_13/Mod_v_observed_2021_09_13.pdf", height = 8, width = 8)




# End script
