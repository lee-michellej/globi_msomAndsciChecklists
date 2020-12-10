########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: grace.direnzo@gmail.com
########################################




# Code objective: To determine the number of parasites on each bee species while accounting for sampling bias

# To do this, we will analyze the globi data using an N-mixture model



########################################
########################################
########################################


# Model assumptions:
# (1)

# (2)

# (3)

# (4)


########################################
######## Table of Contents #############
########################################


# 1. Load libraries & set working directory
# 6. Write the model in JAGS
# 7. Bundle the data
# 8. MCMC settings
# 9. Run the models 
# 10. Look at model outputs
# 11. Compare model outputs to truth


########################################
########################################
########################################





# 1. Load libraries & set working directory -------------------------------------------------------

# Load libraries
library(jagsUI)
library(ggplot2)


# Set working directory
setwd("~/globi_tritrophic_networks/")



# 2. Load data -------------------------------------------------------


# Read in data
load("./Data/globi_data_formatted.rds")





# 3. Write the model in JAGS ------------------------------------------------



sink("./Model/globi_mod_count.txt")
cat("
model{

# Priors
# Average parasite load across bees
lambda ~ dnorm(0, 0.01)


# Probability of detecting a parasite on a bee in each study
p ~ dunif(0, 1)


# Model
for(i in 1:n.bee){   # For each bee species


# N is the true number of parasites that occur on bee species i
# lambda is the average number of parasites across all bee species
N[i] ~ dpois(lambda.eff[i])

log(lambda.eff[i]) <- lambda

for(k in 1:n.study){

    # y = our count data for the number of parasites on each bee species i found by each study k
    # p = the probability that each study detected a parasite-bee interaction
    y[i,k] ~ dbinom(p, N[i])

}

}
    


}
",fill=TRUE)
sink()





# 3. Bundle the data ------------------------------------------------



# Remove crazy column names
colnames(globi.dat) <- 1:ncol(globi.dat)

# Write the file
#write.csv(globi.dat, file = "./Data/globi_formatted_data_2020_11_13.csv")

# Convert globi data to matrix
globi.dat1 <- matrix(as.numeric(unlist(globi.dat[,-1])), 
                    nrow = nrow(globi.dat),
                    ncol = ncol(globi.dat)-1,
                    byrow = FALSE)


# Put all the data together
jags.data <- list(
  n.bee = dim(globi.dat1)[1],
  n.study = dim(globi.dat1)[2],
  y = globi.dat1)



# Initial values
Ninit <- apply(jags.data$y, 1, max, na.rm = TRUE)

inits <- function() {list(
  N = Ninit,
  lambda = runif(1, min = -2, max = 0),
  p = runif(1, min = 0, max = 1)
)}


# List parameters to monitor
params <- c("lambda", "p", "N")




# 8. MCMC settings --------------------------------------------------------




# MCMC settings
ni <- 30000
na <- 10000
nb <- 5000
nt <- 10
nc <- 3




# 9. Run the models --------------------------------------------------------





# Run the model
out <- jags(data = jags.data, 
            inits = inits, 
            parameters.to.save = params, 
            model = "./Model/globi_mod_count.txt", 
            n.chains = nc, 
            n.thin = nt, 
            n.iter = ni, 
            n.burnin = nb, 
            n.adapt = na,
            parallel = TRUE)


# Beep when done
beepr::beep(2)




# 10. Look at model outputs -------------------------------------------------





# Make sure all parameters have converged
print(out)


# Make sure all parameters have converged
plot(out)


# Save the output
save(out, file = "./ModelOutput/out_2020_11_06.rda")


# 11. Compare model outputs to truth ----------------------------------------




# We want to visulaize the model estimates 


# Using the detection-nondetection data
# Combine the names, truth, and model output
mod.est <- data.frame(
  names = 1:jags.data$n.bee, 
  mod.mean = c( out$mean$N), 
  mod.q2.5 = c(out$q2.5$N), 
  mod.q97.5 = c(out$q97.5$N),
  obs = apply(jags.data$y, 1, max, na.rm = TRUE))



# Set colors for truth and estimates
cols <- c("Estimated" = "black", "Observed" = "red")


## Make the plots
# Plot the number of parasites per bee species
 ggplot(mod.est, aes(x = names, y = mod.mean, ymin = mod.q2.5, ymax = mod.q97.5))+ 
  geom_linerange(size = 1) +
  geom_point(size = 3, aes(x = names, y = mod.mean, col = "Estimated")) +
   geom_point(size = 3, aes(x = names, y = obs, col = "Observed")) +
   scale_colour_manual("Values", values=cols)+
  coord_flip() + 
  ylab('Estimated parasite load') +
  xlab("Bee species") +
  ggtitle("")+
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


# Save the output
ggsave("./Figures/Globi_data_count.pdf", width = 10, height = 12)


# Plot the number of parasites per bee species
ggplot(mod.est, aes(x = obs, y = mod.mean, ymin = mod.q2.5, ymax = mod.q97.5))+ 
  geom_pointrange(position = position_jitter()) + 
  geom_abline(slope = 1, intercept = 0, lty = 2)+
  scale_colour_manual("Values", values=cols)+
  ylab('Estimated parasite load') +
  xlab("Observed parasite load") +
  ggtitle("")+
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

# Save the output
ggsave("./Figures/Globi_data_estimate_v_observed.pdf", width = 13, height = 12)



# Stack the data
hist.dat <- data.frame(
    Estimate = rep(c("observed", "estimate"), each = length(mod.est$obs)),
    value = c(mod.est$obs, 
    mod.est$mod.mean))

# Make histograms

ggplot(hist.dat, aes(x = value,  fill = Estimate))+
  geom_histogram(aes(y=..density..), col = "black")+
  geom_density(alpha=.2)+
  ylab("Density")+
  xlab("Number of parasite species")+
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


# Save the output
ggsave("./Figures/Globi_data_histogram.pdf", width = 13, height = 12)


# End script