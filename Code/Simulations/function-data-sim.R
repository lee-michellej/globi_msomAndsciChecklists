########################################
########################################
# This code was written by: G. V. DiRenzo
# If you have any questions, please email: gdirenzo@umass.edu
########################################
########################################



########################################
####### Code Objective #################
########################################

# Objective: 
  # To write the function to simulate species detection/non-detection 


########################################
####### Code Output ####################
########################################


# This code generates 2 functions:
  # make.range()
    # This function simulates bee species plant ranges (i.e., the range of plant species that each bee interacts with)
  # make.data()
    # This function simulates bee species detection/non-detection data


###########################
###### Notes ##############
###########################




########################################
######## Table of Contents #############
########################################

# 1. Define range function
# 2. Define data function

###############################
###############################
###############################



# Call in libraries
library(tidyverse)


# 1. Define range function ------------------------------------------------



## Define the function to simulate 2 scenarios for bee-plant ranges:

  ## Scenario 1: All bee-plant interactions are equally likely (type.range == "equal")
    ## In this case, we are use the sample() function to determine the bee-plant interactions

  ## Scenario 2: Bee species are restricted to interacting with a subset of plant species. This can occur for a variety of reasons in nature (e.g., differing phenology, morphology, etc.). 
    ## In this case, we use a log-normal distribution to determine which plant species a bee could possibly interact with using function rbeta()


# Define function to determine plant ranges of bees
make.ranges <- function(n.bee.sp,          # Number of bee species
                        n.plant.sp,        # Number of plant species
                        type.range) { # type.range can == "equal" or "logn"
  
  # If type.range == "equal" then do this:
  if(type.range == 'equal') {
    
    #  take a sample of the specified size from the elements of x using either with or without replacement
    plant.by.bee <- sample.int(n=n.plant.sp, 
                               size= n.bee.sp, 
                               replace=TRUE)
    
  # If type.range == "logn" then do this:
  } else if(type.range == 'logn') {
    
    # Draw n.bee.sp values from a beta distribution with shape1 = 1 and shape2 = 3 (corresponds to a log normal)
    prop.plants <- rbeta(n.bee.sp, shape1 = 1, shape2 = 3)
    
    # Round the prop.plants values
      # plant.by.bee object = the number of plant species that each bee species interacts with
    plant.by.bee <- round(n.plant.sp * prop.plants)
    
  }
  
  # Function to extract which plant species each bee interacts with
  get.plants.within.range <- function(ii) {
    
    plants <- rep(0, n.plant.sp)
    
    plants[sample(x=1:n.plant.sp, size=ii, replace=FALSE)] <- 1
    
    plants
  }
  
  # Apply the get.plants.within.range() function to the plant.by.bee object
  res <- t(sapply(plant.by.bee, get.plants.within.range))
  
  # Rename the dimensions
  names(dim(res)) <- c('n.bee.sp','n.plant.sp')
  
  # Convert to logical ("TRUE" or "FALSE")
  res==1
  
}





# 2. Define data function ------------------------------------------------



## Define the function to simulate bee-plant detection/non-detection


## simulate data-set

# Definitions-
## n.bee.sp: number of bee species
## n.plant.sp: number of plant species that bee species might actually occupy
## n.citation: number of citations

# Occupancy parameters- 
## mu.psi:
## sigma.psi.sp:

# Detection parameters- 
## mu.p:
## sigma.p.sp:
## sigma.p.cite:

# Visitation parameters- 
## mu.v:
## mu.v.yr: 


n.bee.sp=5
n.plant.sp=10
n.citation=2

# Define model parameters for:
# Occupancy
mu.psi=0
sigma.psi.sp=0.1

# Detection
mu.p=0
sigma.p.sp=0.1
sigma.p.cite=0.1

# Visitation
mu.v=-0.5
mu.v.yr=-0.5

# Bee species range info
type.range='equal'
missing.visits=FALSE
sp.range=NULL


make.data <- function(# Define number of species & citations
                      n.bee.sp=25,
                      n.plant.sp=100,
                      n.citation=5,
                      
                      # Define model parameters for:
                      # Occupancy
                      mu.psi=0,
                      sigma.psi.sp=0.1,
                      
                      # Detection
                      mu.p=0,
                      sigma.p.sp=0.1,
                      sigma.p.cite=0.1,
                      
                      # Visitation
                      mu.v=-0.5,
                      mu.v.yr=-0.5,
                      
                      # Bee species range info
                      type.range='equal',
                      missing.visits=FALSE,
                      sp.range=NULL){
  
  ## ------------------------------------------------------------
  ## If species' ranges are not passed in, simulate them for the
  ## specified number of species and sites.
  if(is.null(sp.range)) {
    sp.range <- make.ranges(n.bee.sp=n.bee.sp,
                            n.plant.sp=n.plant.sp,
                            type.range=type.range)
  }
  ## ------------------------------------------------------------
  
  ## ------------------------------------------------------------
  ## specify species-specific occupancy and detection probabilities
  ## and also site visit probabilities
  
  ## species-specific random intercepts
    ## Logit scale
  psi.sp <- rnorm(n=n.bee.sp, mean=0, sd=sigma.psi.sp)
  p.sp   <- rnorm(n=n.bee.sp, mean=0, sd=sigma.p.sp)
  
  ## effect of citation on detection
    ## Logit scale
  p.cite <- matrix(rnorm(n=n.plant.sp*n.citation, 
                         mean=0, sd=sigma.p.cite),
                   nrow=n.plant.sp,
                   ncol=n.citation)
  
  ## create empty occupancy, detection, and visitation probability
  ## matrices
  # Occupancy
  psi.mat <- array(NA, dim=c(n.bee.sp=n.bee.sp,
                             n.plant.sp=n.plant.sp))
  # Detection
  p.mat <- array(NA, dim=c(n.bee.sp=n.bee.sp,
                           n.plant.sp=n.plant.sp,
                           n.citation=n.citation))
  # Visitation
  v.mat <- array(NA, dim=c(n.plant.sp=n.plant.sp,
                           n.citation=n.citation))
  
  ## Fill in these matrices with the probabilities
  for(plant in 1:n.plant.sp) {
    
      for(bee in 1:n.bee.sp) {
        psi.mat[bee,plant] <- plogis(mu.psi +
                                  psi.sp[bee] )
      }
    
      for(citation in 1:n.citation) {
        v.mat[plant,citation] <- plogis(mu.v)
        
        for(bee in 1:n.bee.sp) {
          p.mat[bee,plant,citation] <- plogis(mu.p +
                                             p.sp[bee] +
                                             p.cite[bee,citation] )
        }
      }
    }


  ## ------------------------------------------------------------
  
  ## ------------------------------------------------------------
  ## Create range, occupancy, detectability, visitation matrices
  
  ## First, we will construct a range matrix (TRUE for all plants
  ## in range of bee species and FALSE for all plants outside of range).  
  range.arr <- array(sp.range, dim=c(n.bee.sp=n.bee.sp,
                                     n.plant.sp=n.plant.sp))
  
# True bee species interactions by plant species
  occ.arr <- array(rbinom(n=length(psi.mat),
                          size=1,
                          prob=psi.mat),
                   dim=c(n.bee.sp=n.bee.sp,
                         n.plant.sp=n.plant.sp))
  

## subset occ.arr down only to plant species within each bee species' range
  occ.arr <- occ.arr*range.arr

    ## Visualize this to confirm - no points occur where occ.arr == 1 and range.arr == 0
    ##   plot(c(occ.arr), c(range.arr))

    
# Now- determine detections
  
  ## Create empty detections array
  det.arr <- array(NA, dim = c(n.bee.sp, n.plant.sp, n.citation))
  
for(plant in 1:n.plant.sp) {
    for(bee in 1:n.bee.sp) {
     for(citation in 1:n.citation) {
       
       det.arr[bee, plant, citation] <- rbinom(n=1,
                                               size=1,
                                               prob=p.mat[bee, plant, citation] *
                                                 occ.arr[bee, plant])
       
    }
  }
}
  ## Visualize the detection data vs the occupancy data
    ## There should be no points where occ.arr == 0 and det.arr == 1
  ## plot(c(apply(det.arr, c(1, 2), max)), c(occ.arr))
  
  ## Collapse the detection array across 1st dimension (bee dimension)
    ## what remains are the plant x citation
  det.plant.cite <- apply(det.arr, c(2, 3), max)
  
  ## Create empty vistiation array
  vis.arr <- array(NA, dim = c(n.plant.sp, n.citation))
  
  for(plant in 1:n.plant.sp) {
    
    for(citation in 1:n.citation) {
      
      vis.arr[plant, citation] <- rbinom(n=1,
                                         size=1,
                                         prob=v.mat[plant, citation])
      
    }
  }
  
  ## subset actual detections by incorporating visits
  if(missing.visits) {
    
    # transpose the array to repeat values for each species - done in 2 steps
  ## step 1. Repeat vis.arr n.bee.sp times 
  vis.arr2 <- array(vis.arr, 
                    dim=dim(det.arr)[c(2:3,1)]) # this moves the 1st dimension (bee ID) to last dimension
  
  ## step 2. transpose the array
    # from: n.plant.sp x n.citation x n.bee.sp
    # to: n.bee.sp x n.plant.sp x n.citation
    vis.arr.with.sp <- aperm(vis.arr2, c(3,1:2))
    
    # Multiply the detections by the visitation array
    det.arr <- det.arr * vis.arr.with.sp
    
  }

  ## ------------------------------------------------------------
  
  ## ------------------------------------------------------------
  ## create objects to return

  ## add dimension names to arrays
  arr.names <- list(bee=paste('bee',str_pad(1:n.bee.sp,3,pad='0'),sep='_'),
                    plant=paste('plant',str_pad(1:n.plant.sp,3,pad='0'),sep='_'),
                    cite=paste('citation',1:n.citation,sep='_'))
  
  if(is.null(sp.range) == FALSE){
  dimnames(sp.range) <- arr.names[1:2]
  }
  dimnames(occ.arr)  <- arr.names[1:2]
  dimnames(det.arr)  <- arr.names[1:3]
  dimnames(vis.arr)  <- arr.names[2:3]
  
  list(# Simulated data (truth, range, observed, visits)
       sp.range=sp.range,
       occ.arr = occ.arr,
       det.arr = det.arr,
       vis.arr=vis.arr,

       # Study design
       n.bee.sp=n.bee.sp,
       n.plant.sp=n.plant.sp,
       n.citation=n.citation,

       # Occupancy parameters
       mu.psi=mu.psi,
       sigma.psi.sp=sigma.psi.sp,

       # Detection parameters
       mu.p=mu.p,
       sigma.p.cite=sigma.p.cite,
       
       # Visitation parameters
       mu.v=mu.v,
       
       # Random effects values
       psi.sp=psi.sp,
       p.sp=p.sp,
       p.cite=p.cite)
  ## ------------------------------------------------------------
}

