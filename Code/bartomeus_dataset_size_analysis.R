### Using network data to understand occurrence ###
# this code was created by M. Lee | 4 Feb 2021




### Questions ###
# In site-intensive data collection -- what might determine the likelihood of capturing certain species?
# How does bee size potentially impact occurrence in more full network?



### This code will do the following ---------------

# 1. download a network dataset from location-based datasets

# 2. add bee size based on genera data compiled by Y. Diao

# 3. check relationship between freq bee is seen and size




### Libraries and set working directory --------------

library(tidyverse)
setwd("~/Desktop/globi_bees/Data")




### call datasets ----------------------------------------

# body length dataset
size <- read_csv("body_length_jan21.csv")
str(size)


# occurrence dataset used in Baromeus et al. 2013
interactions <- read_csv("bartomeus_2013_occurence.csv")
str(interactions)
unique(interactions$insect_genus) # 41 unique bee genera

### clean and merge datasets ------------------------------------
size <- size %>% 
  rename(insect_genus = genera) %>% 
  select(insect_genus, species, family, total_av, av_lower_mm, av_upper_mm)


# size and occurrence
int_size <- merge(interactions, size, by = "insect_genus")
unique(int_size$insect_genus) # down to 15 unique bee genera


### look over data ------------------------------------

# for all bee genera size list -- what is the spread of sizes?
ggplot(size, aes(x = total_av)) +
  geom_histogram() # tri-modal? has longer tails

# for Bartomeus paper -- what is the spread of sizes?
ggplot(int_size, aes(x = total_av)) +
  geom_histogram(bins = 5) # a little less variation than the whole dataset

# count of the number of times the genus appears in the Bartomeus datset
# does not include actual frequency (which is its own column in dataset)
ggplot(int_size, aes(x = insect_genus)) +
  geom_bar()

# count of times the genus appears in Bartomeus dataset by size
ggplot(int_size, aes(x = total_av)) +
  geom_histogram()




### look at relationship between size and appearance ----------

# frequency is the number of times a bee is seen at a site, on a day
# doesn't have zero values, but could be assumed here
ggplot(int_size, aes(x = total_av, y = Freq)) +
  geom_point(position = "jitter") +
  xlab("bee size (mm)") +
  ylab("observations per period") +
  geom_smooth(method = "lm")



### num times genus appears in dataset and size ---------------------


# now looking at the number of times the pollinator appears in the dataset
int_summ <- int_size %>% 
  group_by(insect_genus) %>% 
  summarise(times_in_dataset = length(insect_genus))

# merge with size data
int_size_summ <- merge(int_summ, size, by = "insect_genus")

# look at relationship between size and number of times genus is in dataset
ggplot(int_size_summ, aes(x = total_av, y = times_in_dataset)) +
  geom_point(position = "jitter") +
  xlab("bee size (mm)") +
  ylab("rows in dataset") +
  geom_smooth(method = "lm")







