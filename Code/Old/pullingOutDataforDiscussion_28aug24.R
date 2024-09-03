# checking bee size covariate for discussion =====

size <- read_csv("./Data/Bee traits for checklist species - size.csv")

# ran through workflow #3 to get this file below
bee.size.std <- bee.covariates


avbeesize <- bee.size.std %>% 
  filter(size_std >= -0.1 & size_std <= 0.1)
# about 8.5 mm


bigbeesize <- bee.size.std %>% 
  filter(size_std > 0.1)
# 60 bees
max(bigbeesize$size) #16.5mm
over10 <- bigbeesize %>% 
  filter(size > 10) #30 bees over 1 cm


smallbeesize <- bee.size.std %>% 
  filter(size_std < 0.1)
# 77 bees

# check flower color covariate =====

# from the bottom of workflow #3
flowercolorcheck <- bee_plant_sum %>% 
  arrange(desc(number_bees_per_plant)) %>% 
  filter(color == "blue")












