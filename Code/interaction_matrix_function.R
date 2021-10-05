### Interaction Matrix Function ###
# created 29 jan 2021 by M. Lee
# code adapted from bipartite function frame2webs author: Jochen Fruend

# the purpose of this function is to take an interaction dataframe and convert it to an interaction list or array
# I have adapted this code so it does not need a "webID" that is part of the original code

df2intmatrix <- function(dframe, varnames = c("lower", "higher", "freq"), type.out = "list", emptylist = TRUE) {
if (length(varnames)==3) {
  if (any(is.na(dframe[,varnames[3]]))) warning(paste("NAs in", varnames[3], "converted to 0"))
  webarray <- tapply(dframe[,varnames[3]],dframe[,varnames[1:2]], sum)
}
if (length(varnames)==2) webarray <- tapply(rep(1,nrow(dframe)),dframe[,varnames[1:2]], sum)
webarray[is.na(webarray)] <- 0   # needs to be done when using tapply: unobserved combinations always get a zero, even with na.rm=T
if (type.out=="array") return(webarray)
if (type.out=="list") {
  weblist <- list()
  #for (i in dimnames(webarray)[[3]]) weblist[[i]] <- webarray[,,i]
  #if (emptylist) weblist <- lapply(weblist,empty)
  #return(weblist)
}
}

#interaction_df <- read_csv("2019HandCollections_IDs.csv")
#int_matrix <- df2intmatrix(interaction_df, varnames = c("plant_id", "official_inv_ID"), type.out = "array")

### Compare the output here to a the output from bipartite frame2webs function

#networkdf_2019 <- interaction_df %>% 
 # mutate(Palmyra = " ")
#int_matrix2 <- as.data.frame(frame2webs(networkdf_2019, varnames = c("plant_id", "official_inv_ID", "Palmyra"), type.out = "array"))

### Output is identical!


