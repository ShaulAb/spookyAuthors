# extract per author locations
extractLoc <- function(df, auth) {
  ################################
  # input: list with locations   #
  # per author and author name   #
  #                              #
  # output: all unique locations #
  # used by the author           #
  ################################
  
  df %>%
    select(author, locs) %>% 
    filter(author == auth) %>% 
    unlist(locs) %>% 
    unique()
}


# geocode list of locations
batchGeocode <- function(ls) {
  ######################################
  # input: list of the locations       #
  #                                    #
  # output: the geocodes are returned  #
  # as a data.frame with 2 columns:    #
  # longitude and latitude             #
  ######################################
  
  # divide up the locations list into 10 subsets
  n  <- length(ls)
  sq <- round(seq(1,n, length.out = 10))
  
  # init an empty list to contain the geocodes
  geocodes <- data.frame(lon = 0, lat = 0)
  
  # loop through the subsets, with short sleep in-between 
  for (i in 1:9) {
    codes <- geocode(ls[sq[i]:sq[i + 1]])
    Sys.sleep(20) # avoid query load
    geocodes <- bind_rows(geocodes, codes)
  }
  
  # all done - return
  return(geocodes[-1,])
}
