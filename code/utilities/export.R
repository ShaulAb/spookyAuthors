tagExport <- function(objs, tags) {
  
  # combine it to a single structure
  combined <- lapply(objs, unlist)
  
  # export each list to a separate file
  for (tag in tags){
    obj <- 
      combined[[tag]] %>% 
      unique %>% 
      adf
    write_csv(obj, paste0("data/annotated/", tag, ".csv"))
  }
}
