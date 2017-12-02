####### Named Entity Recognition #########

# Load utility functions and relevant packages -----
pacman::p_load(dplyr, ggplot2, quanteda, readr, entity,
               magrittr, tidytext, stringr)

# stanford's NER java models will be used to tag
# person and location entities

# fetch data corpus
tr_corp <- read_csv("data/raw/train.csv") # train
ts_corp <- read_csv("data/raw/test.csv")  # test

# combine train and test to single corpus
tr_corp %<>% select(text)
ts_corp %<>% select(text)
corp <- rbind(tr_corp, ts_corp)

# person entities tagging
ppl_names <- person_entity(corp$text)
df_ppl <- data.frame(People = unique(unlist(ppl_names)), stringsAsFactors = FALSE)

# location entities tagging
loc_names <- location_entity(corp$text)
df_loc <- data.frame(Locations = unique(unlist(loc_names)), stringsAsFactors = FALSE)

# export the files
write_csv(df_ppl, "data/preprocessed/entities/person.csv")
write_csv(df_loc, "data/preprocessed/entities/location.csv")
