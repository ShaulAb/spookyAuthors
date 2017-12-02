# Reproducibility ----------------
# library(checkpoint)
# checkpoint("2017-12-01") # YYYY-MM-dd


# Load utility functions and relevant packages -----
#devtools::install_github("dkahle/ggmap")
pacman::p_load(dplyr, ggplot2, quanteda, typeless, readr, reshape2, stringr,
               magrittr, tidytext, glue, stringr, rworldmap, ggmap)

# pacman::p_load_gh("trinker/entity")
Smisc::sourceDir("code/utilities")



# read the data files ---------------
train <- read_csv("data/raw/train.csv")
test  <- read_csv("data/raw/test.csv") 

# structure overview
glimpse(train)
glimpse(test)

# dependent variable
count(train, author)



# minor cleaning up ---------------

# unnest the tokens (sentences) into words
train_un <- parseText(train)

# useful msg
msg <- '\nThe train set has {nrow(train)} sentences, {nrow(train_un)} words'
glue(msg)

# unless I is appearent, other words of length 1 will be ommited
train_un %<>% 
  mutate(len = nchar(word),
         one_char = if_else(len == 1, 1, 0)) %>%
  filter(!(one_char == 1 & !word %in% c("i", "I"))) %>% 
  select(-one_char)

# minding the filtration effect
pre <- 'Filtered words of length 1, excluding I:'
glue(paste(pre, msg))  

# word stemming 
train_un %<>% mutate(stemmed = char_wordstem(word))

# eport the unnested train set
# write_csv(train_un, "data/preprocessed/tr_unnested.csv")



# Length comparison ---------------

# Exploring word length per author
train_un %>% 
  group_by(author) %>% 
  summarise(word_mean = mean(len),
            word_median = median(len),
            words_std = sd(len))

# plotting the distribution with boxplot
train_un %>% 
  ggplot(aes(x = author, y = len, fill = author)) +
  labs(title = "Word length comparison - boxplot", 
       y = "# chars") +
  geom_boxplot() +
  theme_bw()

# same comparison on the stemmed words
train_un %>% 
  mutate(len_stem = nchar(stemmed)) %>% 
  group_by(author) %>% 
  summarise(word_mean = mean(len_stem),
            word_median = median(len_stem),
            words_std = sd(len_stem))

# Exploring sentence length per author
train %>%
  mutate(len = str_length(text)) %>% 
  group_by(author) %>% 
  summarise(sen_mean = mean(len),
            sen_median = median(len),
            sen_std = sd(len))
  
# a pattern? let's see the density plot
train %>%
  mutate(len = str_length(text)) %>% 
  ggplot(aes(x = len, fill = author)) +
  geom_density(alpha = .6) +
  scale_x_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  theme_bw()



# Stop words analysis -------------

# extract all stop words
stop_words <- 
  train_un %>% 
  filter(word %in% gsub("\\S+'\\S+", "", stopwords("english")))

# overall words for each author
words_per_author <- 
  train_un %>% 
  group_by(author) %>% 
  summarise(words = n())

# save bp of all stopwords
# write_csv(stop_words, "data/preprocessed/stop_words.csv")

# stop words per author
frequency <-
  stop_words %>%
  count(author, word)

# normalize by the total number of words per author
frequency <- left_join(frequency, words_per_author)
frequency %<>%
  mutate(ratio = n/words)

# plot the most proportionally frequent stop words per author
frequency %>%
  arrange(desc(ratio)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(30, ratio) %>%
  ggplot(aes(word, ratio, fill = author)) +
  geom_col() +
  geom_label(aes(label = scales::percent_format()(ratio)), hjust = .5) +
  facet_wrap(~author) +
  labs(x = NULL, y = "Normalized Frequency") +
  coord_flip() +
  theme_bw()



# bigrams ----------------

# parse train to bigrams  
train_un_2 <- parseText(df = train, ng = 2L)

# stats
msg2 <- "\nThe train set has {nrow(train)} sentences, {nrow(train_un_2)} bigrams"
glue(msg2)

# perform word stemming: separate, stem, unite
  bi_sep <- # separate
    train_un_2 %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bi_sep %<>% # stem
    mutate(stem1 = char_wordstem(word1),
           stem2 = char_wordstem(word2))
  
  train_un_2 <- # unite 
    bi_sep %>% 
    unite(bigram, stem1, stem2, sep = " ")

# export preprocessed bigrams snapshot
# write_csv(train_un_2, "data/preprocessed/bigrams_stemmed.csv")

# tf-idf on bigrams
bi_tf_idf <- 
  train_un_2 %>%
  count(author, bigram) %>%
  bind_tf_idf(bigram, author, n) %>%
  arrange(desc(tf_idf))



# Named entity recognition -----------

# load locations data
locs <- read_csv("data/preprocessed/entities/location.csv")

# locations cleanup
locs %<>% # remove words of length 1,2
  mutate(len = nchar(Locations)) %>% 
  filter(len > 2)

# manual clean up...
placesToRemove <- 
  c("West", "Broad", "U .", "New", "Evil", "Egyptian", "Cruelty", "Latin", 
    "Bay", "Hope", "Ma'm", "King Arthur", "Heraclides", "Richmond", "Eton",
    "Mr.", "Prince de", "Oh", "Devil", "How", "King", "S.C.", "Des", 
    "Their", "Sun", "Oliver", "Elizabeth", "Brook", "Juliet", "Head",
    "Smug.", "Old", "Madame", "Prince", "Leonardo", "Idris", "Love",
    "Nice", "Adams", "St.", "Adrian", "Mr. L I.", "From", 
    "III Health", "Nature", "Mr, L I", "Joe", "Dr.",
    "Hill", "Perdita", "Hi Hi Randy", "Suliivan", "April",
    "Great", "Lords", "Hastings", "James River", "Little",
    "Raphael", "Fall", "Hell", "How")

locs %<>%
  filter(!Locations %in% placesToRemove)

# prepare regex patterns
toMatch <- paste(as.vector(locs$Locations), collapse = "|")

# find the locations in the text
rows <- grep(toMatch, x = train$text)

# location sentences
loc_txt <- train[rows, c("author", "text")]

# output the locations list in a new distinct column
loc_txt %<>%
  mutate(locs = str_extract_all(text, toMatch))

# divide locations by authors
loc_MWS <- extractLoc(df = loc_txt, auth = "MWS")
loc_EAP <- extractLoc(df = loc_txt, auth = "EAP")
loc_HPL <- extractLoc(df = loc_txt, auth = "HPL")

# geocodings
mws_geocode <- batchGeocode(loc_MWS)
eap_geocode <- batchGeocode(loc_EAP)
hpl_geocode <- batchGeocode(loc_HPL)

# export the geocodes
# write_csv(mws_geocode, "data/preprocessed/locations/geocodes/mws.csv")
# write_csv(eap_geocode, "data/preprocessed/locations/geocodes/eap.csv")
# write_csv(hpl_geocode, "data/preprocessed/locations/geocodes/hpl.csv")

# plot the locations according to author
newmap <- getMap(resolution = "high")

plot(newmap, asp = 1)

points(mws_geocode$lon, 
       mws_geocode$lat, 
       col = "red", cex = 1.1, pch = 19)

points(eap_geocode$lon, 
       eap_geocode$lat, 
       col = "blue", cex = 1.1, pch = 19)

points(hpl_geocode$lon, 
       hpl_geocode$lat, 
       col = "yellow", cex = 1.1, pch = 19)


# Topic modelling ---------------

# to be continued...