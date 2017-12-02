# create corpuses for each author
getCorpus <- function(df, auth) {
  #############################
  # input: train set & author #
  #                           #
  # output: corpus of text    #
  # for the specified author  #
  #############################
  df %>% 
    filter(author == auth) %>% 
    select(text) %>% 
    corpus()
}


# parsing sentences data into 1,2 or 3-grams 
parseText <- function(df, ng = 1L) {
  #####################################
  # input: data with author and text  #
  # columns, ng to indicate n-grams   #
  #                                   #
  # output: the parsed data according #
  # to specified n-grams              #
  #####################################
  
  # case 1 gram
  if (ng == 1) {
    ret <- 
      df %>%
      select(author, text) %>% 
      unnest_tokens(word, text)  
  } 
  # case bigram
  if (ng == 2) {
    ret <- 
      df %>%
      select(author, text) %>%
      unnest_tokens(bigram, text, token = "ngrams", n = ng)  
  }
  # case trigram
  if (ng == 3) {
    ret <- 
      df %>%
      select(author, text) %>%
      unnest_tokens(trigram, text, token = "ngrams", n = ng)  
  }
  
  return(ret)
  
}
