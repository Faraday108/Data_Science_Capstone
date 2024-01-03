
library(tidyr)
library(readr)
library(stringr)
library(tokenizers)
library(dplyr)
library(gt)

# Count lines in file
# R.utils::countLines(con)

blogs_path <- "final/en_US/en_US.blogs.txt"
news_path <- "final/en_US/en_US.news.txt"
twitter_path <- "final/en_US/en_US.twitter.txt"

sample_corpus <- function(path, n = 100000) { # helper to sample n times from file
  file_length <- R.utils::countLines(con <- file(path, "rb")); close(con)
  data <- readLines(con <- file(path, "rb"))[sample(1:file_length, n, replace = FALSE)]; close(con)
  data
}

read_corpus <- function(path) { # helper to read all lines from file
  data <- readLines(con <- file(path, "rb"))
  close(con)
  data
}
  
#### Cleaning 
clean_profanity <- function(data) { # helper to remove profanity from data
  if (!file.exists("swearWords.txt")) {
    download.file("http://www.bannedwordlist.com/lists/swearWords.txt", "swearWords.txt")
  }
  swear_words <- read_csv("swearWords.txt", col_names = FALSE, show_col_types = FALSE)
  
  word_match_list = function(...) {
    words = c(...)
    word_options = paste(words, collapse = "|") # combine the words w/ | between them
    paste0('\\b(?:', word_options, ')\\b') 
    # add extra regex formatting that makes it work
  }
  
  word_list_regex <-  word_match_list(swear_words$X1)
  str_remove_all(data, word_list_regex)
}

clean_corpus <- function(data) { # final helper to clean data. 
  # Step 1: keep only alpha and lowercase words. No numbers or punctuation. 
  gsub('[^[:alpha:][:space:]\']', '', data) %>%
    tolower() %>%
    clean_profanity()
  # Step 2: remove profanity
  
}

#### Tokenize
# tokenized_words <- tokenize_words(data)
# word_data <- data.frame(words = unlist(tokenized_words))
# monogram <- word_data %>% 
#   count(words, sort = TRUE)

generate_ngram <- function(data, n = 1) {
  tokenized_data <- tokenize_ngrams(data, n = n)
  tokenized_df <- data.frame(words = unlist(tokenized_data))
  tokenized_df %>% 
    count(words, sort = TRUE)
}

##### CORPUS SAMPLE
# if (!file.exists("data/data.csv")) {
#   data_blogs <- sample_corpus(blogs_path)
#   data_news <- sample_corpus(news_path)
#   data_twitter <- sample_corpus(twitter_path)
#   data <- c(data_blogs, data_news, data_twitter)
#   write_csv(data.frame(lines = data), "data/data.csv")
# }

# data <- read_csv("data/data.csv", show_col_types = FALSE)

blogs_data <- read_corpus(blogs_path) %>%
  clean_corpus()
news_data <- read_corpus(news_path) %>%
  clean_corpus()
twitter_data <- read_corpus(twitter_path) %>%
  clean_corpus()

data <- read_csv("data/data.csv")

data <- c(blogs_data, news_data, twitter_data); rm(blogs_data, news_data, twitter_data)

if(!file.exists("data/monogram.csv")) {
  monogram <- generate_ngram(data)
  monogram <- monogram[which(monogram$n > 1),]
  write_csv(monogram, "data/monogram.csv")
  rm(monogram)
}
if(!file.exists("data/bigram.csv")) {
  bigram <- generate_ngram(data, n = 2)
  bigram <- bigram[which(bigram$n > 1),]
  write_csv(bigram, "data/bigram.csv")
  rm(bigram)
}
if(!file.exists("data/trigram.csv")) {
  trigram <- generate_ngram(data, n = 3)
  trigram <- trigram[which(trigram$n > 1),]
  write_csv(trigram, "data/trigram.csv")
  rm(trigram)
}
if(!file.exists("data/quadgram.csv")) {
  quadgram <- generate_ngram(data, n = 4)
  quadgram <- quadgram[which(quadgram$n > 1),]
  write_csv(quadgram, "data/quadgram.csv")
  rm(quadgram)
}
