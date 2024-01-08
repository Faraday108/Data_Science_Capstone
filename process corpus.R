
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

data <- read_csv("data/data.csv", show_col_types = FALSE)

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
  quadgram <- data.frame()
  tst <- split(data, sample(1:40, nrow(data), replace = TRUE))
  pb <-  txtProgressBar(min = 0, max = 40, initial = 0)
  for (i in 1:40) {
    fname <- paste0("data/quadgram", i, ".csv")
    write_csv(generate_ngram(tst[[i]]$lines, n = 4), fname)
    setTxtProgressBar(pb,i)
    # 
    # quadgram <- rbind(quadgram, generate_ngram(tst[[i]]$lines, n = 4)) %>%
    #   count(words, wt = n, sort = TRUE)
  }
  close(pb)
  # Combine files: proved to be too slow and memory intensive for my laptop.
  # fnames <- paste0("data/quadgram", c(1:40), ".csv")
  # for (i in fnames) {
  #   quadgram <- rbind(quadgram, read_csv(i, show_col_types = FALSE))
  #   cat("=")
  #   file.remove(i)
  # }
  # 
  # write_csv(quadgram, "data/quadgram.csv")
  # quadgram <- read_csv("data/quadgram.csv")
  

  # quadgram <- generate_ngram(data$lines, n = 4)
  # quadgram <- quadgram[which(quadgram$n > 1),]
  # write_csv(quadgram, "data/quadgram.csv")
  # rm(quadgram)
}

# Combining the data files from above has proven to be challenging as it 
# slows R down too much. I'm currently working to try to combine them in chunks
# To see all the inexpert files that were initially used for this, see the file
# combinequadgrams.r

# A more refined, programattically oriented way I began to implement is follows
# The initial method of using count() proved to be slow when I performed a
# benchmark test vs full_join and mutate. 
# Using combine 1 turns out to be 69 times slower than combine_ngram! 
combine1 <- function(file1, file2) {
  rbind(read_csv(file1, show_col_types = FALSE), 
        read_csv(file2, show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
}

# This function takes the names of max 4 files ngram, loads them, and combines 
# them. To be more reproducible, I can use this combine_ngram function to combine
# the data instead of what was done in combinequadgrams.r. 
combine_ngram <- function(files) {
  #data1 <- read_csv(file1, show_col_types = FALSE)
  #data2 <- read_csv(file2, show_col_types = FALSE)
  data <- lapply(files, read_csv, show_col_types = FALSE)
  data <- purrr::reduce(data, full_join, by = "words") %>%
    mutate(n.x = ifelse(is.na(n.x), 0, n.x),
           n.y = ifelse(is.na(n.y), 0, n.y), 
           n.x.x = ifelse(is.na(n.x.x), 0, n.x.x), 
           n.y.y = ifelse(is.na(n.y.y), 0, n.y.y)) %>%
    dplyr::mutate(n = n.x + n.y + n.x.x + n.y.y, .keep = "unused")
  data
}

# To combine all the files, they need to be combined in groups, the following
# will split the files into 4 groups and they can then be combined. 
# fnames <- paste0("data/quadgram", c(1:40), ".csv")
# split(fnames, rep(1:10, each = 4))
# combine_ngram()
bigram <- read_csv("data/bigram.csv")
bigram_sep <- bigram %>%
  separate_wider_delim(cols = words, 
                       delim = " ", 
                       names = c("first", "second"))

trigram <- read_csv("data/trigram.csv")
trigram_sep <- trigram %>% 
  separate_wider_delim(cols = words, 
                       delim = " ", 
                       names = c("first", "second", "third"))
trigram_sep2 <- trigram %>%
  mutate(stem = word(words, 1, 2), 
         pred = word(words, -1))
  

quadgram <- read_csv("data/quadgram.csv")
quadgram_sep <- quadgram %>%
  separate_wider_delim(cols = words, 
                       delim = " ", 
                       names = c("first", "second", "third", "fourth"))

# Function to find Linear Good-Turing Smoothing coefficients
LGT <- function(ngram_sep) {
  t1 <- table(ngram_sep$n) # Find frequency of different counts
  t <- c(as.numeric(names(t1))[-1],as.numeric(names(t1))[length(t1)]) # lead 1
  q <- c(0,as.numeric(names(t1))[-length(t1)]) # lag 1
  zr <- t1 / (.5*(t-q)) # calculate zr
  # convert from table object to dataframe
  zr <- as.data.frame(zr) %>%
    mutate(z_r = as.numeric(as.character(Var1))
           ,.keep = "unused") %>%
    select(c(2,1))
  
  # convert nr, unaltered data to dataframe
  nr <- as.data.frame(t1) %>% 
    mutate(n_r = as.numeric(as.character(Var1)), 
           .keep = "unused")
  smooth_zr <- lm(log10(Freq) ~ log10(z_r), data = zr)
  return(smooth_zr$coefficients)
}

# Helper: Takes in r-values (such as nr$n_r), and coefficients from smoothed fit 
# of Good-Turing and returns estimates for r_star
rs_GT_est <- function(r, coef) {
  return(r * (1 + 1/r) ^ (1 + coef[2]))
}

# Helper: Turing estimates of r_star on nr, the raw counts
rs_T_est <- function(nr) {
  (nr$n_r + 1) * c(nr$Freq[-1],0) / nr$Freq
}

# Function to add Turing/Good-Turing discounting to existing ngram_sep data. 
add_GT_prob <- function(ngram_sep) {
  t1 <- table(ngram_sep$n) # Find frequency of different counts
  # convert nr, unaltered data to dataframe
  nr <- as.data.frame(t1) %>% 
    mutate(n_r = as.numeric(as.character(Var1)), 
           .keep = "unused")
  smooth_zr <- LGT(ngram_sep)
  # Compute Turing rstars
  rs_T <- rs_T_est(nr)
  # Compute Good-Turing rstars
  rs_GT <- rs_GT_est(nr$n_r, smooth_zr)
  
  # Compute the standard deviation of the Turing estimates as described in paper
  rs_T_sd <- rep(1, nrow(nr))
  for (i in 1:nrow(nr)) {
    rs_T_sd[i] <- (nr$n_r[i]+1)/nr$Freq[i] * sqrt(nr$Freq[i+1]*(1+nr$Freq[i+1]/nr$Freq[i]))
  }
  
  # Make switch from Turing to Linear Good-Turing estimates 
  rs_combined <- rep(0, nrow(nr))
  useturing <- TRUE
  for (i in 1:nrow(nr)) {
    if (!useturing) {
      rs_combined[i] <- rs_GT[i]
    } else if (abs(rs_T - rs_GT)[i] > 1.65*rs_T_sd[i]) {
      rs_combined[i] <- rs_T[i]
    } else {
      useturing <- FALSE
      rs_combined[i] <- rs_GT[i]
    }
  }
  
  ## renormalize probabilities 
  xN <- sum(nr$n_r*nr$Freq)
  sum_prob <- sum(rs_combined*nr$Freq/xN)
  # To renormalize, we want to include room for N0 which is approximated as N1/N
  rs_combined_norm <- (1-nr$Freq[1]/xN)*(rs_combined/xN) / sum_prob
  
  pr_freq_lookup <- data.frame(pr = rs_combined_norm, 
                               n = nr$n_r, 
                               n2 = rs_combined)
  
  left_join(ngram_sep, pr_freq_lookup, by = "n")
}

monogram <- read_csv("data/monogram.csv")
bigram_sep <- add_GT_prob(bigram_sep)
write_csv(bigram_sep, "data/bigram_sep.csv")
trigram_sep <- add_GT_prob(trigram_sep)
write_csv(trigram_sep, "data/trigram_sep.csv")
quadgram_sep <- add_GT_prob(quadgram_sep)
write_csv(quadgram_sep, "data/quadgram_sep.csv")
