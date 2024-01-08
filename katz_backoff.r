library(readr)
library(dplyr)
library(stringr)
library(tidyverse)

monogram <- read_csv("data/monogram.csv")
bigram_sep <- read_csv("data/bigram_sep.csv")
trigram_sep <- read_csv("data/trigram_sep.csv")
quadgram_sep <- read_csv("data/quadgram_sep.csv")

# Prediction functions
katz_bigram <- function(phrase = character()) {
  phrase <- gsub(" $", "", phrase) %>%
    tolower() # get rid of trailing whitespace
  first <- word(phrase, -1) # find last word of phrase
  matches <- bigram_sep[which(bigram_sep$first == first),] #find next word 
  alpha <- 1 - sum(matches$n2) / sum(matches$n)
  top_match <- matches[1,]
  top_A_prob <- sum(matches$n) # total probability of all i-1
  # number of instances of top bigram
  top_A <- data.frame(prediction = top_match$second, prob = top_match$n2/top_A_prob)
  
  if(is.na(top_A[1])) {  # If no matches found, return NA
    return(NA)
  }
  
  # find set B: words that are not in A
  setB <- anti_join(monogram, bigram_sep[which(bigram_sep$first == phrase),], by = join_by(words == second))
  # Find top match in B and probability = alpha * c(wi)/c(w)
  top_B <- data.frame(prediction = setB$words[1], prob = alpha * setB$n[1]/sum(setB$n))
  
  if(top_A$prob > top_B$prob) {
    return(top_A)
  } else {
    return(top_B)
  }
}

katz_trigram <- function(phrase = character()) {
  phrase <- gsub(" $", "", phrase) %>%
    tolower() # get rid of trailing whitespace
  first <- word(phrase, -2)
  second <- word(phrase, -1)
  matches <- trigram_sep[which(trigram_sep$first == first & 
                                 trigram_sep$second == second),] #find next word 
  alpha <- 1 - sum(matches$n2) / sum(matches$n)
  
  top_match <- matches[1,]
  top_A_prob <- sum(matches$n) # total probability of all i-1
  # number of instances of top trigram
  top_A <- data.frame(prediction = top_match$third, prob = top_match$n2/top_A_prob)
  
  if(is.na(top_A[1])) { # If no matches found, return NA
    return(NA)
  }
  
  # find set B: words that are not in A
  setB <- anti_join(monogram, matches, by = join_by(words == third))
  # Find top match in B and probability = alpha * qBO(wi|wi-1) / sum(qBO(w|wi-1))

  setB_bigrams <- data.frame(first = second, second = setB$words)
  setB_matches <- semi_join(bigram_sep, 
                            setB_bigrams, 
                            by = join_by(first, second))
  top_B <- data.frame(prediction = setB_matches$second[1],
                      prob =  ifelse(nrow(setB_matches[1])==0, 0, alpha * setB_matches$n[1] / sum(setB_matches$n)))
  
  if(top_A$prob > top_B$prob) {
    return(top_A)
  } else {
    return(top_B)
  }
}

katz_quadgram <- function(phrase) {
  phrase <- gsub(" $", "", phrase) %>%
    tolower() # get rid of trailing whitespace
  first <- word(phrase, -3)
  second <- word(phrase, -2)
  third <- word(phrase, -1)
  
  matches <- quadgram_sep[which(quadgram_sep$first == first & 
                                  quadgram_sep$second == second & 
                                  quadgram_sep$third == third),] #find next word 
  alpha <- 1 - sum(matches$n2) / sum(matches$n)
  top_match <- matches[1,]
  top_A_prob <- sum(matches$n) # total probability of all i-1
  # number of instances of top quadgram
  top_A <- data.frame(prediction = top_match$fourth, prob = top_match$n2/top_A_prob)
  
  if(is.na(top_A[1])) { # If no matches found, return NA
    return(NA)
  }
  
  # find set B: words that are not in A
  setB <- anti_join(monogram, matches, by = join_by(words == fourth))
  # Find top match in B and probability = alpha * qBO(wi|wi-2,wi-1) / sum(qBO(w|wi-2,wi-1))
  
  setB_trigrams <- data.frame(first = second, second = third, third = setB$words)
  setB_matches <- semi_join(trigram_sep, 
                            setB_trigrams, 
                            by = join_by(first, second, third))
  top_B <- data.frame(prediction = setB_matches$third[1],
                      prob =  ifelse(nrow(setB_matches[1])==0, 0, alpha * setB_matches$n[1] / sum(setB_matches$n)))
  
  if(top_A$prob > top_B$prob) {
    return(top_A)
  } else {
    return(top_B)
  }
}

katz_ngram <- function(phrase = character()) {
  phrase <- gsub(" $", "", phrase) # get rid of trailing whitespace
  len <- length(strsplit(phrase, "\\W+")[[1]])
  
  if (len >= 3) {
    rtrn_guess <- katz_quadgram(phrase)
    if(is.na(rtrn_guess[1])) {
      rtrn_guess <- katz_trigram(phrase)
      if (is.na(rtrn_guess[1])) {
        rtrn_guess <- katz_bigram(phrase)
        if(is.na(rtrn_guess[1])) {
          return("Unk")
        }
      }
    }
    rtrn_guess
  } else if (len == 2) {
    rtrn_guess <- katz_trigram(phrase)
    if(is.na(rtrn_guess[1])) {
      rtrn_guess <- katz_bigram(phrase)
      if (is.na(rtrn_guess[1])) {
        return("Unk")
      }
    } 
    rtrn_guess
  } else {
    rtrn_guess <- katz_bigram(phrase)
    if(is.na(rtrn_guess[1])) {
      return("Unk")
    }
    rtrn_guess
  }
}
