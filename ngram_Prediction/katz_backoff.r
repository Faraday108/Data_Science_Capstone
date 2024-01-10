library(readr)
library(dplyr)
library(stringr)
library(tidyverse)

incProgress(0, detail = "Monogram")
monogram <- read_csv("data/monogram.csv")
incProgress(1/4, detail = "Bigram")
#bigram_sep <- read_csv("data/bigram_sep.csv")
bigram_sep <- read_csv("data/bigram_trim5.csv")
incProgress(2/4, detail = "Trigram")
#trigram_sep <- read_csv("data/trigram_sep.csv")
trigram_sep <- read_csv("data/trigram_trim5.csv")
incProgress(3/4, detail = "Quadgram")
#quadgram_sep <- read_csv("data/quadgram_sep.csv")
quadgram_sep <- read_csv("data/quadgram_trim5.csv")

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
  top_10A <- data.frame(prediction = matches$second[1:10], 
                        prob = matches$n2[1:10]/sum(matches$n), 
                        source = "bigram")
  if(is.na(top_A[1])) {  # If no matches found, return NA
    return(data.frame(prediction  = character(), 
                      prob = numeric(), 
                      ngram = character()))
  }
  
  # find set B: words that are not in A
  setB <- anti_join(monogram, bigram_sep[which(bigram_sep$first == phrase),], by = join_by(words == second))
  # Find top match in B and probability = alpha * c(wi)/c(w)
  top_B <- data.frame(prediction = setB$words[1], prob = alpha * setB$n[1]/sum(setB$n))
  top_10B <- data.frame(prediction = setB$words[1:10], 
                        prob = alpha * setB$n[1:10]/sum(setB$n), 
                        source = "monogram backoff")
  
  rbind(top_10A, top_10B) %>%
    arrange(desc(prob)) %>%
    head(10)
  # if(top_A$prob > top_B$prob) {
  #   return(top_A)
  # } else {
  #   return(top_B)
  # }
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
  top_10A <- data.frame(prediction = matches$third[1:10], 
                        prob = matches$n2[1:10]/sum(matches$n), 
                        source = "trigram")
  if(is.na(top_A[1])) { # If no matches found, return NA
    return(data.frame(prediction  = character(), 
                      prob = numeric(), 
                      ngram = character()))
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
  top_10B <- data.frame(prediction = setB$words[1:10], 
                        prob = alpha * setB$n[1:10]/sum(setB$n), 
                        source = "bigram backoff")
  
  rbind(top_10A, top_10B) %>%
    arrange(desc(prob)) %>%
    head(10)
  # 
  # if(top_A$prob > top_B$prob) {
  #   return(top_A)
  # } else {
  #   return(top_B)
  # }
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
  top_10A <- data.frame(prediction = matches$fourth[1:10], 
                        prob = matches$n2[1:10]/sum(matches$n), 
                        source = "quadgram")
  if(is.na(top_A[1])) { # If no matches found, return NA
    return(data.frame(prediction  = character(), 
                      prob = numeric(), 
                      ngram = character()))
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
  top_10B <- data.frame(prediction = setB$words[1:10], 
                        prob = alpha * setB$n[1:10]/sum(setB$n), 
                        source = "trigram backoff")
  
  rbind(top_10A, top_10B) %>%
    arrange(desc(prob)) %>%
    head(10)
  # 
  # if(top_A$prob > top_B$prob) {
  #   return(top_A)
  # } else {
  #   return(top_B)
  # }
}

katz_ngram <- function(phrase = character()) {
  phrase <- gsub(" $", "", phrase) # get rid of trailing whitespace
  len <- length(strsplit(phrase, "\\W+")[[1]])
  
  if (len >= 3) {
    rtrn_guess <- katz_quadgram(phrase)
    if(nrow(rtrn_guess) < 10) {
      rtrn_guess <- katz_trigram(phrase)
      if (nrow(rtrn_guess) < 10) {
        rtrn_guess <- katz_bigram(phrase)
        if(nrow(rtrn_guess) < 10) {
          return("Not in corpus")
        }
      }
    }
    rtrn_guess
  } else if (len == 2) {
    rtrn_guess <- katz_trigram(phrase)
    if(nrow(rtrn_guess) < 10) {
      rtrn_guess <- katz_bigram(phrase)
      if (nrow(rtrn_guess) < 10) {
        return("Not in corpus")
      }
    } 
    rtrn_guess
  } else {
    rtrn_guess <- katz_bigram(phrase)
    if(nrow(rtrn_guess) < 10) {
      return("Not in corpus")
    }
    rtrn_guess
  }
}
