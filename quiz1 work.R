con <- file("final/en_US/en_US.twitter.txt", "r")
readLines(con, 1)

# How many lines in twitter? 
length(readLines(con))

# Longest line? 
blogs <- file("final/en_US/en_US.blogs.txt", "r")
news <- file("final/en_US/en_US.news.txt", "r")
twitter <- file("final/en_US/en_US.twitter.txt", "r")

# max length of line in each file
max(sapply(readLines(blogs), nchar))
max(sapply(readLines(news), nchar))
max(sapply(readLines(twitter), nchar))

# Ratio of love to hate in twitter
twitter <- file("final/en_US/en_US.twitter.txt", "r")
nlove <- length(grep("love", readLines(twitter)))
twitter <- file("final/en_US/en_US.twitter.txt", "r")
nhate <- length(grep("hate", readLines(twitter)))
nlove/nhate

# biostats in twitter  
twitter <- file("final/en_US/en_US.twitter.txt", "r")
grep("biostats", readLines(twitter), value = TRUE)

# Match longer string in twitter  
twitter <- file("final/en_US/en_US.twitter.txt", "r")
grep("A computer once beat me at chess, but it was no match for me at kickboxing", 
     readLines(twitter))

close(blogs); close(news); close(twitter)
