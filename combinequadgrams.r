quadgram12 <- rbind(read_csv("data/quadgram1.csv", show_col_types = FALSE), 
                    read_csv("data/quadgram2.csv", show_col_types = FALSE))
quadgram34 <- rbind(read_csv("data/quadgram3.csv", show_col_types = FALSE), 
                    read_csv("data/quadgram4.csv", show_col_types = FALSE))
quadgram14 <- count(rbind(quadgram12, quadgram34), words, wt = n, sort = TRUE)
rm(quadgram12, quadgram34)
quadgram56 <- rbind(read_csv("data/quadgram5.csv", show_col_types = FALSE), 
                    read_csv("data/quadgram6.csv", show_col_types = FALSE))
quadgram78 <- rbind(read_csv("data/quadgram7.csv", show_col_types = FALSE), 
                    read_csv("data/quadgram8.csv", show_col_types = FALSE))
quadgram58 <- count(rbind(quadgram56, quadgram78), words, wt = n, sort = TRUE)
rm(quadgram56, quadgram78)
quadgram18 <- count(rbind(quadgram14, quadgram58), words, wt = n, sort = TRUE)
rm(quadgram14, quadgram58)

write_csv(quadgram18, "data/quadgram1-8.csv")

quadgram910 <- rbind(read_csv("data/quadgram9.csv", show_col_types = FALSE), 
                     read_csv("data/quadgram10.csv", show_col_types = FALSE))
quadgram1112 <- rbind(read_csv("data/quadgram11.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram12.csv", show_col_types = FALSE))
quadgram912 <- count(rbind(quadgram910, quadgram1112), words, wt = n, sort = TRUE)
rm(quadgram910, quadgram1112)
quadgram1314 <- rbind(read_csv("data/quadgram13.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram14.csv", show_col_types = FALSE))
quadgram1516 <- rbind(read_csv("data/quadgram15.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram16.csv", show_col_types = FALSE))
quadgram1316 <- count(rbind(quadgram1314, quadgram1516), words, wt = n, sort = TRUE)
rm(quadgram1314, quadgram1516)
quadgram916 <- count(rbind(quadgram912, quadgram1316), words, wt = n, sort = TRUE)
rm(quadgram912, quadgram1316)
write_csv(quadgram916, "data/quadgram916.csv")
quadgram116 <- count(rbind(quadgram18, quadgram916), words, wt = n, sort = TRUE)
write_csv(quadgram116, "data/quadgram116.csv")
rm(quadgram116, quadgram18, quadgram916)

quadgram1720 <- rbind(read_csv("data/quadgram17.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram19.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram20.csv", show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
quadgram2124 <- rbind(read_csv("data/quadgram21.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram22.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram23.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram24.csv", show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
quadgram1724 <- count(rbind(quadgram1720, quadgram2124), words, wt = n, sort = TRUE)
rm(quadgram1720, quadgram2124)
quadgram2528 <- rbind(read_csv("data/quadgram25.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram26.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram27.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram28.csv", show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
quadgram2932 <- rbind(read_csv("data/quadgram29.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram30.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram31.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram32.csv", show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
quadgram2532 <- count(rbind(quadgram2528, quadgram2932), words, wt = n, sort = TRUE)
rm(quadgram2528, quadgram2932)
quadgram1732 <- count(rbind(quadgram1724, quadgram2532), words, wt = n, sort = TRUE)
write_csv(quadgram1732, "data/quadgram1732.csv")
rm(quadgram1724, quadgram2532, quadgram1732)
quadgram3336 <- rbind(read_csv("data/quadgram33.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram34.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram35.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram36.csv", show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
quadgram3740 <- rbind(read_csv("data/quadgram37.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram38.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram39.csv", show_col_types = FALSE), 
                      read_csv("data/quadgram40.csv", show_col_types = FALSE)) %>%
  count(words, wt = n, sort = TRUE)
quadgram3340 <- count(rbind(quadgram3336, quadgram3740), words, wt = n, sort = TRUE)
rm(quadgram3336, quadgram3740)
write_csv(quadgram3340, "data/quadgram3340.csv")
rm(quadgram3340)

quadgram116 <- read_csv("data/quadgram116.csv", show_col_types = FALSE)
quadgram1732 <- read_csv("data/quadgram1732.csv", show_col_types = FALSE)

quadgram132 <- full_join(quadgram116, quadgram1732, by = "words") 
rm(quadgram116, quadgram1732)
quadgram132 <- quadgram132 %>%
  mutate(n.x = ifelse(is.na(n.x), 0, n.x),
         n.y = ifelse(is.na(n.y), 0, n.y)) %>%
  dplyr::mutate(n = n.x + n.y, .keep = "unused")
quadgram132 <- quadgram132[!quadgram132$n == 1,]
write_csv(quadgram132, "data/quadgram132.csv")
quadgram3340 <- read_csv("data/quadgram3340.csv", show_col_types = FALSE)
quadgram140 <- full_join(quadgram132, quadgram3340, by = "words")
rm(quadgram132, quadgram3340)
quadgram140 <- quadgram140 %>%
  mutate(n.x = ifelse(is.na(n.x), 0, n.x),
         n.y = ifelse(is.na(n.y), 0, n.y)) %>%
  dplyr::mutate(n = n.x + n.y, .keep = "unused")
quadgram140 <- quadgram140[!quadgram140$n == 1,]
write_csv(quadgram140, "data/quadgram140.csv")