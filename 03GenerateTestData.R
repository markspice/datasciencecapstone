##TEST DATASET:

set.seed(12345)
test3 <- unnest_tokens(test, term, text, token = "ngrams", n = 4, to_lower = TRUE, drop = FALSE)
test3 <- test3 %>% 
  subset(term != "<NA>") %>%
  separate(col = term, into = c("word_1","word_2","word_3","word"), sep = " ") %>%
  select(word_1, word_2, word_3, word)
test3 <- test3[sample(nrow(test3),size = 1000, replace = FALSE),]

set.seed(54321)
test2 <- unnest_tokens(test, term, text, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
test2 <- test2 %>% 
  subset(term != "<NA>") %>%
  separate(col = term, into = c("word_2","word_3","word"), sep = " ") %>%
  select(word_2, word_3, word)
test2 <- test2[sample(nrow(test2),size = 1000, replace = FALSE),]

set.seed(12321)
test1 <- unnest_tokens(test, term, text, token = "ngrams", n = 2, to_lower = TRUE, drop = FALSE)
test1 <- test1 %>% 
  subset(term != "<NA>") %>%
  separate(col = term, into = c("word_3","word"), sep = " ") %>%
  select(word_3, word)
test1 <- test1[sample(nrow(test1),size = 1000, replace = FALSE),]

##TEST DATASET (CLEAN):

test$cleanText <- replace_contraction(test$text)
test$cleanText <- replace_ordinal(test$cleanText)
test$cleanText <- replace_word_elongation(test$cleanText)

set.seed(12345)
cleanTest3 <- unnest_tokens(test, term, cleanText, token = "ngrams", n = 4, to_lower = TRUE, drop = FALSE)
cleanTest3 <- cleanTest3 %>%
  subset(term != "<NA>") %>%
  separate(col = term, into = c("word_1","word_2","word_3","word"), sep = " ") %>%
  select(word_1, word_2, word_3, word)
cleanTest3 <- cleanTest3[sample(nrow(cleanTest3),size = 1000, replace = FALSE),]

cleanTest3$word_1 <- replace_names(cleanTest3$word_1, replacement = "<<NAME>>")
cleanTest3$word_1 <- replace_time(cleanTest3$word_1, replacement = "<<TIME>>")
cleanTest3$word_1 <- gsub("[\\.|,]", "", cleanTest3$word_1)
cleanTest3$word_1 <- gsub("[0-9]+", "<<NUMBER>>", cleanTest3$word_1)
cleanTest3$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", cleanTest3$word_1)

cleanTest3$word_2 <- replace_names(cleanTest3$word_2, replacement = "<<NAME>>")
cleanTest3$word_2 <- replace_time(cleanTest3$word_2, replacement = "<<TIME>>")
cleanTest3$word_2 <- gsub("[\\.|,]", "", cleanTest3$word_2)
cleanTest3$word_2 <- gsub("[0-9]+", "<<NUMBER>>", cleanTest3$word_2)
cleanTest3$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", cleanTest3$word_2)

cleanTest3$word_3 <- replace_names(cleanTest3$word_3, replacement = "<<NAME>>")
cleanTest3$word_3 <- replace_time(cleanTest3$word_3, replacement = "<<TIME>>")
cleanTest3$word_3 <- gsub("[\\.|,]", "", cleanTest3$word_3)
cleanTest3$word_3 <- gsub("[0-9]+", "<<NUMBER>>", cleanTest3$word_3)
cleanTest3$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", cleanTest3$word_3)

set.seed(54321)
cleanTest2 <- unnest_tokens(test, term, cleanText, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
cleanTest2 <- cleanTest2 %>% 
  subset(term != "<NA>") %>%
  separate(col = term, into = c("word_2","word_3","word"), sep = " ") %>%
  select(word_2, word_3, word)
cleanTest2 <- cleanTest2[sample(nrow(cleanTest2),size = 1000, replace = FALSE),]

cleanTest2$word_2 <- replace_names(cleanTest2$word_2, replacement = "<<NAME>>")
cleanTest2$word_2 <- replace_time(cleanTest2$word_2, replacement = "<<TIME>>")
cleanTest2$word_2 <- gsub("[\\.|,]", "", cleanTest2$word_2)
cleanTest2$word_2 <- gsub("[0-9]+", "<<NUMBER>>", cleanTest2$word_2)
cleanTest2$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", cleanTest2$word_2)

cleanTest2$word_3 <- replace_names(cleanTest2$word_3, replacement = "<<NAME>>")
cleanTest2$word_3 <- replace_time(cleanTest2$word_3, replacement = "<<TIME>>")
cleanTest2$word_3 <- gsub("[\\.|,]", "", cleanTest2$word_3)
cleanTest2$word_3 <- gsub("[0-9]+", "<<NUMBER>>", cleanTest2$word_3)
cleanTest2$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", cleanTest2$word_3)

set.seed(12321)
cleanTest1 <- unnest_tokens(test, term, cleanText, token = "ngrams", n = 2, to_lower = TRUE, drop = FALSE)
cleanTest1 <- cleanTest1 %>% 
  subset(term != "<NA>") %>%
  separate(col = term, into = c("word_3","word"), sep = " ") %>%
  select(word_3, word)
cleanTest1 <- cleanTest1[sample(nrow(cleanTest1),size = 1000, replace = FALSE),]

cleanTest1$word_3 <- replace_names(cleanTest1$word_3, replacement = "<<NAME>>")
cleanTest1$word_3 <- replace_time(cleanTest1$word_3, replacement = "<<TIME>>")
cleanTest1$word_3 <- gsub("[\\.|,]", "", cleanTest1$word_3)
cleanTest1$word_3 <- gsub("[0-9]+", "<<NUMBER>>", cleanTest1$word_3)
cleanTest1$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", cleanTest1$word_3)