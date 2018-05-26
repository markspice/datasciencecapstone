##Function for testing Model c3 with maximmum 2 word input

sizeTest <- function(x) {
  
  set.seed(10101)
  bigSmp <- rbinom(len, 1, x)
  bigSmpText <- allText[bigSmp == 1,]
  
  bigSmpText$text <- gsub("[^[:alnum:][:space:]'-]", "", bigSmpText$text)
  
  a <- c(" isnt ", " hasnt ", " hadnt ", " didnt ", " wouldnt ", " couldnt ", " shouldnt ", " shes ", " theres ", " whos ", " dont ", " youd ", " hed ", " theyd ", " ive ", " youve ", " weve ", " theyve ", " youre ", " theyre ")
  b <- c(" is not ", " has not ", " had not ", " did not ", " would not ", " could not ", " should not ", " she is ", " there is ", " who is ", " do not ", " you would ", " he would ", " they would ", " i have ", " you have ", " we have ", " they have ", " you are ", " they are ")
  for(i in seq_along(a)) bigSmpText$text <- gsub(a[i], b[i], bigSmpText$text, fixed = TRUE)
  
  a <- c("^isnt ", "^hasnt ", "^hadnt ", "^didnt ", "^wouldnt ", "^couldnt ", "^shouldnt ", "^shes ", "^theres ", "^whos ", "^dont ", "^youd ", "^hed ", "^theyd ", "^ive ", "^youve ", "^weve ", "^theyve ", "^youre ", "^theyre ")
  b <- c("^is not ", "^has not ", "^had not ", "^did not ", "^would not ", "^could not ", "^should not ", "^she is ", "^there is ", "^who is ", "^do not ", "^you would ", "^he would ", "^they would ", "^i have ", "^you have ", "^we have ", "^they have ", "^you are ", "^they are ")
  for(i in seq_along(a)) bigSmpText$text <- gsub(a[i], b[i], bigSmpText$text, fixed = TRUE)
  
  a <- c(" isnt$", " hasnt$", " hadnt$", " didnt$", " wouldnt$", " couldnt$", " shouldnt$", " shes$", " theres$", " whos$", " dont$", " youd$", " hed$", " theyd$", " ive$", " youve$", " weve$", " theyve$", " youre$", " theyre$")
  b <- c(" is not$", " has not$", " had not$", " did not$", " would not$", " could not$", " should not$", " she is$", " there is$", " who is$", " do not$", " you would$", " he would$", " they would$", " i have$", " you have$", " we have$", " they have$", " you are$", " they are$")
  for(i in seq_along(a)) bigSmpText$text <- gsub(a[i], b[i], bigSmpText$text, fixed = TRUE)
  
  library(textclean)
  
  bigSmpText$text <- replace_contraction(bigSmpText$text)
  bigSmpText$text <- replace_ordinal(bigSmpText$text)
  
  #test set
  
  bigTest <- allText[bigSmp == 0,]
  len = length(bigTest$text)
  set.seed(13579)
  bigSmp <- rbinom(len, 1, 0.001)
  bigTest <- bigTest[smp == 1,]
  
  bigTest$text <- gsub("[^[:alnum:][:space:]'-]", "", bigTest$text)
  
  a <- c(" isnt ", " hasnt ", " hadnt ", " didnt ", " wouldnt ", " couldnt ", " shouldnt ", " shes ", " theres ", " whos ", " dont ", " youd ", " hed ", " theyd ", " ive ", " youve ", " weve ", " theyve ", " youre ", " theyre ")
  b <- c(" is not ", " has not ", " had not ", " did not ", " would not ", " could not ", " should not ", " she is ", " there is ", " who is ", " do not ", " you would ", " he would ", " they would ", " i have ", " you have ", " we have ", " they have ", " you are ", " they are ")
  for(i in seq_along(a)) bigTest$text <- gsub(a[i], b[i], bigTest$text, fixed = TRUE)
  
  a <- c("^isnt ", "^hasnt ", "^hadnt ", "^didnt ", "^wouldnt ", "^couldnt ", "^shouldnt ", "^shes ", "^theres ", "^whos ", "^dont ", "^youd ", "^hed ", "^theyd ", "^ive ", "^youve ", "^weve ", "^theyve ", "^youre ", "^theyre ")
  b <- c("^is not ", "^has not ", "^had not ", "^did not ", "^would not ", "^could not ", "^should not ", "^she is ", "^there is ", "^who is ", "^do not ", "^you would ", "^he would ", "^they would ", "^i have ", "^you have ", "^we have ", "^they have ", "^you are ", "^they are ")
  for(i in seq_along(a)) bigTest$text <- gsub(a[i], b[i], bigTest$text, fixed = TRUE)
  
  a <- c(" isnt$", " hasnt$", " hadnt$", " didnt$", " wouldnt$", " couldnt$", " shouldnt$", " shes$", " theres$", " whos$", " dont$", " youd$", " hed$", " theyd$", " ive$", " youve$", " weve$", " theyve$", " youre$", " theyre$")
  b <- c(" is not$", " has not$", " had not$", " did not$", " would not$", " could not$", " should not$", " she is$", " there is$", " who is$", " do not$", " you would$", " he would$", " they would$", " i have$", " you have$", " we have$", " they have$", " you are$", " they are$")
  for(i in seq_along(a)) bigTest$text <- gsub(a[i], b[i], bigTest$text, fixed = TRUE)
  
  bigTest$text <- replace_contraction(bigTest$text)
  bigTest$text <- replace_ordinal(bigTest$text)
  
  bigTest$cleanText <- replace_word_elongation(bigTest$text)
  
  set.seed(54321)
  bigCleanTest2 <- unnest_tokens(bigTest, term, cleanText, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
  bigCleanTest2 <- bigCleanTest2 %>% 
    subset(term != "<NA>") %>%
    separate(col = term, into = c("word_2","word_3","word"), sep = " ") %>%
    select(word_2, word_3, word)
  bigCleanTest2 <- bigCleanTest2[sample(nrow(bigCleanTest2),size = 1000, replace = FALSE),]
  
  bigCleanTest2$word_2 <- replace_names(bigCleanTest2$word_2, replacement = "<<NAME>>")
  bigCleanTest2$word_2 <- replace_time(bigCleanTest2$word_2, replacement = "<<TIME>>")
  bigCleanTest2$word_2 <- gsub("[\\.|,]", "", bigCleanTest2$word_2)
  bigCleanTest2$word_2 <- gsub("[0-9]+", "<<NUMBER>>", bigCleanTest2$word_2)
  bigCleanTest2$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", bigCleanTest2$word_2)
  
  bigCleanTest2$word_3 <- replace_names(bigCleanTest2$word_3, replacement = "<<NAME>>")
  bigCleanTest2$word_3 <- replace_time(bigCleanTest2$word_3, replacement = "<<TIME>>")
  bigCleanTest2$word_3 <- gsub("[\\.|,]", "", bigCleanTest2$word_3)
  bigCleanTest2$word_3 <- gsub("[0-9]+", "<<NUMBER>>", bigCleanTest2$word_3)
  bigCleanTest2$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", bigCleanTest2$word_3)
  
  
  ##Single word frequencies from all sources combined:
  
  bigSmpWords <- unnest_tokens(bigSmpText, words, text, token = "words", to_lower = TRUE, drop = FALSE)
  includeWords <- grepl("^([A-Z]|'|-)+$", bigSmpWords$words, ignore.case = TRUE)
  bigSmpWordTotals <- data.frame(table(bigSmpWords$words[includeWords == TRUE], bigSmpWords$source[includeWords == TRUE]), stringsAsFactors = FALSE)
  names(bigSmpWordTotals) <- c("word","source","freq")
  bigSmpWordTotals$source <- as.factor(bigSmpWordTotals$source)
  bigSmpWordTotals <- subset(bigSmpWordTotals, bigSmpWordTotals$freq > 0)
  
  bigSmpAllSources <- tapply(bigSmpWordTotals$freq, bigSmpWordTotals$word, sum)
  bigSmpAllSources <- data.frame(word = names(bigSmpAllSources[!is.na(bigSmpAllSources)]), wordFreq = bigSmpAllSources[!is.na(bigSmpAllSources)])
  bigSmpAllSources <- bigSmpAllSources %>% arrange(desc(wordFreq))
  
  #2-Gram
  
  bigSmpClean2Words <- unnest_tokens(bigSmpText, terms, text, token = "ngrams", n = 2, to_lower = TRUE, drop = FALSE)
  bigSmpClean2WordTotals <- data.frame(table(bigSmpClean2Words$terms), stringsAsFactors = FALSE)
  names(bigSmpClean2WordTotals) <- c("term","freq")
  bigSmpClean2WordTotals <- bigSmpClean2WordTotals %>% separate(col = term, into = c("term","word"), sep = " ")
  
  bigSmpClean2WordTotals$term <- replace_word_elongation(bigSmpClean2WordTotals$term)
  bigSmpClean2WordTotals$term <- replace_names(bigSmpClean2WordTotals$term, replacement = "<<NAME>>")
  bigSmpClean2WordTotals$term <- replace_time(bigSmpClean2WordTotals$term, replacement = "<<TIME>>")
  bigSmpClean2WordTotals$term <- gsub("[0-9][\\.|,][0-9]", "0", bigSmpClean2WordTotals$term)
  bigSmpClean2WordTotals$term <- gsub("([0-9]+)", "<<NUMBER>>", bigSmpClean2WordTotals$term)
  bigSmpClean2WordTotals$term <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", bigSmpClean2WordTotals$term)
  
  includeTerms <- grepl("^([A-Z]|'|-|<<.*>>)+$", bigSmpClean2WordTotals$term, ignore.case = TRUE)
  includeWords <- grepl("^([A-Z]|'|-|)+$", bigSmpClean2WordTotals$word, ignore.case = TRUE)
  bigSmpClean2WordTotals <- bigSmpClean2WordTotals %>%
    subset(includeTerms == TRUE & includeWords == TRUE & freq > 0)
  
  bigSmpClean2WordTotals <- aggregate(bigSmpClean2WordTotals$freq, by=list(term = bigSmpClean2WordTotals$term, word = bigSmpClean2WordTotals$word), FUN=sum)
  colnames(bigSmpClean2WordTotals)[3] <- "freq"
  
  #3-Gram
  
  bigSmpClean3Words <- unnest_tokens(bigSmpText, terms, text, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
  bigSmpClean3WordTotals <- data.frame(table(bigSmpClean3Words$terms), stringsAsFactors = FALSE)
  names(bigSmpClean3WordTotals) <- c("term","freq")
  bigSmpClean3WordTotals <- bigSmpClean3WordTotals %>% separate(col = term, into = c("word_1","word_2","word"), sep = " ")
  
  bigSmpClean3WordTotals$word_1 <- replace_word_elongation(bigSmpClean3WordTotals$word_1)
  bigSmpClean3WordTotals$word_1 <- replace_names(bigSmpClean3WordTotals$word_1, replacement = "<<NAME>>")
  bigSmpClean3WordTotals$word_1 <- replace_time(bigSmpClean3WordTotals$word_1, replacement = "<<TIME>>")
  bigSmpClean3WordTotals$word_1 <- gsub("[\\.|,]", "", bigSmpClean3WordTotals$word_1)
  bigSmpClean3WordTotals$word_1 <- gsub("[0-9]+", "<<NUMBER>>", bigSmpClean3WordTotals$word_1)
  bigSmpClean3WordTotals$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", bigSmpClean3WordTotals$word_1)
  
  bigSmpClean3WordTotals$word_2 <- replace_word_elongation(bigSmpClean3WordTotals$word_2)
  bigSmpClean3WordTotals$word_2 <- replace_names(bigSmpClean3WordTotals$word_2, replacement = "<<NAME>>")
  bigSmpClean3WordTotals$word_2 <- replace_time(bigSmpClean3WordTotals$word_2, replacement = "<<TIME>>")
  bigSmpClean3WordTotals$word_2 <- gsub("[\\.|,]", "", bigSmpClean3WordTotals$word_2)
  bigSmpClean3WordTotals$word_2 <- gsub("[0-9]+", "<<NUMBER>>", bigSmpClean3WordTotals$word_2)
  bigSmpClean3WordTotals$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", bigSmpClean3WordTotals$word_2)
  
  includeWord_1 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", bigSmpClean3WordTotals$word_1, ignore.case = TRUE)
  includeWord_2 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", bigSmpClean3WordTotals$word_2, ignore.case = TRUE)
  includeWords <- grepl("^([A-Z]|'|-|)+$", bigSmpClean3WordTotals$word, ignore.case = TRUE)
  bigSmpClean3WordTotals <- bigSmpClean3WordTotals %>%
    subset(includeWord_1 == TRUE & includeWord_2 == TRUE & includeWords == TRUE & freq > 0)
  
  bigSmpClean3WordTotals <- aggregate(bigSmpClean3WordTotals$freq, by=list(word_1 = bigSmpClean3WordTotals$word_1, word_2 = bigSmpClean3WordTotals$word_2, word = bigSmpClean3WordTotals$word), FUN=sum)
  colnames(bigSmpClean3WordTotals)[4] <- "freq"
  
  bigdfc <- rbind(data.frame(word_1 = "<unkn>", word_2 = bigSmpClean3WordTotals$word_1, word_3 = bigSmpClean3WordTotals$word_2, word = bigSmpClean3WordTotals$word, termFreq = bigSmpClean3WordTotals$freq),
                  data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = bigSmpClean2WordTotals$term, word = bigSmpClean2WordTotals$word, termFreq = bigSmpClean2WordTotals$freq),
                  data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = bigSmpAllSources$word, termFreq = bigSmpAllSources$wordFreq))
  
  bigdfc3b <- bigdfc %>% subset(termFreq > 1) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
  bigdfc3b <- merge(x = bigdfc3b, y = smpAllSources, by.x = "word", by.y = "word")
  bigdfc3b <- bigdfc3b %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
  bigdfc3b <- bigdfc3b[with(bigdfc3b, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]
  
  for (i in 1:length(bigCleanTest2$word)) {
    
    r21 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == bigCleanTest2$word_2[i] & bigdfc3b$word_3 == bigCleanTest2$word_3[i]],3)))[1]
    r22 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == bigCleanTest2$word_2[i] & bigdfc3b$word_3 == bigCleanTest2$word_3[i]],3)))[2]
    r23 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == bigCleanTest2$word_2[i] & bigdfc3b$word_3 == bigCleanTest2$word_3[i]],3)))[3]
    
    r11 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == "<unkn>" & bigdfc3b$word_3 == bigCleanTest2$word_3[i]],3)))[1]
    r12 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == "<unkn>" & bigdfc3b$word_3 == bigCleanTest2$word_3[i]],3)))[2]
    r13 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == "<unkn>" & bigdfc3b$word_3 == bigCleanTest2$word_3[i]],3)))[3]
    
    r01 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == "<unkn>" & bigdfc3b$word_3 == "<unkn>"],3)))[1]
    r02 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == "<unkn>" & bigdfc3b$word_3 == "<unkn>"],3)))[2]
    r03 <- as.character(paste(head(bigdfc3b$word[bigdfc3b$word_1 == "<unkn>" & bigdfc3b$word_2 == "<unkn>" & bigdfc3b$word_3 == "<unkn>"],3)))[3]
    
    if (is.na(r21) == FALSE) {
      bigCleanTest2$result3_1[i] <- r21
    } else if (is.na(r11) == FALSE) {
      bigCleanTest2$result3_1[i] <- r11
    } else {
      bigCleanTest2$result3_1[i] <- r01
    }
    if (is.na(r22) == FALSE) {
      bigCleanTest2$result3_2[i] <- r22
    } else if (is.na(r11) == FALSE & r11 != bigCleanTest2$result3_1[i]) {
      bigCleanTest2$result3_2[i] <- r11 
    } else if (is.na(r12) == FALSE) {
      bigCleanTest2$result3_2[i] <- r12
    } else if (r01 != bigCleanTest2$result3_1[i]) {
      bigCleanTest2$result3_2[i] <- r01
    } else {
      bigCleanTest2$result3_2[i] <- r02
    }
    if (is.na(r23) == FALSE) {
      bigCleanTest2$result3_3[i] <- r23
    } else if (is.na(r11) == FALSE & r11 != bigCleanTest2$result3_1[i] & r11 != bigCleanTest2$result3_2[i]) {
      bigCleanTest2$result3_3[i] <- r11 
    } else if (is.na(r12) == FALSE & r12 != bigCleanTest2$result3_1[i] & r12 != bigCleanTest2$result3_2[i]) {
      bigCleanTest2$result3_3[i] <- r12
    } else if (is.na(r13) == FALSE) {
      bigCleanTest2$result3_3[i] <- r13
    } else if (r01 != bigCleanTest2$result3_1[i] & r01 != bigCleanTest2$result3_2[i]) {
      bigCleanTest2$result3_3[i] <- r01
    } else if (r02 != bigCleanTest2$result3_1[i] & r02 != bigCleanTest2$result3_2[i]) {
      bigCleanTest2$result3_3[i] <- r02
    } else {
      bigCleanTest2$result3_3[i] <- r03
    }
    
  }
  
  bigCleanTest2$bigsuccess3b <- bigCleanTest2$word == bigCleanTest2$result3_1 | bigCleanTest2$word == bigCleanTest2$result3_2 | bigCleanTest2$word == bigCleanTest2$result3_3
  
  df32g <- bigdfc3b[bigdfc3b$word_1 == "<unkn>", c(1,3,4)]
  names(df32g) <- c("word","word_1","word_2")
  
  bigTest <- data.frame(trainingObs = nrow(bigSmpText),
                        successRate = mean(bigCleanTest2$bigsuccess3b),
                        modelSize = format(object.size(df32g), units = "Mb"), 
                        runTime = system.time(mod3c_2(inputText = inputText))[3])
  
  bigTest
  
}