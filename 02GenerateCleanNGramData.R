##Generate clean N-word terms

smpClean2Words <- unnest_tokens(smpText, terms, text, token = "ngrams", n = 2, to_lower = TRUE, drop = FALSE)
smpClean2WordTotals <- data.frame(table(smpClean2Words$terms), stringsAsFactors = FALSE)
names(smpClean2WordTotals) <- c("term","freq")
smpClean2WordTotals <- smpClean2WordTotals %>% separate(col = term, into = c("term","word"), sep = " ")

smpClean2WordTotals$term <- replace_word_elongation(smpClean2WordTotals$term)
smpClean2WordTotals$term <- replace_names(smpClean2WordTotals$term, replacement = "<<NAME>>")
smpClean2WordTotals$term <- replace_time(smpClean2WordTotals$term, replacement = "<<TIME>>")
smpClean2WordTotals$term <- gsub("[0-9][\\.|,][0-9]", "0", smpClean2WordTotals$term)
smpClean2WordTotals$term <- gsub("([0-9]+)", "<<NUMBER>>", smpClean2WordTotals$term)
smpClean2WordTotals$term <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", smpClean2WordTotals$term)

includeTerms <- grepl("^([A-Z]|'|-|<<.*>>)+$", smpClean2WordTotals$term, ignore.case = TRUE)
includeWords <- grepl("^([A-Z]|'|-|)+$", smpClean2WordTotals$word, ignore.case = TRUE)
smpClean2WordTotals <- smpClean2WordTotals %>%
  subset(includeTerms == TRUE & includeWords == TRUE & freq > 0)

smpClean2WordTotals <- aggregate(smpClean2WordTotals$freq, by=list(term = smpClean2WordTotals$term, word = smpClean2WordTotals$word), FUN=sum)
colnames(smpClean2WordTotals)[3] <- "freq"


smpClean3Words <- unnest_tokens(smpText, terms, text, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
smpClean3WordTotals <- data.frame(table(smpClean3Words$terms), stringsAsFactors = FALSE)
names(smpClean3WordTotals) <- c("term","freq")
smpClean3WordTotals <- smpClean3WordTotals %>% separate(col = term, into = c("word_1","word_2","word"), sep = " ")

smpClean3WordTotals$word_1 <- replace_word_elongation(smpClean3WordTotals$word_1)
smpClean3WordTotals$word_1 <- replace_names(smpClean3WordTotals$word_1, replacement = "<<NAME>>")
smpClean3WordTotals$word_1 <- replace_time(smpClean3WordTotals$word_1, replacement = "<<TIME>>")
smpClean3WordTotals$word_1 <- gsub("[\\.|,]", "", smpClean3WordTotals$word_1)
smpClean3WordTotals$word_1 <- gsub("[0-9]+", "<<NUMBER>>", smpClean3WordTotals$word_1)
smpClean3WordTotals$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", smpClean3WordTotals$word_1)

smpClean3WordTotals$word_2 <- replace_word_elongation(smpClean3WordTotals$word_2)
smpClean3WordTotals$word_2 <- replace_names(smpClean3WordTotals$word_2, replacement = "<<NAME>>")
smpClean3WordTotals$word_2 <- replace_time(smpClean3WordTotals$word_2, replacement = "<<TIME>>")
smpClean3WordTotals$word_2 <- gsub("[\\.|,]", "", smpClean3WordTotals$word_2)
smpClean3WordTotals$word_2 <- gsub("[0-9]+", "<<NUMBER>>", smpClean3WordTotals$word_2)
smpClean3WordTotals$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", smpClean3WordTotals$word_2)

includeWord_1 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", smpClean3WordTotals$word_1, ignore.case = TRUE)
includeWord_2 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", smpClean3WordTotals$word_2, ignore.case = TRUE)
includeWords <- grepl("^([A-Z]|'|-|)+$", smpClean3WordTotals$word, ignore.case = TRUE)
smpClean3WordTotals <- smpClean3WordTotals %>%
  subset(includeWord_1 == TRUE & includeWord_2 == TRUE & includeWords == TRUE & freq > 0)

smpClean3WordTotals <- aggregate(smpClean3WordTotals$freq, by=list(word_1 = smpClean3WordTotals$word_1, word_2 = smpClean3WordTotals$word_2, word = smpClean3WordTotals$word), FUN=sum)
colnames(smpClean3WordTotals)[4] <- "freq"


smpClean4Words <- unnest_tokens(smpText, terms, text, token = "ngrams", n = 4, to_lower = TRUE, drop = FALSE)
smpClean4WordTotals <- data.frame(table(smpClean4Words$terms), stringsAsFactors = FALSE)
names(smpClean4WordTotals) <- c("term","freq")
smpClean4WordTotals <- smpClean4WordTotals %>% separate(col = term, into = c("word_1","word_2","word_3","word"), sep = " ")

smpClean4WordTotals$word_1 <- replace_word_elongation(smpClean4WordTotals$word_1)
smpClean4WordTotals$word_1 <- replace_names(smpClean4WordTotals$word_1, replacement = "<<NAME>>")
smpClean4WordTotals$word_1 <- replace_time(smpClean4WordTotals$word_1, replacement = "<<TIME>>")
smpClean4WordTotals$word_1 <- gsub("[\\.|,]", "", smpClean4WordTotals$word_1)
smpClean4WordTotals$word_1 <- gsub("[0-9]+", "<<NUMBER>>", smpClean4WordTotals$word_1)
smpClean4WordTotals$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", smpClean4WordTotals$word_1)

smpClean4WordTotals$word_2 <- replace_word_elongation(smpClean4WordTotals$word_2)
smpClean4WordTotals$word_2 <- replace_names(smpClean4WordTotals$word_2, replacement = "<<NAME>>")
smpClean4WordTotals$word_2 <- replace_time(smpClean4WordTotals$word_2, replacement = "<<TIME>>")
smpClean4WordTotals$word_2 <- gsub("[\\.|,]", "", smpClean4WordTotals$word_2)
smpClean4WordTotals$word_2 <- gsub("[0-9]+", "<<NUMBER>>", smpClean4WordTotals$word_2)
smpClean4WordTotals$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", smpClean4WordTotals$word_2)

smpClean4WordTotals$word_3 <- replace_word_elongation(smpClean4WordTotals$word_3)
smpClean4WordTotals$word_3 <- replace_names(smpClean4WordTotals$word_3, replacement = "<<NAME>>")
smpClean4WordTotals$word_3 <- replace_time(smpClean4WordTotals$word_3, replacement = "<<TIME>>")
smpClean4WordTotals$word_3 <- gsub("[\\.|,]", "", smpClean4WordTotals$word_3)
smpClean4WordTotals$word_3 <- gsub("[0-9]+", "<<NUMBER>>", smpClean4WordTotals$word_3)
smpClean4WordTotals$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", smpClean4WordTotals$word_3)

includeWord_1 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", smpClean4WordTotals$word_1, ignore.case = TRUE)
includeWord_2 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", smpClean4WordTotals$word_2, ignore.case = TRUE)
includeWord_3 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", smpClean4WordTotals$word_3, ignore.case = TRUE)
includeWords <- grepl("^([A-Z]|'|-|)+$", smpClean4WordTotals$word, ignore.case = TRUE)
smpClean4WordTotals <- smpClean4WordTotals %>%
  subset(includeWord_1 == TRUE & includeWord_2 == TRUE & includeWord_3 == TRUE & includeWords == TRUE & freq > 0)

smpClean4WordTotals <- aggregate(smpClean4WordTotals$freq, by=list(word_1 = smpClean4WordTotals$word_1, word_2 = smpClean4WordTotals$word_2, word_3 = smpClean4WordTotals$word_3, word = smpClean4WordTotals$word), FUN=sum)
colnames(smpClean4WordTotals)[5] <- "freq"