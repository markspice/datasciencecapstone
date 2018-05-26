setwd("C:/Users/Mark/Documents/Coursera/Capstone/")

## Get data

nodir <- FALSE
nozip <- FALSE

if(!file.exists("./final/en_US/en_US.twitter.txt")) {
  nodir <- TRUE
  if (!file.exists("Coursera-SwiftKey.zip")) {
    nozip <- TRUE
    fileurl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(fileurl,"./Coursera-SwiftKey.zip")
  }
  unzip("Coursera-SwiftKey.zip")
}
setwd("./final/en_US")

con <- file("en_US.twitter.txt", "r")
temp <- readLines(con, skipNul = TRUE)
close(con)
len = length(temp)
set.seed(10101)
smp <- rbinom(len, 1, 0.1)
sampleTwitterEn <- temp[smp == 1]
rm("temp")
rm("smp")

## Read data

con <- file("en_US.blogs.txt", "r")
blogs <- readLines(con, skipNul = TRUE, encoding = "UTF-8")
close(con)

con <- file("en_US.news.txt", "r", encoding = "UTF-8")
news <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.twitter.txt", "r", encoding = "UTF-8")
twitter <- readLines(con, skipNul = TRUE)
close(con)

allText <- rbind(data.frame(source = "blogs", text = blogs, stringsAsFactors = FALSE),
                 data.frame(source = "news", text = news, stringsAsFactors = FALSE),
                 data.frame(source = "twitter", text = twitter, stringsAsFactors = FALSE))
allText$source <- as.factor(allText$source)

##Generate train and test sample texts and undertake initial cleaning

len = length(allText$text)
set.seed(10101)
smp <- rbinom(len, 1, 0.01)
smpText <- allText[smp == 1,]

smpText$text <- gsub("[^[:alnum:][:space:]'-]", "", smpText$text)

a <- c(" isnt ", " hasnt ", " hadnt ", " didnt ", " wouldnt ", " couldnt ", " shouldnt ", " shes ", " theres ", " whos ", " dont ", " youd ", " hed ", " theyd ", " ive ", " youve ", " weve ", " theyve ", " youre ", " theyre ")
b <- c(" is not ", " has not ", " had not ", " did not ", " would not ", " could not ", " should not ", " she is ", " there is ", " who is ", " do not ", " you would ", " he would ", " they would ", " i have ", " you have ", " we have ", " they have ", " you are ", " they are ")
for(i in seq_along(a)) smpText$text <- gsub(a[i], b[i], smpText$text, fixed = TRUE)

a <- c("^isnt ", "^hasnt ", "^hadnt ", "^didnt ", "^wouldnt ", "^couldnt ", "^shouldnt ", "^shes ", "^theres ", "^whos ", "^dont ", "^youd ", "^hed ", "^theyd ", "^ive ", "^youve ", "^weve ", "^theyve ", "^youre ", "^theyre ")
b <- c("^is not ", "^has not ", "^had not ", "^did not ", "^would not ", "^could not ", "^should not ", "^she is ", "^there is ", "^who is ", "^do not ", "^you would ", "^he would ", "^they would ", "^i have ", "^you have ", "^we have ", "^they have ", "^you are ", "^they are ")
for(i in seq_along(a)) smpText$text <- gsub(a[i], b[i], smpText$text, fixed = TRUE)

a <- c(" isnt$", " hasnt$", " hadnt$", " didnt$", " wouldnt$", " couldnt$", " shouldnt$", " shes$", " theres$", " whos$", " dont$", " youd$", " hed$", " theyd$", " ive$", " youve$", " weve$", " theyve$", " youre$", " theyre$")
b <- c(" is not$", " has not$", " had not$", " did not$", " would not$", " could not$", " should not$", " she is$", " there is$", " who is$", " do not$", " you would$", " he would$", " they would$", " i have$", " you have$", " we have$", " they have$", " you are$", " they are$")
for(i in seq_along(a)) smpText$text <- gsub(a[i], b[i], smpText$text, fixed = TRUE)

library(textclean)

smpText$text <- replace_contraction(smpText$text)
smpText$text <- replace_ordinal(smpText$text)

test <- allText[smp == 0,]
len = length(test$text)
set.seed(13579)
smp <- rbinom(len, 1, 0.001)
test <- test[smp == 1,]

test$text <- gsub("[^[:alnum:][:space:]'-]", "", test$text)

a <- c(" isnt ", " hasnt ", " hadnt ", " didnt ", " wouldnt ", " couldnt ", " shouldnt ", " shes ", " theres ", " whos ", " dont ", " youd ", " hed ", " theyd ", " ive ", " youve ", " weve ", " theyve ", " youre ", " theyre ")
b <- c(" is not ", " has not ", " had not ", " did not ", " would not ", " could not ", " should not ", " she is ", " there is ", " who is ", " do not ", " you would ", " he would ", " they would ", " i have ", " you have ", " we have ", " they have ", " you are ", " they are ")
for(i in seq_along(a)) test$text <- gsub(a[i], b[i], test$text, fixed = TRUE)

a <- c("^isnt ", "^hasnt ", "^hadnt ", "^didnt ", "^wouldnt ", "^couldnt ", "^shouldnt ", "^shes ", "^theres ", "^whos ", "^dont ", "^youd ", "^hed ", "^theyd ", "^ive ", "^youve ", "^weve ", "^theyve ", "^youre ", "^theyre ")
b <- c("^is not ", "^has not ", "^had not ", "^did not ", "^would not ", "^could not ", "^should not ", "^she is ", "^there is ", "^who is ", "^do not ", "^you would ", "^he would ", "^they would ", "^i have ", "^you have ", "^we have ", "^they have ", "^you are ", "^they are ")
for(i in seq_along(a)) test$text <- gsub(a[i], b[i], test$text, fixed = TRUE)

a <- c(" isnt$", " hasnt$", " hadnt$", " didnt$", " wouldnt$", " couldnt$", " shouldnt$", " shes$", " theres$", " whos$", " dont$", " youd$", " hed$", " theyd$", " ive$", " youve$", " weve$", " theyve$", " youre$", " theyre$")
b <- c(" is not$", " has not$", " had not$", " did not$", " would not$", " could not$", " should not$", " she is$", " there is$", " who is$", " do not$", " you would$", " he would$", " they would$", " i have$", " you have$", " we have$", " they have$", " you are$", " they are$")
for(i in seq_along(a)) test$text <- gsub(a[i], b[i], test$text, fixed = TRUE)

test$text <- replace_contraction(test$text)
test$text <- replace_ordinal(test$text)

#GENERATE N-WORD TERMS

library(tidytext)
library(tidyr)
library(dplyr)

smpWords <- unnest_tokens(smpText, words, text, token = "words", to_lower = TRUE, drop = FALSE)
includeWords <- grepl("^([A-Z]|'|-)+$", smpWords$words, ignore.case = TRUE)
smpWordTotals <- data.frame(table(smpWords$words[includeWords == TRUE], smpWords$source[includeWords == TRUE]), stringsAsFactors = FALSE)
names(smpWordTotals) <- c("word","source","freq")
smpWordTotals$source <- as.factor(smpWordTotals$source)
smpWordTotals <- subset(smpWordTotals, smpWordTotals$freq > 0)

##Single word frequencies from all sources combined:

smpAllSources <- tapply(smpWordTotals$freq, smpWordTotals$word, sum)
smpAllSources <- data.frame(word = names(smpAllSources[!is.na(smpAllSources)]), wordFreq = smpAllSources[!is.na(smpAllSources)])
smpAllSources <- smpAllSources %>% arrange(desc(wordFreq))

##Frequency of sequences of two words:

smp2Words <- unnest_tokens(smpText, terms, text, token = "ngrams", n = 2, to_lower = TRUE, drop = FALSE)
smp2WordTotals <- data.frame(table(smp2Words$terms), stringsAsFactors = FALSE)
names(smp2WordTotals) <- c("term","freq")
includeWords <- grepl("^([A-Z]|'|-| )+$", smp2WordTotals$term, ignore.case = TRUE)
smp2WordTotals <- smp2WordTotals %>%
  subset(includeWords == TRUE & smp2WordTotals$freq > 0) %>%
  separate(col = term, into = c("term","word"), sep = " ")

##Frequency of sequences of three words:

smp3Words <- unnest_tokens(smpText, terms, text, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
smp3WordTotals <- data.frame(table(smp3Words$terms), stringsAsFactors = FALSE)
names(smp3WordTotals) <- c("term","freq")
includeWords <- grepl("^([A-Z]|'|-| )+$", smp3WordTotals$term, ignore.case = TRUE)
smp3WordTotals <- smp3WordTotals %>%
  subset(includeWords == TRUE & smp3WordTotals$freq > 0) %>% 
  separate(col = term, into = c("word_1","word_2","word"), sep = " ")

##Frequency of sequences of four words:

smp4Words <- unnest_tokens(smpText, terms, text, token = "ngrams", n = 4, to_lower = TRUE, drop = FALSE)
smp4WordTotals <- data.frame(table(smp4Words$terms), stringsAsFactors = FALSE)
names(smp4WordTotals) <- c("term","freq")
includeWords <- grepl("^([A-Z]|'|-| )+$", smp4WordTotals$term, ignore.case = TRUE)
smp4WordTotals <- smp4WordTotals %>% 
  subset(includeWords == TRUE & smp4WordTotals$freq > 0) %>%
  separate(col = term, into = c("word_1","word_2","word_3","word"), sep = " ")

