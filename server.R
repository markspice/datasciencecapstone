library(shiny)
library(textclean)
library(dplyr)
library(repmis)
library(stringr)
library(tidyr)
library(tidytext)

## Function for cleaning input text

cleanText <- function(x) {
        
        text <- gsub("[^[:alnum:][:space:]'-]", "", x)
        
        a <- c(" isnt ", " hasnt ", " hadnt ", " didnt ", " wouldnt ", " couldnt ", " shouldnt ", " shes ", " theres ", " whos ", " dont ", " youd ", " hed ", " theyd ", " ive ", " youve ", " weve ", " theyve ", " youre ", " theyre ")
        b <- c(" is not ", " has not ", " had not ", " did not ", " would not ", " could not ", " should not ", " she is ", " there is ", " who is ", " do not ", " you would ", " he would ", " they would ", " i have ", " you have ", " we have ", " they have ", " you are ", " they are ")
        for(i in seq_along(a)) text <- gsub(a[i], b[i], text, fixed = TRUE)
        
        a <- c("^isnt ", "^hasnt ", "^hadnt ", "^didnt ", "^wouldnt ", "^couldnt ", "^shouldnt ", "^shes ", "^theres ", "^whos ", "^dont ", "^youd ", "^hed ", "^theyd ", "^ive ", "^youve ", "^weve ", "^theyve ", "^youre ", "^theyre ")
        b <- c("^is not ", "^has not ", "^had not ", "^did not ", "^would not ", "^could not ", "^should not ", "^she is ", "^there is ", "^who is ", "^do not ", "^you would ", "^he would ", "^they would ", "^i have ", "^you have ", "^we have ", "^they have ", "^you are ", "^they are ")
        for(i in seq_along(a)) text <- gsub(a[i], b[i], text, fixed = TRUE)
        
        a <- c(" isnt$", " hasnt$", " hadnt$", " didnt$", " wouldnt$", " couldnt$", " shouldnt$", " shes$", " theres$", " whos$", " dont$", " youd$", " hed$", " theyd$", " ive$", " youve$", " weve$", " theyve$", " youre$", " theyre$")
        b <- c(" is not$", " has not$", " had not$", " did not$", " would not$", " could not$", " should not$", " she is$", " there is$", " who is$", " do not$", " you would$", " he would$", " they would$", " i have$", " you have$", " we have$", " they have$", " you are$", " they are$")
        for(i in seq_along(a)) text <- gsub(a[i], b[i], text, fixed = TRUE)
        
        text <- replace_contraction(text)
        text <- replace_ordinal(text)
        text <- replace_white(text)
        
        text
        
}

##Function for splitting input text into individual words (maximum last three) and replacing 
##names and numbers with generics

splitCleanText3 <- function(x) {
        
        x <- replace_word_elongation(x)
        
        spaces <- str_locate_all(pattern =' ', x)
        wordNos <- length(spaces[[1]][,1])+1
        termLngth <- nchar(x)
        
        if (wordNos > 3) {
                
                s <- spaces[[1]][wordNos-3]
                test <- data.frame(term = substr(x,s+1,termLngth)) %>% 
                        separate(col = term, into = c("word_1","word_2","word_3"), sep = " ")
                
        } else if (wordNos == 2) {
                
                test <- data.frame(term = x, word_1 = "<unkn>") %>% 
                        separate(col = term, into = c("word_2","word_3"), sep = " ")
                
        } else if (wordNos == 1) {
                
                test <- data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = x)
                
        } else {
                
                test <- data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>")
        }
        
        test$word_1 <- replace_names(test$word_1, replacement = "<<NAME>>")
        test$word_1 <- replace_time(test$word_1, replacement = "<<TIME>>")
        test$word_1 <- gsub("[\\.|,]", "", test$word_1)
        test$word_1 <- gsub("[0-9]+", "<<NUMBER>>", test$word_1)
        test$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", test$word_1)
        
        test$word_2 <- replace_names(test$word_2, replacement = "<<NAME>>")
        test$word_2 <- replace_time(test$word_2, replacement = "<<TIME>>")
        test$word_2 <- gsub("[\\.|,]", "", test$word_2)
        test$word_2 <- gsub("[0-9]+", "<<NUMBER>>", test$word_2)
        test$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", test$word_2)
        
        test$word_3 <- replace_names(test$word_3, replacement = "<<NAME>>")
        test$word_3 <- replace_time(test$word_3, replacement = "<<TIME>>")
        test$word_3 <- gsub("[\\.|,]", "", test$word_3)
        test$word_3 <- gsub("[0-9]+", "<<NUMBER>>", test$word_3)
        test$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", test$word_3)
        
        test
}

##Function for generating next word predictions

mod3c_3 <- function(inputText, pastText, ngramTable) {
        
        text <- paste(pastText, cleanText(inputText), sep = " ")
        test <- splitCleanText3(text)
        df33g <- ngramTable
        
        r31 <- head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)[1]
        r32 <- head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)[2]
        r33 <- head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)[3]
        
        r21 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)[1]
        r22 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)[2]
        r23 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)[3]

        r11 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)[1]
        r12 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)[2]
        r13 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)[3]
        
        r01 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)[1]
        r02 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)[2]
        r03 <- head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)[3]
        
        if (is.na(r31) == FALSE) {
                test$result3_1[1] <- r31
        } else if (is.na(r21) == FALSE) {
                test$result3_1[1] <- r21
        } else if (is.na(r11) == FALSE) {
                test$result3_1[1] <- r11
        } else {
                test$result3_1[1] <- r01
        }
        if (is.na(r32) == FALSE) {
                test$result3_2[1] <- r32
        } else if (is.na(r21) == FALSE & r21 != test$result3_1[1]) {
                test$result3_2[1] <- r21 
        } else if (is.na(r22) == FALSE) {
                test$result3_2[1] <- r22
        } else if (is.na(r11) == FALSE & r11 != test$result3_1[1]) {
                test$result3_2[1] <- r11 
        } else if (is.na(r12) == FALSE) {
                test$result3_2[1] <- r12
        } else if (r01 != test$result3_1[1]) {
                test$result3_2[1] <- r01
        } else {
                test$result3_2[1] <- r02
        }
        if (is.na(r33) == FALSE) {
                test$result3_3[1] <- r33
        } else if (is.na(r21) == FALSE & r21 != test$result3_1[1] & r21 != test$result3_2[1]) {
                test$result3_3[1] <- r21 
        } else if (is.na(r22) == FALSE & r22 != test$result3_1[1] & r22 != test$result3_2[1]) {
                test$result3_3[1] <- r22
        } else if (is.na(r23) == FALSE) {
                test$result3_3[1] <- r23
        } else if (is.na(r11) == FALSE & r11 != test$result3_1[1] & r11 != test$result3_2[1]) {
                test$result3_3[1] <- r11 
        } else if (is.na(r12) == FALSE & r12 != test$result3_1[1] & r12 != test$result3_2[1]) {
                test$result3_3[1] <- r12
        } else if (is.na(r13) == FALSE) {
                test$result3_3[1] <- r13
        } else if (r01 != test$result3_1[1] & r01 != test$result3_2[1]) {
                test$result3_3[1] <- r01
        } else if (r02 != test$result3_1[1] & r02 != test$result3_2[1]) {
                test$result3_3[1] <- r02
        } else {
                test$result3_3[1] <- r03
        }
        
        test[,c("result3_1","result3_2","result3_3")]
        
}

##Function for generating new ngrams from inputted text

newNgrams <- function(x) {
        
        newText <- x
        
        #New words newText
        
        newWords <- unnest_tokens(newText, words, text, token = "words", to_lower = TRUE, drop = TRUE)
        includeWords <- grepl("^([A-Z]|'|-)+$", newWords$words, ignore.case = TRUE)
        newWordTotals <- data.frame(table(newWords$words[includeWords == TRUE]), stringsAsFactors = FALSE)
        names(newWordTotals) <- c("word","wordFreq")
        
        #New 2-grams
        
        if (sum(newWordTotals$wordFreq) >= 2) {
                
                new2Words <- unnest_tokens(newText, terms, text, token = "ngrams", n = 2, to_lower = TRUE, drop = FALSE)
                new2WordTotals <- data.frame(table(new2Words$terms), stringsAsFactors = FALSE)
                names(new2WordTotals) <- c("term","freq")
                new2WordTotals <- new2WordTotals %>% separate(col = term, into = c("term","word"), sep = " ")
                
                new2WordTotals$term <- replace_word_elongation(new2WordTotals$term)
                new2WordTotals$term <- replace_names(new2WordTotals$term, replacement = "<<NAME>>")
                new2WordTotals$term <- replace_time(new2WordTotals$term, replacement = "<<TIME>>")
                new2WordTotals$term <- gsub("[0-9][\\.|,][0-9]", "0", new2WordTotals$term)
                new2WordTotals$term <- gsub("([0-9]+)", "<<NUMBER>>", new2WordTotals$term)
                new2WordTotals$term <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", new2WordTotals$term)
                
                includeTerms <- grepl("^([A-Z]|'|-|<<.*>>)+$", new2WordTotals$term, ignore.case = TRUE)
                includeWords <- grepl("^([A-Z]|'|-|)+$", new2WordTotals$word, ignore.case = TRUE)
                new2WordTotals <- new2WordTotals %>%
                        subset(includeTerms == TRUE & includeWords == TRUE & freq > 0)
                
                new2WordTotals <- aggregate(new2WordTotals$freq, by=list(term = new2WordTotals$term, word = new2WordTotals$word), FUN=sum)
                colnames(new2WordTotals)[3] <- "freq"        
        }
 
        #New 3-Gram
        
        if (sum(newWordTotals$wordFreq) >= 3) {
                
                new3Words <- unnest_tokens(newText, terms, text, token = "ngrams", n = 3, to_lower = TRUE, drop = FALSE)
                new3WordTotals <- data.frame(table(new3Words$terms), stringsAsFactors = FALSE)
                names(new3WordTotals) <- c("term","freq")
                new3WordTotals <- new3WordTotals %>% separate(col = term, into = c("word_1","word_2","word"), sep = " ")
                
                new3WordTotals$word_1 <- replace_word_elongation(new3WordTotals$word_1)
                new3WordTotals$word_1 <- replace_names(new3WordTotals$word_1, replacement = "<<NAME>>")
                new3WordTotals$word_1 <- replace_time(new3WordTotals$word_1, replacement = "<<TIME>>")
                new3WordTotals$word_1 <- gsub("[\\.|,]", "", new3WordTotals$word_1)
                new3WordTotals$word_1 <- gsub("[0-9]+", "<<NUMBER>>", new3WordTotals$word_1)
                new3WordTotals$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", new3WordTotals$word_1)
                
                new3WordTotals$word_2 <- replace_word_elongation(new3WordTotals$word_2)
                new3WordTotals$word_2 <- replace_names(new3WordTotals$word_2, replacement = "<<NAME>>")
                new3WordTotals$word_2 <- replace_time(new3WordTotals$word_2, replacement = "<<TIME>>")
                new3WordTotals$word_2 <- gsub("[\\.|,]", "", new3WordTotals$word_2)
                new3WordTotals$word_2 <- gsub("[0-9]+", "<<NUMBER>>", new3WordTotals$word_2)
                new3WordTotals$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", new3WordTotals$word_2)
                
                includeWord_1 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", new3WordTotals$word_1, ignore.case = TRUE)
                includeWord_2 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", new3WordTotals$word_2, ignore.case = TRUE)
                includeWords <- grepl("^([A-Z]|'|-|)+$", new3WordTotals$word, ignore.case = TRUE)
                new3WordTotals <- new3WordTotals %>%
                        subset(includeWord_1 == TRUE & includeWord_2 == TRUE & includeWords == TRUE & freq > 0)
                
                new3WordTotals <- aggregate(new3WordTotals$freq, by=list(word_1 = new3WordTotals$word_1, word_2 = new3WordTotals$word_2, word = new3WordTotals$word), FUN=sum)
                colnames(new3WordTotals)[4] <- "freq"
        }
                
        #New 4-Gram
        
        if (sum(newWordTotals$wordFreq) >= 4) {
                
                new4Words <- unnest_tokens(newText, terms, text, token = "ngrams", n = 4, to_lower = TRUE, drop = FALSE)
                new4WordTotals <- data.frame(table(new4Words$terms), stringsAsFactors = FALSE)
                names(new4WordTotals) <- c("term","freq")
                new4WordTotals <- new4WordTotals %>% separate(col = term, into = c("word_1","word_2","word_3","word"), sep = " ")
                
                new4WordTotals$word_1 <- replace_word_elongation(new4WordTotals$word_1)
                new4WordTotals$word_1 <- replace_names(new4WordTotals$word_1, replacement = "<<NAME>>")
                new4WordTotals$word_1 <- replace_time(new4WordTotals$word_1, replacement = "<<TIME>>")
                new4WordTotals$word_1 <- gsub("[\\.|,]", "", new4WordTotals$word_1)
                new4WordTotals$word_1 <- gsub("[0-9]+", "<<NUMBER>>", new4WordTotals$word_1)
                new4WordTotals$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", new4WordTotals$word_1)
                
                new4WordTotals$word_2 <- replace_word_elongation(new4WordTotals$word_2)
                new4WordTotals$word_2 <- replace_names(new4WordTotals$word_2, replacement = "<<NAME>>")
                new4WordTotals$word_2 <- replace_time(new4WordTotals$word_2, replacement = "<<TIME>>")
                new4WordTotals$word_2 <- gsub("[\\.|,]", "", new4WordTotals$word_2)
                new4WordTotals$word_2 <- gsub("[0-9]+", "<<NUMBER>>", new4WordTotals$word_2)
                new4WordTotals$word_2 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", new4WordTotals$word_2)
                
                new4WordTotals$word_3 <- replace_word_elongation(new4WordTotals$word_3)
                new4WordTotals$word_3 <- replace_names(new4WordTotals$word_3, replacement = "<<NAME>>")
                new4WordTotals$word_3 <- replace_time(new4WordTotals$word_3, replacement = "<<TIME>>")
                new4WordTotals$word_3 <- gsub("[\\.|,]", "", new4WordTotals$word_3)
                new4WordTotals$word_3 <- gsub("[0-9]+", "<<NUMBER>>", new4WordTotals$word_3)
                new4WordTotals$word_3 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", new4WordTotals$word_3)
                
                includeWord_1 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", new4WordTotals$word_1, ignore.case = TRUE)
                includeWord_2 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", new4WordTotals$word_2, ignore.case = TRUE)
                includeWord_3 <- grepl("^([A-Z]|'|-|<<.*>>| )+$", new4WordTotals$word_3, ignore.case = TRUE)
                includeWords <- grepl("^([A-Z]|'|-|)+$", new4WordTotals$word, ignore.case = TRUE)
                new4WordTotals <- new4WordTotals %>%
                        subset(includeWord_1 == TRUE & includeWord_2 == TRUE & includeWord_3 == TRUE & includeWords == TRUE & freq > 0)
                
                new4WordTotals <- aggregate(new4WordTotals$freq, by=list(word_1 = new4WordTotals$word_1, word_2 = new4WordTotals$word_2, word_3 = new4WordTotals$word_3, word = new4WordTotals$word), FUN=sum)
                colnames(new4WordTotals)[5] <- "freq"
        }
        
        if (sum(newWordTotals$wordFreq) >= 4) {
                
                dfc <- rbind(data.frame(word_1 = new4WordTotals$word_1, word_2 = new4WordTotals$word_2, word_3 = new4WordTotals$word_3, word = new4WordTotals$word, termFreq = new4WordTotals$freq),
                             data.frame(word_1 = "<unkn>", word_2 = new3WordTotals$word_1, word_3 = new3WordTotals$word_2, word = new3WordTotals$word, termFreq = new3WordTotals$freq),
                             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = new2WordTotals$term, word = new2WordTotals$word, termFreq = new2WordTotals$freq),
                             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = newWordTotals$word, termFreq = newWordTotals$wordFreq))
                
        } else if (sum(newWordTotals$wordFreq) == 3) {
                
                dfc <- rbind(data.frame(word_1 = "<unkn>", word_2 = new3WordTotals$word_1, word_3 = new3WordTotals$word_2, word = new3WordTotals$word, termFreq = new3WordTotals$freq),
                             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = new2WordTotals$term, word = new2WordTotals$word, termFreq = new2WordTotals$freq),
                             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = newWordTotals$word, termFreq = newWordTotals$wordFreq))
                
        } else if (sum(newWordTotals$wordFreq) == 2) {
                
                dfc <- rbind(data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = new2WordTotals$term, word = new2WordTotals$word, termFreq = new2WordTotals$freq),
                             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = newWordTotals$word, termFreq = newWordTotals$wordFreq))
                
        } else {
                
                dfc <- data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = newWordTotals$word, termFreq = newWordTotals$wordFreq)
        }
        dfc
}

##Function for adding new ngrams to the term frequency model

addNgrams <- function(old,new) {
        
        old_df <- old
        newData <- new
        
        words <- old_df %>% select(word, wordFreq)
        words <- aggregate(words$wordFreq, by = list(word = words$word), FUN = mean)
        colnames(words) <- c("word","termFreq")
        
        newWords <- newData %>% 
                subset(word_1 == "<unkn>" & word_2 == "<unkn>" & word_3 == "<unkn>") %>% 
                select(word, termFreq)
        
        words <- rbind(words, newWords)
        words <- aggregate(words$termFreq, by = list(word = words$word), FUN = sum)
        colnames(words) <- c("word","wordFreq")
        
        new_df <- rbind(select(old_df, word_1, word_2, word_3, word, termFreq), newData)
        new_df <- aggregate(new_df$termFreq, by = list(new_df$word_1, new_df$word_2, new_df$word_3, new_df$word), FUN = sum)
        colnames(new_df) <- c("word_1", "word_2", "word_3", "word","termFreq")
        
        new_df <- merge(x = new_df, y = words, by.x = "word", by.y = "word")
        new_df <- new_df[with(new_df, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]
        
        new_df        
        
}

#Shiny server function for calling the word prediction function

shinyServer(function(input, output, session) {
        
        session$sendCustomMessage(type="refocus",message=list(NULL))
        
        ##Load word frequency data
        
        df33g <- source_data("https://github.com/markspice/datasciencecapstone/blob/master/cleanNGram33b.csv?raw=true", stringsAsFactors = FALSE)
        
        ##Define reactive variables
        
        values <- reactiveValues(df_data = NULL, warning = NULL, text = NULL, df_newTerms = NULL, ngrams = df33g)
        
        #Define button labels reactively from model outputs
        
        label1 <- reactive({
                x    <- mod3c_3(inputText = input$text, pastText = paste(values$df_data, collapse = " "), ngramTable = values$ngrams)
                label1 <- x$result3_1
        })
  
        output$Button1 <- renderUI({
                actionButton("word1", label = label1(), width = "250px")
        })
  
        label2 <- reactive({
                x    <- mod3c_3(inputText = input$text, pastText = paste(values$df_data, collapse = " "), ngramTable = values$ngrams)
                label2 <- x$result3_2
        })
  
        output$Button2 <- renderUI({
                actionButton("word2", label = label2(), width = "250px")
        })
  
        label3 <- reactive({
                x    <- mod3c_3(inputText = input$text, pastText = paste(values$df_data, collapse = " "), ngramTable = values$ngrams)
                label3 <- x$result3_3
        })
  
        output$Button3 <- renderUI({
                actionButton("word3", label = label3(), width = "250px")
        })
        
        ##Create reactive data frame to store inputted text
        
        observeEvent(input$word1, {
                values$df_data <- rbind(values$df_data, paste(input$text, label1(), sep = " "))
                values$warning <- NULL
                shinyjs::reset("text")
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$word2, {
                values$df_data <- rbind(values$df_data, paste(input$text, label2(), sep = " "))
                values$warning <- NULL
                shinyjs::reset("text")
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$word3, {
                values$df_data <- rbind(values$df_data, paste(input$text, label3(), sep = " "))
                values$warning <- NULL
                shinyjs::reset("text")
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$stop, {
                values$df_data <- rbind(values$df_data, paste(input$text, ".", sep = ""))
                values$warning <- NULL
                shinyjs::reset("text")
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$question, {
                values$df_data <- rbind(values$df_data, paste(input$text, "?", sep = ""))
                values$warning <- NULL
                shinyjs::reset("text")
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$exclamation, {
                values$df_data <- rbind(values$df_data, paste(input$text, "!", sep = ""))
                values$warning <- NULL
                shinyjs::reset("text")
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        
        observeEvent(input$text, {
                values$warning <- NULL
        })
        
        ##Actions for deleting some or all of the inputted text
        
        observeEvent(input$deleteLast, {
                n <- length(values$df_data)-1
                if (n < 0) {
                        values$warning <- paste("Nothing to delete!")
                } else if (n == 0) {
                        values$df_data <- values$df_data[0]
                        values$warning <- NULL
                } else if (n > 0) {
                        values$df_data <- values$df_data[1:n]
                        values$warning <- NULL
                }
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$deleteAll, {
                if (length(values$df_data) == 0) {
                        values$warning <- paste("Nothing to delete!")
                } else {
                        values$df_data <- values$df_data[0]  
                        values$warning <- NULL
                }
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        
        ##Call to function to add new text to term frequency table
        
        observeEvent(input$submit, {
                if (length(values$df_data) == 0) {
                        values$warning <- paste("No words to add!")
                } else {
                        values$text <- cleanText(x = paste(values$df_data, collapse = " "))
                        if (nchar(values$text) == 0) {
                                values$warning <- paste("No words to add!")
                        } else {
                                values$df_newTerms <- newNgrams(x = data.frame(text = values$text, stringsAsFactors = FALSE))
                                values$ngrams <- addNgrams(old = values$ngrams, new = values$df_newTerms)
                                values$df_data <- values$df_data[0] 
                                values$warning <- NULL
                        }
                }
                session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        
        output$message <- renderPrint(cat(paste(values$df_data)))
        output$warning <- renderPrint(cat(values$warning))
        
})
