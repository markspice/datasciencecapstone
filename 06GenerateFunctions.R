##Function for initial cleaning of input data

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

##Function for splitting input data and extracting last three words

splitText3 <- function(x) {
  
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
  
  test
  
}

##Function for splitting input data and extracting last two words

splitText2 <- function(x) {
  
  spaces <- str_locate_all(pattern =' ', x)
  wordNos <- length(spaces[[1]][,1])+1
  termLngth <- nchar(x)
  
  if (wordNos > 2) {
    
    s <- spaces[[1]][wordNos-2]
    test <- data.frame(term = substr(x,s+1,termLngth)) %>% 
      separate(col = term, into = c("word_1","word_2"), sep = " ")
    
  } else if (wordNos == 1) {
    
    test <- data.frame(word_1 = "<unkn>", word_2 = x)
    
  } else {
    
    test <- data.frame(word_1 = "<unkn>", word_2 = "<unkn>")
  }
  
  test
  
}

##Function for splitting input data and extracting last word

splitText1 <- function(x) {
  
  spaces <- str_locate_all(pattern =' ', x)
  wordNos <- length(spaces[[1]][,1])+1
  termLngth <- nchar(x)
  
  if (wordNos > 1) {
    
    s <- spaces[[1]][wordNos-1]
    test <- data.frame(term = substr(x,s+1,termLngth)) %>% 
      separate(col = term, into = c("word_1"), sep = " ")
    
  } else {
    
    test <- data.frame(word_1 = "<unkn>")
  }
  
  test
  
}

##Function for splitting input data and extracting last three words before undertaking additional cleaning

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

##Function for splitting input data and extracting last two words before undertaking additional cleaning

splitCleanText2 <- function(x) {
  
  x <- replace_word_elongation(x)
  
  spaces <- str_locate_all(pattern =' ', x)
  wordNos <- length(spaces[[1]][,1])+1
  termLngth <- nchar(x)
  
  if (wordNos > 2) {
    
    s <- spaces[[1]][wordNos-2]
    test <- data.frame(term = substr(x,s+1,termLngth)) %>% 
      separate(col = term, into = c("word_1","word_2"), sep = " ")
    
  } else if (wordNos == 1) {
    
    test <- data.frame(word_1 = "<unkn>", word_2 = x)
    
  } else {
    
    test <- data.frame(word_1 = "<unkn>", word_2 = "<unkn>")
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
  
  test
  
}

##Function for splitting input data and extracting last word before undertaking additional cleaning

splitCleanText1 <- function(x) {
  
  x <- replace_word_elongation(x)
  
  spaces <- str_locate_all(pattern =' ', x)
  wordNos <- length(spaces[[1]][,1])+1
  termLngth <- nchar(x)
  
  if (wordNos > 1) {
    
    s <- spaces[[1]][wordNos-1]
    test <- data.frame(term = substr(x,s+1,termLngth)) %>% 
      separate(col = term, into = c("word_1"), sep = " ")
    
  } else {
    
    test <- data.frame(word_1 = "<unkn>")
  }
  
  test$word_1 <- replace_names(test$word_1, replacement = "<<NAME>>")
  test$word_1 <- replace_time(test$word_1, replacement = "<<TIME>>")
  test$word_1 <- gsub("[\\.|,]", "", test$word_1)
  test$word_1 <- gsub("[0-9]+", "<<NUMBER>>", test$word_1)
  test$word_1 <- gsub("<<NUMBER>>.*<<NUMBER>>", "<<NUMBER>>", test$word_1)
  
  test
  
}

##Function for running model 3 with maximum 3 word input

mod3_3 <- function(inputText) {
  
  text <- cleanText(inputText)
  test <- splitText3(text)
  
  r31 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[1]
  r32 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[2]
  r33 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[3]
  
  r21 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[1]
  r22 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[2]
  r23 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[3]
  
  r11 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)))[1]
  r12 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)))[2]
  r13 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)))[3]
  
  r01 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)))[3]
  
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

##Function for running model 3 with maximum 2 word input

mod3_2 <- function(inputText) {
  
  text <- cleanText(inputText)
  test <- splitText2(text)
  
  r21 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1]],3)))[1]
  r22 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1]],3)))[2]
  r23 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1]],3)))[3]
  
  r11 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1]],3)))[1]
  r12 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1]],3)))[2]
  r13 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1]],3)))[3]
  
  r01 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test$result2_1[1] <- r21
  } else if (is.na(r11) == FALSE) {
    test$result2_1[1] <- r11
  } else {
    test$result2_1[1] <- r01
  }
  if (is.na(r22) == FALSE) {
    test$result2_2[1] <- r22
  } else if (is.na(r11) == FALSE & r11 != test$result2_1[1]) {
    test$result2_2[1] <- r11 
  } else if (is.na(r12) == FALSE) {
    test$result2_2[1] <- r12
  } else if (r01 != test$result2_1[1]) {
    test$result2_2[1] <- r01
  } else {
    test$result2_2[1] <- r02
  }
  if (is.na(r23) == FALSE) {
    test$result2_3[1] <- r23
  } else if (is.na(r11) == FALSE & r11 != test$result2_1[1] & r11 != test$result2_2[1]) {
    test$result2_3[1] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test$result2_1[1] & r12 != test$result2_2[1]) {
    test$result2_3[1] <- r12
  } else if (is.na(r13) == FALSE) {
    test$result2_3[1] <- r13
  } else if (r01 != test$result2_1[1] & r01 != test$result2_2[1]) {
    test$result2_3[1] <- r01
  } else if (r02 != test$result2_1[1] & r02 != test$result2_2[1]) {
    test$result2_3[1] <- r02
  } else {
    test$result2_3[1] <- r03
  }
  
  test[,c("result2_1","result2_2","result2_3")]
}

##Function for running model 3 with maximum 1 word input

mod3_1 <- function(inputText) {
  
  text <- cleanText(inputText)
  test <- splitText1(text)
  
  r11 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1]],3)))[1]
  r12 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1]],3)))[2]
  r13 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1]],3)))[3]
  
  r01 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    test$result1_1[1] <- r11
  } else {
    test$result1_1[1] <- r01
  }
  if (is.na(r12) == FALSE) {
    test$result1_2[1] <- r12
  } else if (r01 != test$result1_1[1]) {
    test$result1_2[1] <- r01
  } else {
    test$result1_2[1] <- r02
  }
  if (is.na(r13) == FALSE) {
    test$result1_3[1] <- r13
  } else if (r01 != test$result1_1[1] & r01 != test$result1_2[1]) {
    test$result1_3[1] <- r01
  } else if (r02 != test$result1_1[1] & r02 != test$result1_2[1]) {
    test$result1_3[1] <- r02
  } else {
    test$result1_3[1] <- r03
  }
  
  test[,c("result1_1","result1_2","result1_3")]
}

##Function for running model c3 with maximum 3 word input

mod3c_3 <- function(inputText) {
  
  text <- cleanText(inputText)
  test <- splitCleanText3(text)
  
  r31 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[1]
  r32 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[2]
  r33 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[3]
  
  r21 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[1]
  r22 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[2]
  r23 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1] & df33g$word_3 == test$word_3[1]],3)))[3]
  
  r11 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)))[1]
  r12 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)))[2]
  r13 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == test$word_3[1]],3)))[3]
  
  r01 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>" & df33g$word_3 == "<unkn>"],3)))[3]
  
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

##Function for running model c3 with maximum 2 word input

mod3c_2 <- function(inputText) {
  
  text <- cleanText(inputText)
  test <- splitCleanText2(text)
  
  r21 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1]],3)))[1]
  r22 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1]],3)))[2]
  r23 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1] & df33g$word_2 == test$word_2[1]],3)))[3]
  
  r11 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1]],3)))[1]
  r12 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1]],3)))[2]
  r13 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == test$word_2[1]],3)))[3]
  
  r01 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>" & df33g$word_2 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test$result2_1[1] <- r21
  } else if (is.na(r11) == FALSE) {
    test$result2_1[1] <- r11
  } else {
    test$result2_1[1] <- r01
  }
  if (is.na(r22) == FALSE) {
    test$result2_2[1] <- r22
  } else if (is.na(r11) == FALSE & r11 != test$result2_1[1]) {
    test$result2_2[1] <- r11 
  } else if (is.na(r12) == FALSE) {
    test$result2_2[1] <- r12
  } else if (r01 != test$result2_1[1]) {
    test$result2_2[1] <- r01
  } else {
    test$result2_2[1] <- r02
  }
  if (is.na(r23) == FALSE) {
    test$result2_3[1] <- r23
  } else if (is.na(r11) == FALSE & r11 != test$result2_1[1] & r11 != test$result2_2[1]) {
    test$result2_3[1] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test$result2_1[1] & r12 != test$result2_2[1]) {
    test$result2_3[1] <- r12
  } else if (is.na(r13) == FALSE) {
    test$result2_3[1] <- r13
  } else if (r01 != test$result2_1[1] & r01 != test$result2_2[1]) {
    test$result2_3[1] <- r01
  } else if (r02 != test$result2_1[1] & r02 != test$result2_2[1]) {
    test$result2_3[1] <- r02
  } else {
    test$result2_3[1] <- r03
  }
  
  test[,c("result2_1","result2_2","result2_3")]
}

##Function for running model c3 with maximum 1 word input

mod3c_1 <- function(inputText) {
  
        r11 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1]],3)))[1]
        r12 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1]],3)))[2]
        r13 <- as.character(paste(head(df33g$word[df33g$word_1 == test$word_1[1]],3)))[3]
        
        r01 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>"],3)))[1]
        r02 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>"],3)))[2]
        r03 <- as.character(paste(head(df33g$word[df33g$word_1 == "<unkn>"],3)))[3]
        
  if (is.na(r11) == FALSE) {
    test$result1_1[1] <- r11
  } else {
    test$result1_1[1] <- r01
  }
  if (is.na(r12) == FALSE) {
    test$result1_2[1] <- r12
  } else if (r01 != test$result1_1[1]) {
    test$result1_2[1] <- r01
  } else {
    test$result1_2[1] <- r02
  }
  if (is.na(r13) == FALSE) {
    test$result1_3[1] <- r13
  } else if (r01 != test$result1_1[1] & r01 != test$result1_2[1]) {
    test$result1_3[1] <- r01
  } else if (r02 != test$result1_1[1] & r02 != test$result1_2[1]) {
    test$result1_3[1] <- r02
  } else {
    test$result1_3[1] <- r03
  }
  
  test[,c("result1_1","result1_2","result1_3")]
}