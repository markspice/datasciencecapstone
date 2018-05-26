##NULL MODEL Predictions:

#1 word prediction

test3$null <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == "<unkn>"],1)))
test2$null <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == "<unkn>"],1)))
test1$null <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == "<unkn>"],1)))

test3$nullsuccess <- test3$word == test3$null
test2$nullsuccess <- test2$word == test2$null
test1$nullsuccess <- test1$word == test1$null

#3 word prediction

test3$null3_1 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[1]
test2$null3_1 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[1]
test1$null3_1 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[1]
test3$null3_2 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[2]
test2$null3_2 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[2]
test1$null3_2 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[2]
test3$null3_3 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[3]
test2$null3_3 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[3]
test1$null3_3 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[3]

test3$nullsuccess3 <- test3$word == test3$null3_1 | test3$word == test3$null3_2 | test3$word == test3$null3_3
test2$nullsuccess3 <- test2$word == test2$null3_1 | test2$word == test2$null3_2 | test2$word == test2$null3_3
test1$nullsuccess3 <- test1$word == test1$null3_1 | test1$word == test1$null3_2 | test1$word == test1$null3_3

# Clean 3 word prediction

cleanTest3$null3_1 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[1]
cleanTest2$null3_1 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[1]
cleanTest1$null3_1 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[1]
cleanTest3$null3_2 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[2]
cleanTest2$null3_2 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[2]
cleanTest1$null3_2 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[2]
cleanTest3$null3_3 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[3]
cleanTest2$null3_3 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[3]
cleanTest1$null3_3 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[3]

cleanTest3$nullsuccess3 <- cleanTest3$word == cleanTest3$null3_1 | cleanTest3$word == cleanTest3$null3_2 | cleanTest3$word == cleanTest3$null3_3
cleanTest2$nullsuccess3 <- cleanTest2$word == cleanTest2$null3_1 | cleanTest2$word == cleanTest2$null3_2 | cleanTest2$word == cleanTest2$null3_3
cleanTest1$nullsuccess3 <- cleanTest1$word == cleanTest1$null3_1 | cleanTest1$word == cleanTest1$null3_2 | cleanTest1$word == cleanTest1$null3_3

##MODEL 1 predictions:

for (i in 1:length(test3$word)) {
  if (length(df1$word[df1$word_1 == test3$word_1[i] & df1$word_2 == test3$word_2[i] & df1$word_3 == test3$word_3[i]]) > 0) {
    test3$result[i] <- as.character(paste(head(df1$word[df1$word_1 == test3$word_1[i] & df1$word_2 == test3$word_2[i] & df1$word_3 == test3$word_3[i]],1)))
  } else if (length(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == test3$word_2[i] & df1$word_3 == test3$word_3[i]]) > 0) {
    test3$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == test3$word_2[i] & df1$word_3 == test3$word_3[i]],1)))
  } else if (length(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == test3$word_3[i]]) > 0) {
    test3$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == test3$word_3[i]],1)))
  } else {
    test3$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == "<unkn>"],1)))
  }
}

for (i in 1:length(test2$word)) {
  if (length(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == test2$word_2[i] & df1$word_3 == test2$word_3[i]]) > 0) {
    test2$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == test2$word_2[i] & df1$word_3 == test2$word_3[i]],1)))
  } else if (length(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == test2$word_3[i]]) > 0) {
    test2$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == test2$word_3[i]],1)))
  } else {
    test2$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == "<unkn>"],1)))
  }
}

for (i in 1:length(test1$word)) {
  if (length(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == test1$word_3[i]]) > 0) {
    test1$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == test1$word_3[i]],1)))
  } else {
    test1$result[i] <- as.character(paste(head(df1$word[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>" & df1$word_3 == "<unkn>"],1)))
  }
}

test3$success <- test3$word == test3$result
test2$success <- test2$word == test2$result
test1$success <- test1$word == test1$result

##MODEL 2 Predictions:

for (i in 1:length(test3$word)) {
  if (length(df2$word[df2$word_1 == test3$word_1[i] & df2$word_2 == test3$word_2[i] & df2$word_3 == test3$word_3[i]]) > 0) {
    test3$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == test3$word_1[i] & df2$word_2 == test3$word_2[i] & df2$word_3 == test3$word_3[i]],1)))
  } else if (length(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == test3$word_2[i] & df2$word_3 == test3$word_3[i]]) > 0) {
    test3$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == test3$word_2[i] & df2$word_3 == test3$word_3[i]],1)))
  } else if (length(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == test3$word_3[i]]) > 0) {
    test3$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == test3$word_3[i]],1)))
  } else {
    test3$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == "<unkn>"],1)))
  }
}

for (i in 1:length(test2$word)) {
  if (length(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == test2$word_2[i] & df2$word_3 == test2$word_3[i]]) > 0) {
    test2$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == test2$word_2[i] & df2$word_3 == test2$word_3[i]],1)))
  } else if (length(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == test2$word_3[i]]) > 0) {
    test2$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == test2$word_3[i]],1)))
  } else {
    test2$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == "<unkn>"],1)))
  }
}

for (i in 1:length(test1$word)) {
  if (length(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == test1$word_3[i]]) > 0) {
    test1$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == test1$word_3[i]],1)))
  } else {
    test1$result2[i] <- as.character(paste(head(df2$word[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>" & df2$word_3 == "<unkn>"],1)))
  }
}

test3$success2 <- test3$word == test3$result2
test2$success2 <- test2$word == test2$result2
test1$success2 <- test1$word == test1$result2

##MODEL 3 Predictions:

for (i in 1:length(test3$word)) {
  
  r31 <- as.character(paste(head(df3$word[df3$word_1 == test3$word_1[i] & df3$word_2 == test3$word_2[i] & df3$word_3 == test3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(df3$word[df3$word_1 == test3$word_1[i] & df3$word_2 == test3$word_2[i] & df3$word_3 == test3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(df3$word[df3$word_1 == test3$word_1[i] & df3$word_2 == test3$word_2[i] & df3$word_3 == test3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == test3$word_2[i] & df3$word_3 == test3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == test3$word_2[i] & df3$word_3 == test3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == test3$word_2[i] & df3$word_3 == test3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test3$word_3[i]],3)))[2]
  r13 <- paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test3$word_3[i]],3))[3]
  
  r01 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    test3$result3_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    test3$result3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test3$result3_1[i] <- r11
  } else {
    test3$result3_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    test3$result3_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != test3$result3_1[i]) {
    test3$result3_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    test3$result3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test3$result3_1[i]) {
    test3$result3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test3$result3_2[i] <- r12
  } else if (r01 != test3$result3_1[i]) {
    test3$result3_2[i] <- r01
  } else {
    test3$result3_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    test3$result3_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != test3$result3_1[i] & r21 != test3$result3_2[i]) {
    test3$result3_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != test3$result3_1[i] & r22 != test3$result3_2[i]) {
    test3$result3_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    test3$result3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test3$result3_1[i] & r11 != test3$result3_2[i]) {
    test3$result3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test3$result3_1[i] & r12 != test3$result3_2[i]) {
    test3$result3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test3$result3_3[i] <- r13
  } else if (r01 != test3$result3_1[i] & r01 != test3$result3_2[i]) {
    test3$result3_3[i] <- r01
  } else if (r02 != test3$result3_1[i] & r02 != test3$result3_2[i]) {
    test3$result3_3[i] <- r02
  } else {
    test3$result3_3[i] <- r03
  }

}
  
for (i in 1:length(test2$word)) {
  
  r21 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == test2$word_2[i] & df3$word_3 == test2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == test2$word_2[i] & df3$word_3 == test2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == test2$word_2[i] & df3$word_3 == test2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test2$result3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test2$result3_1[i] <- r11
  } else {
    test2$result3_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    test2$result3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test2$result3_1[i]) {
    test2$result3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test2$result3_2[i] <- r12
  } else if (r01 != test2$result3_1[i]) {
    test2$result3_2[i] <- r01
  } else {
    test2$result3_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    test2$result3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test2$result3_1[i] & r11 != test2$result3_2[i]) {
    test2$result3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test2$result3_1[i] & r12 != test2$result3_2[i]) {
    test2$result3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test2$result3_3[i] <- r13
  } else if (r01 != test2$result3_1[i] & r01 != test2$result3_2[i]) {
    test2$result3_3[i] <- r01
  } else if (r02 != test2$result3_1[i] & r02 != test2$result3_2[i]) {
    test2$result3_3[i] <- r02
  } else {
    test2$result3_3[i] <- r03
  }
  
}

for (i in 1:length(test1$word)) {
        
  r11 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == test1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    test1$result3_1[i] <- r11
  } else {
    test1$result3_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    test1$result3_2[i] <- r12
  } else if (r01 != test1$result3_1[i]) {
    test1$result3_2[i] <- r01
  } else {
    test1$result3_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    test1$result3_3[i] <- r13
  } else if (r01 != test1$result3_1[i] & r01 != test1$result3_2[i]) {
    test1$result3_3[i] <- r01
  } else if (r02 != test1$result3_1[i] & r02 != test1$result3_2[i]) {
    test1$result3_3[i] <- r02
  } else {
    test1$result3_3[i] <- r03
  }
  
}

test3$success3 <- test3$word == test3$result3_1 | test3$word == test3$result3_2 | test3$word == test3$result3_3
test2$success3 <- test2$word == test2$result3_1 | test2$word == test2$result3_2 | test2$word == test2$result3_3
test1$success3 <- test1$word == test1$result3_1 | test1$word == test1$result3_2 | test1$word == test1$result3_3

##MODEL 4 Predictions:

for (i in 1:length(test3$word)) {
  
  rxxx1 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == test3$word_2[i] & df4$word_3 == test3$word_3[i]],3)))[1]
  rxxx2 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == test3$word_2[i] & df4$word_3 == test3$word_3[i]],3)))[2]
  rxxx3 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == test3$word_2[i] & df4$word_3 == test3$word_3[i]],3)))[3]
  
  roxx1 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == test3$word_2[i] & df4$word_3 == test3$word_3[i]],3)))[1]
  roxx2 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == test3$word_2[i] & df4$word_3 == test3$word_3[i]],3)))[2]
  roxx3 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == test3$word_2[i] & df4$word_3 == test3$word_3[i]],3)))[3]
  
  rxox1 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == "<unkn>" & df4$word_3 == test3$word_3[i]],3)))[1]
  rxox2 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == "<unkn>" & df4$word_3 == test3$word_3[i]],3)))[2]
  rxox3 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == "<unkn>" & df4$word_3 == test3$word_3[i]],3)))[3]
  
  rxxo1 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == test3$word_2[i] & df4$word_3 == "<unkn>"],3)))[1]
  rxxo2 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == test3$word_2[i] & df4$word_3 == "<unkn>"],3)))[2]
  rxxo3 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == test3$word_2[i] & df4$word_3 == "<unkn>"],3)))[3]
  
  roox1 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == "<unkn>" & df4$word_3 == test3$word_3[i]],3)))[1]
  roox2 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == "<unkn>" & df4$word_3 == test3$word_3[i]],3)))[2]
  roox3 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == "<unkn>" & df4$word_3 == test3$word_3[i]],3)))[3]
  
  roxo1 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == test3$word_2[i] & df4$word_3 == "<unkn>"],3)))[1]
  roxo2 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == test3$word_2[i] & df4$word_3 == "<unkn>"],3)))[2]
  roxo3 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == test3$word_2[i] & df4$word_3 == "<unkn>"],3)))[3]
  
  rxoo1 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == "<unkn>" & df4$word_3 == "<unkn>"],3)))[1]
  rxoo2 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == "<unkn>" & df4$word_3 == "<unkn>"],3)))[2]
  rxoo3 <- as.character(paste(head(df4$word[df4$word_1 == test3$word_1[i] & df4$word_2 == "<unkn>" & df4$word_3 == "<unkn>"],3)))[3]
  
  rooo1 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == "<unkn>" & df4$word_3 == "<unkn>"],3)))[1]
  rooo2 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == "<unkn>" & df4$word_3 == "<unkn>"],3)))[2]
  rooo3 <- as.character(paste(head(df4$word[df4$word_1 == "<unkn>" & df4$word_2 == "<unkn>" & df4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(rxxx1) == FALSE) {
    test3$result4_1[i] <- rxxx1
  } else if (is.na(roxx1) == FALSE) {
    test3$result4_1[i] <- roxx1
  } else if (is.na(rxox1) == FALSE) {
    test3$result4_1[i] <- rxox1
  } else if (is.na(rxxo1) == FALSE) {
    test3$result4_1[i] <- rxxo1
  } else if (is.na(roox1) == FALSE) {
    test3$result4_1[i] <- roox1
  } else if (is.na(roxo1) == FALSE) {
    test3$result4_1[i] <- roxo1
  } else if (is.na(rxoo1) == FALSE) {
    test3$result4_1[i] <- rxoo1
  } else {
    test3$result4_1[i] <- rooo1
  }
  if (is.na(rxxx2) == FALSE) {
    test3$result4_2[i] <- rxxx2
  } else if (is.na(roxx1) == FALSE & roxx1 != test3$result4_1[i]) {
    test3$result4_2[i] <- roxx1 
  } else if (is.na(roxx2) == FALSE) {
    test3$result4_2[i] <- roxx2
  } else if (is.na(rxox1) == FALSE & rxox1 != test3$result4_1[i]) {
    test3$result4_2[i] <- rxox1 
  } else if (is.na(rxox2) == FALSE) {
    test3$result4_2[i] <- rxox2
  } else if (is.na(rxxo1) == FALSE & rxxo1 != test3$result4_1[i]) {
    test3$result4_2[i] <- rxxo1
  } else if (is.na(rxxo2) == FALSE) {
    test3$result4_2[i] <- rxxo2
  } else if (is.na(roox1) == FALSE & roox1 != test3$result4_1[i]) {
    test3$result4_2[i] <- roox1 
  } else if (is.na(roox2) == FALSE) {
    test3$result4_2[i] <- roox2
  } else if (is.na(roxo1) == FALSE & roxo1 != test3$result4_1[i]) {
    test3$result4_2[i] <- roxo1 
  } else if (is.na(roxo2) == FALSE) {
    test3$result4_2[i] <- roxo2
  } else if (is.na(rxoo1) == FALSE & rxoo1 != test3$result4_1[i]) {
    test3$result4_2[i] <- rxoo1
  } else if (is.na(rxoo2) == FALSE) {
    test3$result4_2[i] <- rxoo2
  } else if (rooo1 != test3$result4_1[i]) {
    test3$result4_2[i] <- rooo1
  } else {
    test3$result4_2[i] <- rooo2
  }
  if (is.na(rxxx3) == FALSE) {
    test3$result4_3[i] <- rxxx3
  } else if (is.na(roxx1) == FALSE & roxx1 != test3$result4_1[i] & roxx1 != test3$result4_2[i]) {
    test3$result4_3[i] <- roxx1 
  } else if (is.na(roxx2) == FALSE & roxx2 != test3$result4_1[i] & roxx2 != test3$result4_2[i]) {
    test3$result4_3[i] <- roxx2
  } else if (is.na(roxx3) == FALSE) {
    test3$result4_3[i] <- roxx3
  } else if (is.na(rxox1) == FALSE & rxox1 != test3$result4_1[i] & rxox1 != test3$result4_2[i]) {
    test3$result4_3[i] <- rxox1 
  } else if (is.na(rxox2) == FALSE & rxox2 != test3$result4_1[i] & rxox2 != test3$result4_2[i]) {
    test3$result4_3[i] <- rxox2
  } else if (is.na(rxox3) == FALSE) {
    test3$result4_3[i] <- rxox3
  } else if (is.na(rxxo1) == FALSE & rxxo1 != test3$result4_1[i] & rxxo1 != test3$result4_2[i]) {
    test3$result4_3[i] <- rxxo1 
  } else if (is.na(rxxo2) == FALSE & rxxo2 != test3$result4_1[i] & rxxo2 != test3$result4_2[i]) {
    test3$result4_3[i] <- rxxo2
  } else if (is.na(rxxo3) == FALSE) {
    test3$result4_3[i] <- rxxo3
  } else if (is.na(roox1) == FALSE & roox1 != test3$result4_1[i] & roox1 != test3$result4_2[i]) {
    test3$result4_3[i] <- roox1 
  } else if (is.na(roox2) == FALSE & roox2 != test3$result4_1[i] & roox2 != test3$result4_2[i]) {
    test3$result4_3[i] <- roox2
  } else if (is.na(roox3) == FALSE) {
    test3$result4_3[i] <- roox3
  } else if (is.na(roxo1) == FALSE & roxo1 != test3$result4_1[i] & roxo1 != test3$result4_2[i]) {
    test3$result4_3[i] <- roxo1 
  } else if (is.na(roxo2) == FALSE & roxo2 != test3$result4_1[i] & roxo2 != test3$result4_2[i]) {
    test3$result4_3[i] <- roxo2
  } else if (is.na(roxo3) == FALSE) {
    test3$result4_3[i] <- roxo3
  } else if (is.na(rxoo1) == FALSE & rxoo1 != test3$result4_1[i] & rxoo1 != test3$result4_2[i]) {
    test3$result4_3[i] <- rxoo1 
  } else if (is.na(rxoo2) == FALSE & rxoo2 != test3$result4_1[i] & rxoo2 != test3$result4_2[i]) {
    test3$result4_3[i] <- rxoo2
  } else if (is.na(rxoo3) == FALSE) {
    test3$result4_3[i] <- rxoo3
  } else if (rooo1 != test3$result4_1[i] & rooo1 != test3$result4_2[i]) {
    test3$result4_3[i] <- rooo1 
  } else if (rooo2 != test3$result4_1[i] & rooo2 != test3$result4_2[i]) {
    test3$result4_3[i] <- rooo2
  } else {
    test3$result4_3[i] <- rooo3
  }
  
}

test3$success4 <- test3$word == test3$result4_1 | test3$word == test3$result4_2 | test3$word == test3$result4_3

##MODEL c3 Predictions:

for (i in 1:length(cleanTest3$word)) {
  
  r31 <- as.character(paste(head(dfc3$word[dfc3$word_1 == cleanTest3$word_1[i] & dfc3$word_2 == cleanTest3$word_2[i] & dfc3$word_3 == cleanTest3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(dfc3$word[dfc3$word_1 == cleanTest3$word_1[i] & dfc3$word_2 == cleanTest3$word_2[i] & dfc3$word_3 == cleanTest3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(dfc3$word[dfc3$word_1 == cleanTest3$word_1[i] & dfc3$word_2 == cleanTest3$word_2[i] & dfc3$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == cleanTest3$word_2[i] & dfc3$word_3 == cleanTest3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == cleanTest3$word_2[i] & dfc3$word_3 == cleanTest3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == cleanTest3$word_2[i] & dfc3$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    cleanTest3$result3_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    cleanTest3$result3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest3$result3_1[i] <- r11
  } else {
    cleanTest3$result3_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    cleanTest3$result3_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3_1[i]) {
    cleanTest3$result3_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    cleanTest3$result3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3_1[i]) {
    cleanTest3$result3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest3$result3_2[i] <- r12
  } else if (r01 != cleanTest3$result3_1[i]) {
    cleanTest3$result3_2[i] <- r01
  } else {
    cleanTest3$result3_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    cleanTest3$result3_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3_1[i] & r21 != cleanTest3$result3_2[i]) {
    cleanTest3$result3_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != cleanTest3$result3_1[i] & r22 != cleanTest3$result3_2[i]) {
    cleanTest3$result3_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    cleanTest3$result3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3_1[i] & r11 != cleanTest3$result3_2[i]) {
    cleanTest3$result3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest3$result3_1[i] & r12 != cleanTest3$result3_2[i]) {
    cleanTest3$result3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest3$result3_3[i] <- r13
  } else if (r01 != cleanTest3$result3_1[i] & r01 != cleanTest3$result3_2[i]) {
    cleanTest3$result3_3[i] <- r01
  } else if (r02 != cleanTest3$result3_1[i] & r02 != cleanTest3$result3_2[i]) {
    cleanTest3$result3_3[i] <- r02
  } else {
    cleanTest3$result3_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest2$word)) {
  
  r21 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == cleanTest2$word_2[i] & dfc3$word_3 == cleanTest2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == cleanTest2$word_2[i] & dfc3$word_3 == cleanTest2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == cleanTest2$word_2[i] & dfc3$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    cleanTest2$result3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest2$result3_1[i] <- r11
  } else {
    cleanTest2$result3_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    cleanTest2$result3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3_1[i]) {
    cleanTest2$result3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest2$result3_2[i] <- r12
  } else if (r01 != cleanTest2$result3_1[i]) {
    cleanTest2$result3_2[i] <- r01
  } else {
    cleanTest2$result3_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    cleanTest2$result3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3_1[i] & r11 != cleanTest2$result3_2[i]) {
    cleanTest2$result3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest2$result3_1[i] & r12 != cleanTest2$result3_2[i]) {
    cleanTest2$result3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest2$result3_3[i] <- r13
  } else if (r01 != cleanTest2$result3_1[i] & r01 != cleanTest2$result3_2[i]) {
    cleanTest2$result3_3[i] <- r01
  } else if (r02 != cleanTest2$result3_1[i] & r02 != cleanTest2$result3_2[i]) {
    cleanTest2$result3_3[i] <- r02
  } else {
    cleanTest2$result3_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest1$word)) {
  
  r11 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == cleanTest1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3$word[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>" & dfc3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    cleanTest1$result3_1[i] <- r11
  } else {
    cleanTest1$result3_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    cleanTest1$result3_2[i] <- r12
  } else if (r01 != cleanTest1$result3_1[i]) {
    cleanTest1$result3_2[i] <- r01
  } else {
    cleanTest1$result3_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    cleanTest1$result3_3[i] <- r13
  } else if (r01 != cleanTest1$result3_1[i] & r01 != cleanTest1$result3_2[i]) {
    cleanTest1$result3_3[i] <- r01
  } else if (r02 != cleanTest1$result3_1[i] & r02 != cleanTest1$result3_2[i]) {
    cleanTest1$result3_3[i] <- r02
  } else {
    cleanTest1$result3_3[i] <- r03
  }
  
}

cleanTest3$success3 <- cleanTest3$word == cleanTest3$result3_1 | cleanTest3$word == cleanTest3$result3_2 | cleanTest3$word == cleanTest3$result3_3
cleanTest2$success3 <- cleanTest2$word == cleanTest2$result3_1 | cleanTest2$word == cleanTest2$result3_2 | cleanTest2$word == cleanTest2$result3_3
cleanTest1$success3 <- cleanTest1$word == cleanTest1$result3_1 | cleanTest1$word == cleanTest1$result3_2 | cleanTest1$word == cleanTest1$result3_3

##MODEL 3b Predictions:

for (i in 1:length(test3$word)) {
  
  r31 <- as.character(paste(head(df3b$word[df3b$word_1 == test3$word_1[i] & df3b$word_2 == test3$word_2[i] & df3b$word_3 == test3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(df3b$word[df3b$word_1 == test3$word_1[i] & df3b$word_2 == test3$word_2[i] & df3b$word_3 == test3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(df3b$word[df3b$word_1 == test3$word_1[i] & df3b$word_2 == test3$word_2[i] & df3b$word_3 == test3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == test3$word_2[i] & df3b$word_3 == test3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == test3$word_2[i] & df3b$word_3 == test3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == test3$word_2[i] & df3b$word_3 == test3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    test3$result3b_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    test3$result3b_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test3$result3b_1[i] <- r11
  } else {
    test3$result3b_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    test3$result3b_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != test3$result3b_1[i]) {
    test3$result3b_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    test3$result3b_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test3$result3b_1[i]) {
    test3$result3b_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test3$result3b_2[i] <- r12
  } else if (r01 != test3$result3b_1[i]) {
    test3$result3b_2[i] <- r01
  } else {
    test3$result3b_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    test3$result3b_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != test3$result3b_1[i] & r21 != test3$result3b_2[i]) {
    test3$result3b_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != test3$result3b_1[i] & r22 != test3$result3b_2[i]) {
    test3$result3b_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    test3$result3b_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test3$result3b_1[i] & r11 != test3$result3b_2[i]) {
    test3$result3b_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test3$result3b_1[i] & r12 != test3$result3b_2[i]) {
    test3$result3b_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test3$result3b_3[i] <- r13
  } else if (r01 != test3$result3b_1[i] & r01 != test3$result3b_2[i]) {
    test3$result3b_3[i] <- r01
  } else if (r02 != test3$result3b_1[i] & r02 != test3$result3b_2[i]) {
    test3$result3b_3[i] <- r02
  } else {
    test3$result3b_3[i] <- r03
  }
  
}

for (i in 1:length(test2$word)) {
  
  r21 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == test2$word_2[i] & df3b$word_3 == test2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == test2$word_2[i] & df3b$word_3 == test2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == test2$word_2[i] & df3b$word_3 == test2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test2$result3b_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test2$result3b_1[i] <- r11
  } else {
    test2$result3b_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    test2$result3b_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test2$result3b_1[i]) {
    test2$result3b_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test2$result3b_2[i] <- r12
  } else if (r01 != test2$result3b_1[i]) {
    test2$result3b_2[i] <- r01
  } else {
    test2$result3b_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    test2$result3b_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test2$result3b_1[i] & r11 != test2$result3b_2[i]) {
    test2$result3b_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test2$result3b_1[i] & r12 != test2$result3b_2[i]) {
    test2$result3b_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test2$result3b_3[i] <- r13
  } else if (r01 != test2$result3b_1[i] & r01 != test2$result3b_2[i]) {
    test2$result3b_3[i] <- r01
  } else if (r02 != test2$result3b_1[i] & r02 != test2$result3b_2[i]) {
    test2$result3b_3[i] <- r02
  } else {
    test2$result3b_3[i] <- r03
  }
  
}

for (i in 1:length(test1$word)) {
  
  r11 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == test1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b$word[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>" & df3b$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    test1$result3b_1[i] <- r11
  } else {
    test1$result3b_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    test1$result3b_2[i] <- r12
  } else if (r01 != test1$result3b_1[i]) {
    test1$result3b_2[i] <- r01
  } else {
    test1$result3b_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    test1$result3b_3[i] <- r13
  } else if (r01 != test1$result3b_1[i] & r01 != test1$result3b_2[i]) {
    test1$result3b_3[i] <- r01
  } else if (r02 != test1$result3b_1[i] & r02 != test1$result3b_2[i]) {
    test1$result3b_3[i] <- r02
  } else {
    test1$result3b_3[i] <- r03
  }
  
}

test3$success3b <- test3$word == test3$result3b_1 | test3$word == test3$result3b_2 | test3$word == test3$result3b_3
test2$success3b <- test2$word == test2$result3b_1 | test2$word == test2$result3b_2 | test2$word == test2$result3b_3
test1$success3b <- test1$word == test1$result3b_1 | test1$word == test1$result3b_2 | test1$word == test1$result3b_3

##MODEL 3b2 Predictions:

for (i in 1:length(test3$word)) {
  
  r31 <- as.character(paste(head(df3b2$word[df3b2$word_1 == test3$word_1[i] & df3b2$word_2 == test3$word_2[i] & df3b2$word_3 == test3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(df3b2$word[df3b2$word_1 == test3$word_1[i] & df3b2$word_2 == test3$word_2[i] & df3b2$word_3 == test3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(df3b2$word[df3b2$word_1 == test3$word_1[i] & df3b2$word_2 == test3$word_2[i] & df3b2$word_3 == test3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == test3$word_2[i] & df3b2$word_3 == test3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == test3$word_2[i] & df3b2$word_3 == test3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == test3$word_2[i] & df3b2$word_3 == test3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    test3$result3b2_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    test3$result3b2_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test3$result3b2_1[i] <- r11
  } else {
    test3$result3b2_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    test3$result3b2_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != test3$result3b2_1[i]) {
    test3$result3b2_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    test3$result3b2_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test3$result3b2_1[i]) {
    test3$result3b2_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test3$result3b2_2[i] <- r12
  } else if (r01 != test3$result3b2_1[i]) {
    test3$result3b2_2[i] <- r01
  } else {
    test3$result3b2_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    test3$result3b2_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != test3$result3b2_1[i] & r21 != test3$result3b2_2[i]) {
    test3$result3b2_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != test3$result3b2_1[i] & r22 != test3$result3b2_2[i]) {
    test3$result3b2_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    test3$result3b2_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test3$result3b2_1[i] & r11 != test3$result3b2_2[i]) {
    test3$result3b2_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test3$result3b2_1[i] & r12 != test3$result3b2_2[i]) {
    test3$result3b2_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test3$result3b2_3[i] <- r13
  } else if (r01 != test3$result3b2_1[i] & r01 != test3$result3b2_2[i]) {
    test3$result3b2_3[i] <- r01
  } else if (r02 != test3$result3b2_1[i] & r02 != test3$result3b2_2[i]) {
    test3$result3b2_3[i] <- r02
  } else {
    test3$result3b2_3[i] <- r03
  }
  
}

for (i in 1:length(test2$word)) {
  
  r21 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == test2$word_2[i] & df3b2$word_3 == test2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == test2$word_2[i] & df3b2$word_3 == test2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == test2$word_2[i] & df3b2$word_3 == test2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test2$result3b2_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test2$result3b2_1[i] <- r11
  } else {
    test2$result3b2_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    test2$result3b2_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test2$result3b2_1[i]) {
    test2$result3b2_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test2$result3b2_2[i] <- r12
  } else if (r01 != test2$result3b2_1[i]) {
    test2$result3b2_2[i] <- r01
  } else {
    test2$result3b2_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    test2$result3b2_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test2$result3b2_1[i] & r11 != test2$result3b2_2[i]) {
    test2$result3b2_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test2$result3b2_1[i] & r12 != test2$result3b2_2[i]) {
    test2$result3b2_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test2$result3b2_3[i] <- r13
  } else if (r01 != test2$result3b2_1[i] & r01 != test2$result3b2_2[i]) {
    test2$result3b2_3[i] <- r01
  } else if (r02 != test2$result3b2_1[i] & r02 != test2$result3b2_2[i]) {
    test2$result3b2_3[i] <- r02
  } else {
    test2$result3b2_3[i] <- r03
  }
  
}

for (i in 1:length(test1$word)) {
  
  r11 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == test1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b2$word[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>" & df3b2$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    test1$result3b2_1[i] <- r11
  } else {
    test1$result3b2_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    test1$result3b2_2[i] <- r12
  } else if (r01 != test1$result3b2_1[i]) {
    test1$result3b2_2[i] <- r01
  } else {
    test1$result3b2_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    test1$result3b2_3[i] <- r13
  } else if (r01 != test1$result3b2_1[i] & r01 != test1$result3b2_2[i]) {
    test1$result3b2_3[i] <- r01
  } else if (r02 != test1$result3b2_1[i] & r02 != test1$result3b2_2[i]) {
    test1$result3b2_3[i] <- r02
  } else {
    test1$result3b2_3[i] <- r03
  }
  
}

test3$success3b2 <- test3$word == test3$result3b2_1 | test3$word == test3$result3b2_2 | test3$word == test3$result3b2_3
test2$success3b2 <- test2$word == test2$result3b2_1 | test2$word == test2$result3b2_2 | test2$word == test2$result3b2_3
test1$success3b2 <- test1$word == test1$result3b2_1 | test1$word == test1$result3b2_2 | test1$word == test1$result3b2_3

##MODEL 3b3 Predictions:

for (i in 1:length(test3$word)) {
  
  r31 <- as.character(paste(head(df3b3$word[df3b3$word_1 == test3$word_1[i] & df3b3$word_2 == test3$word_2[i] & df3b3$word_3 == test3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(df3b3$word[df3b3$word_1 == test3$word_1[i] & df3b3$word_2 == test3$word_2[i] & df3b3$word_3 == test3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(df3b3$word[df3b3$word_1 == test3$word_1[i] & df3b3$word_2 == test3$word_2[i] & df3b3$word_3 == test3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == test3$word_2[i] & df3b3$word_3 == test3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == test3$word_2[i] & df3b3$word_3 == test3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == test3$word_2[i] & df3b3$word_3 == test3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    test3$result3b3_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    test3$result3b3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test3$result3b3_1[i] <- r11
  } else {
    test3$result3b3_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    test3$result3b3_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != test3$result3b3_1[i]) {
    test3$result3b3_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    test3$result3b3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test3$result3b3_1[i]) {
    test3$result3b3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test3$result3b3_2[i] <- r12
  } else if (r01 != test3$result3b3_1[i]) {
    test3$result3b3_2[i] <- r01
  } else {
    test3$result3b3_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    test3$result3b3_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != test3$result3b3_1[i] & r21 != test3$result3b3_2[i]) {
    test3$result3b3_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != test3$result3b3_1[i] & r22 != test3$result3b3_2[i]) {
    test3$result3b3_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    test3$result3b3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test3$result3b3_1[i] & r11 != test3$result3b3_2[i]) {
    test3$result3b3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test3$result3b3_1[i] & r12 != test3$result3b3_2[i]) {
    test3$result3b3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test3$result3b3_3[i] <- r13
  } else if (r01 != test3$result3b3_1[i] & r01 != test3$result3b3_2[i]) {
    test3$result3b3_3[i] <- r01
  } else if (r02 != test3$result3b3_1[i] & r02 != test3$result3b3_2[i]) {
    test3$result3b3_3[i] <- r02
  } else {
    test3$result3b3_3[i] <- r03
  }
  
}

for (i in 1:length(test2$word)) {
  
  r21 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == test2$word_2[i] & df3b3$word_3 == test2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == test2$word_2[i] & df3b3$word_3 == test2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == test2$word_2[i] & df3b3$word_3 == test2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test2$result3b3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test2$result3b3_1[i] <- r11
  } else {
    test2$result3b3_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    test2$result3b3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test2$result3b3_1[i]) {
    test2$result3b3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test2$result3b3_2[i] <- r12
  } else if (r01 != test2$result3b3_1[i]) {
    test2$result3b3_2[i] <- r01
  } else {
    test2$result3b3_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    test2$result3b3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test2$result3b3_1[i] & r11 != test2$result3b3_2[i]) {
    test2$result3b3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test2$result3b3_1[i] & r12 != test2$result3b3_2[i]) {
    test2$result3b3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test2$result3b3_3[i] <- r13
  } else if (r01 != test2$result3b3_1[i] & r01 != test2$result3b3_2[i]) {
    test2$result3b3_3[i] <- r01
  } else if (r02 != test2$result3b3_1[i] & r02 != test2$result3b3_2[i]) {
    test2$result3b3_3[i] <- r02
  } else {
    test2$result3b3_3[i] <- r03
  }
  
}

for (i in 1:length(test1$word)) {
  
  r11 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == test1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b3$word[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>" & df3b3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    test1$result3b3_1[i] <- r11
  } else {
    test1$result3b3_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    test1$result3b3_2[i] <- r12
  } else if (r01 != test1$result3b3_1[i]) {
    test1$result3b3_2[i] <- r01
  } else {
    test1$result3b3_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    test1$result3b3_3[i] <- r13
  } else if (r01 != test1$result3b3_1[i] & r01 != test1$result3b3_2[i]) {
    test1$result3b3_3[i] <- r01
  } else if (r02 != test1$result3b3_1[i] & r02 != test1$result3b3_2[i]) {
    test1$result3b3_3[i] <- r02
  } else {
    test1$result3b3_3[i] <- r03
  }
  
}

test3$success3b3 <- test3$word == test3$result3b3_1 | test3$word == test3$result3b3_2 | test3$word == test3$result3b3_3
test2$success3b3 <- test2$word == test2$result3b3_1 | test2$word == test2$result3b3_2 | test2$word == test2$result3b3_3
test1$success3b3 <- test1$word == test1$result3b3_1 | test1$word == test1$result3b3_2 | test1$word == test1$result3b3_3

##MODEL 3b4 Predictions:

for (i in 1:length(test3$word)) {
  
  r31 <- as.character(paste(head(df3b4$word[df3b4$word_1 == test3$word_1[i] & df3b4$word_2 == test3$word_2[i] & df3b4$word_3 == test3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(df3b4$word[df3b4$word_1 == test3$word_1[i] & df3b4$word_2 == test3$word_2[i] & df3b4$word_3 == test3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(df3b4$word[df3b4$word_1 == test3$word_1[i] & df3b4$word_2 == test3$word_2[i] & df3b4$word_3 == test3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == test3$word_2[i] & df3b4$word_3 == test3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == test3$word_2[i] & df3b4$word_3 == test3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == test3$word_2[i] & df3b4$word_3 == test3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    test3$result3b4_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    test3$result3b4_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test3$result3b4_1[i] <- r11
  } else {
    test3$result3b4_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    test3$result3b4_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != test3$result3b4_1[i]) {
    test3$result3b4_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    test3$result3b4_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test3$result3b4_1[i]) {
    test3$result3b4_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test3$result3b4_2[i] <- r12
  } else if (r01 != test3$result3b4_1[i]) {
    test3$result3b4_2[i] <- r01
  } else {
    test3$result3b4_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    test3$result3b4_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != test3$result3b4_1[i] & r21 != test3$result3b4_2[i]) {
    test3$result3b4_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != test3$result3b4_1[i] & r22 != test3$result3b4_2[i]) {
    test3$result3b4_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    test3$result3b4_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test3$result3b4_1[i] & r11 != test3$result3b4_2[i]) {
    test3$result3b4_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test3$result3b4_1[i] & r12 != test3$result3b4_2[i]) {
    test3$result3b4_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test3$result3b4_3[i] <- r13
  } else if (r01 != test3$result3b4_1[i] & r01 != test3$result3b4_2[i]) {
    test3$result3b4_3[i] <- r01
  } else if (r02 != test3$result3b4_1[i] & r02 != test3$result3b4_2[i]) {
    test3$result3b4_3[i] <- r02
  } else {
    test3$result3b4_3[i] <- r03
  }
  
}

for (i in 1:length(test2$word)) {
  
  r21 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == test2$word_2[i] & df3b4$word_3 == test2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == test2$word_2[i] & df3b4$word_3 == test2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == test2$word_2[i] & df3b4$word_3 == test2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    test2$result3b4_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    test2$result3b4_1[i] <- r11
  } else {
    test2$result3b4_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    test2$result3b4_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != test2$result3b4_1[i]) {
    test2$result3b4_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    test2$result3b4_2[i] <- r12
  } else if (r01 != test2$result3b4_1[i]) {
    test2$result3b4_2[i] <- r01
  } else {
    test2$result3b4_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    test2$result3b4_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != test2$result3b4_1[i] & r11 != test2$result3b4_2[i]) {
    test2$result3b4_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != test2$result3b4_1[i] & r12 != test2$result3b4_2[i]) {
    test2$result3b4_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    test2$result3b4_3[i] <- r13
  } else if (r01 != test2$result3b4_1[i] & r01 != test2$result3b4_2[i]) {
    test2$result3b4_3[i] <- r01
  } else if (r02 != test2$result3b4_1[i] & r02 != test2$result3b4_2[i]) {
    test2$result3b4_3[i] <- r02
  } else {
    test2$result3b4_3[i] <- r03
  }
  
}

for (i in 1:length(test1$word)) {
  
  r11 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == test1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(df3b4$word[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>" & df3b4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    test1$result3b4_1[i] <- r11
  } else {
    test1$result3b4_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    test1$result3b4_2[i] <- r12
  } else if (r01 != test1$result3b4_1[i]) {
    test1$result3b4_2[i] <- r01
  } else {
    test1$result3b4_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    test1$result3b4_3[i] <- r13
  } else if (r01 != test1$result3b4_1[i] & r01 != test1$result3b4_2[i]) {
    test1$result3b4_3[i] <- r01
  } else if (r02 != test1$result3b4_1[i] & r02 != test1$result3b4_2[i]) {
    test1$result3b4_3[i] <- r02
  } else {
    test1$result3b4_3[i] <- r03
  }
  
}

test3$success3b4 <- test3$word == test3$result3b4_1 | test3$word == test3$result3b4_2 | test3$word == test3$result3b4_3
test2$success3b4 <- test2$word == test2$result3b4_1 | test2$word == test2$result3b4_2 | test2$word == test2$result3b4_3
test1$success3b4 <- test1$word == test1$result3b4_1 | test1$word == test1$result3b4_2 | test1$word == test1$result3b4_3

##MODEL c3b Predictions:

for (i in 1:length(cleanTest3$word)) {
  
  r31 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == cleanTest3$word_1[i] & dfc3b$word_2 == cleanTest3$word_2[i] & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == cleanTest3$word_1[i] & dfc3b$word_2 == cleanTest3$word_2[i] & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == cleanTest3$word_1[i] & dfc3b$word_2 == cleanTest3$word_2[i] & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == cleanTest3$word_2[i] & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == cleanTest3$word_2[i] & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == cleanTest3$word_2[i] & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    cleanTest3$result3b_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    cleanTest3$result3b_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest3$result3b_1[i] <- r11
  } else {
    cleanTest3$result3b_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    cleanTest3$result3b_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b_1[i]) {
    cleanTest3$result3b_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    cleanTest3$result3b_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b_1[i]) {
    cleanTest3$result3b_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest3$result3b_2[i] <- r12
  } else if (r01 != cleanTest3$result3b_1[i]) {
    cleanTest3$result3b_2[i] <- r01
  } else {
    cleanTest3$result3b_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    cleanTest3$result3b_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b_1[i] & r21 != cleanTest3$result3b_2[i]) {
    cleanTest3$result3b_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != cleanTest3$result3b_1[i] & r22 != cleanTest3$result3b_2[i]) {
    cleanTest3$result3b_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    cleanTest3$result3b_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b_1[i] & r11 != cleanTest3$result3b_2[i]) {
    cleanTest3$result3b_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest3$result3b_1[i] & r12 != cleanTest3$result3b_2[i]) {
    cleanTest3$result3b_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest3$result3b_3[i] <- r13
  } else if (r01 != cleanTest3$result3b_1[i] & r01 != cleanTest3$result3b_2[i]) {
    cleanTest3$result3b_3[i] <- r01
  } else if (r02 != cleanTest3$result3b_1[i] & r02 != cleanTest3$result3b_2[i]) {
    cleanTest3$result3b_3[i] <- r02
  } else {
    cleanTest3$result3b_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest2$word)) {
  
  r21 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == cleanTest2$word_2[i] & dfc3b$word_3 == cleanTest2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == cleanTest2$word_2[i] & dfc3b$word_3 == cleanTest2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == cleanTest2$word_2[i] & dfc3b$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    cleanTest2$result3b_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest2$result3b_1[i] <- r11
  } else {
    cleanTest2$result3b_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    cleanTest2$result3b_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b_1[i]) {
    cleanTest2$result3b_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest2$result3b_2[i] <- r12
  } else if (r01 != cleanTest2$result3b_1[i]) {
    cleanTest2$result3b_2[i] <- r01
  } else {
    cleanTest2$result3b_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    cleanTest2$result3b_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b_1[i] & r11 != cleanTest2$result3b_2[i]) {
    cleanTest2$result3b_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest2$result3b_1[i] & r12 != cleanTest2$result3b_2[i]) {
    cleanTest2$result3b_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest2$result3b_3[i] <- r13
  } else if (r01 != cleanTest2$result3b_1[i] & r01 != cleanTest2$result3b_2[i]) {
    cleanTest2$result3b_3[i] <- r01
  } else if (r02 != cleanTest2$result3b_1[i] & r02 != cleanTest2$result3b_2[i]) {
    cleanTest2$result3b_3[i] <- r02
  } else {
    cleanTest2$result3b_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest1$word)) {
  
  r11 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == cleanTest1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b$word[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>" & dfc3b$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    cleanTest1$result3b_1[i] <- r11
  } else {
    cleanTest1$result3b_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    cleanTest1$result3b_2[i] <- r12
  } else if (r01 != cleanTest1$result3b_1[i]) {
    cleanTest1$result3b_2[i] <- r01
  } else {
    cleanTest1$result3b_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    cleanTest1$result3b_3[i] <- r13
  } else if (r01 != cleanTest1$result3b_1[i] & r01 != cleanTest1$result3b_2[i]) {
    cleanTest1$result3b_3[i] <- r01
  } else if (r02 != cleanTest1$result3b_1[i] & r02 != cleanTest1$result3b_2[i]) {
    cleanTest1$result3b_3[i] <- r02
  } else {
    cleanTest1$result3b_3[i] <- r03
  }
  
}

cleanTest3$success3b <- cleanTest3$word == cleanTest3$result3b_1 | cleanTest3$word == cleanTest3$result3b_2 | cleanTest3$word == cleanTest3$result3b_3
cleanTest2$success3b <- cleanTest2$word == cleanTest2$result3b_1 | cleanTest2$word == cleanTest2$result3b_2 | cleanTest2$word == cleanTest2$result3b_3
cleanTest1$success3b <- cleanTest1$word == cleanTest1$result3b_1 | cleanTest1$word == cleanTest1$result3b_2 | cleanTest1$word == cleanTest1$result3b_3

##MODEL c3b2 Predictions:

for (i in 1:length(cleanTest3$word)) {
  
  r31 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == cleanTest3$word_1[i] & dfc3b2$word_2 == cleanTest3$word_2[i] & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == cleanTest3$word_1[i] & dfc3b2$word_2 == cleanTest3$word_2[i] & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == cleanTest3$word_1[i] & dfc3b2$word_2 == cleanTest3$word_2[i] & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == cleanTest3$word_2[i] & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == cleanTest3$word_2[i] & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == cleanTest3$word_2[i] & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    cleanTest3$result3b2_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    cleanTest3$result3b2_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest3$result3b2_1[i] <- r11
  } else {
    cleanTest3$result3b2_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    cleanTest3$result3b2_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b2_1[i]) {
    cleanTest3$result3b2_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    cleanTest3$result3b2_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b2_1[i]) {
    cleanTest3$result3b2_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest3$result3b2_2[i] <- r12
  } else if (r01 != cleanTest3$result3b2_1[i]) {
    cleanTest3$result3b2_2[i] <- r01
  } else {
    cleanTest3$result3b2_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    cleanTest3$result3b2_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b2_1[i] & r21 != cleanTest3$result3b2_2[i]) {
    cleanTest3$result3b2_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != cleanTest3$result3b2_1[i] & r22 != cleanTest3$result3b2_2[i]) {
    cleanTest3$result3b2_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    cleanTest3$result3b2_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b2_1[i] & r11 != cleanTest3$result3b2_2[i]) {
    cleanTest3$result3b2_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest3$result3b2_1[i] & r12 != cleanTest3$result3b2_2[i]) {
    cleanTest3$result3b2_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest3$result3b2_3[i] <- r13
  } else if (r01 != cleanTest3$result3b2_1[i] & r01 != cleanTest3$result3b2_2[i]) {
    cleanTest3$result3b2_3[i] <- r01
  } else if (r02 != cleanTest3$result3b2_1[i] & r02 != cleanTest3$result3b2_2[i]) {
    cleanTest3$result3b2_3[i] <- r02
  } else {
    cleanTest3$result3b2_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest2$word)) {
  
  r21 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == cleanTest2$word_2[i] & dfc3b2$word_3 == cleanTest2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == cleanTest2$word_2[i] & dfc3b2$word_3 == cleanTest2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == cleanTest2$word_2[i] & dfc3b2$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    cleanTest2$result3b2_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest2$result3b2_1[i] <- r11
  } else {
    cleanTest2$result3b2_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    cleanTest2$result3b2_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b2_1[i]) {
    cleanTest2$result3b2_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest2$result3b2_2[i] <- r12
  } else if (r01 != cleanTest2$result3b2_1[i]) {
    cleanTest2$result3b2_2[i] <- r01
  } else {
    cleanTest2$result3b2_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    cleanTest2$result3b2_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b2_1[i] & r11 != cleanTest2$result3b2_2[i]) {
    cleanTest2$result3b2_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest2$result3b2_1[i] & r12 != cleanTest2$result3b2_2[i]) {
    cleanTest2$result3b2_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest2$result3b2_3[i] <- r13
  } else if (r01 != cleanTest2$result3b2_1[i] & r01 != cleanTest2$result3b2_2[i]) {
    cleanTest2$result3b2_3[i] <- r01
  } else if (r02 != cleanTest2$result3b2_1[i] & r02 != cleanTest2$result3b2_2[i]) {
    cleanTest2$result3b2_3[i] <- r02
  } else {
    cleanTest2$result3b2_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest1$word)) {
  
  r11 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == cleanTest1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b2$word[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>" & dfc3b2$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    cleanTest1$result3b2_1[i] <- r11
  } else {
    cleanTest1$result3b2_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    cleanTest1$result3b2_2[i] <- r12
  } else if (r01 != cleanTest1$result3b2_1[i]) {
    cleanTest1$result3b2_2[i] <- r01
  } else {
    cleanTest1$result3b2_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    cleanTest1$result3b2_3[i] <- r13
  } else if (r01 != cleanTest1$result3b2_1[i] & r01 != cleanTest1$result3b2_2[i]) {
    cleanTest1$result3b2_3[i] <- r01
  } else if (r02 != cleanTest1$result3b2_1[i] & r02 != cleanTest1$result3b2_2[i]) {
    cleanTest1$result3b2_3[i] <- r02
  } else {
    cleanTest1$result3b2_3[i] <- r03
  }
  
}

cleanTest3$success3b2 <- cleanTest3$word == cleanTest3$result3b2_1 | cleanTest3$word == cleanTest3$result3b2_2 | cleanTest3$word == cleanTest3$result3b2_3
cleanTest2$success3b2 <- cleanTest2$word == cleanTest2$result3b2_1 | cleanTest2$word == cleanTest2$result3b2_2 | cleanTest2$word == cleanTest2$result3b2_3
cleanTest1$success3b2 <- cleanTest1$word == cleanTest1$result3b2_1 | cleanTest1$word == cleanTest1$result3b2_2 | cleanTest1$word == cleanTest1$result3b2_3

##MODEL c3b3 Predictions:

for (i in 1:length(cleanTest3$word)) {
  
  r31 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == cleanTest3$word_1[i] & dfc3b3$word_2 == cleanTest3$word_2[i] & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == cleanTest3$word_1[i] & dfc3b3$word_2 == cleanTest3$word_2[i] & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == cleanTest3$word_1[i] & dfc3b3$word_2 == cleanTest3$word_2[i] & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == cleanTest3$word_2[i] & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == cleanTest3$word_2[i] & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == cleanTest3$word_2[i] & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    cleanTest3$result3b3_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    cleanTest3$result3b3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest3$result3b3_1[i] <- r11
  } else {
    cleanTest3$result3b3_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    cleanTest3$result3b3_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b3_1[i]) {
    cleanTest3$result3b3_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    cleanTest3$result3b3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b3_1[i]) {
    cleanTest3$result3b3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest3$result3b3_2[i] <- r12
  } else if (r01 != cleanTest3$result3b3_1[i]) {
    cleanTest3$result3b3_2[i] <- r01
  } else {
    cleanTest3$result3b3_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    cleanTest3$result3b3_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b3_1[i] & r21 != cleanTest3$result3b3_2[i]) {
    cleanTest3$result3b3_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != cleanTest3$result3b3_1[i] & r22 != cleanTest3$result3b3_2[i]) {
    cleanTest3$result3b3_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    cleanTest3$result3b3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b3_1[i] & r11 != cleanTest3$result3b3_2[i]) {
    cleanTest3$result3b3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest3$result3b3_1[i] & r12 != cleanTest3$result3b3_2[i]) {
    cleanTest3$result3b3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest3$result3b3_3[i] <- r13
  } else if (r01 != cleanTest3$result3b3_1[i] & r01 != cleanTest3$result3b3_2[i]) {
    cleanTest3$result3b3_3[i] <- r01
  } else if (r02 != cleanTest3$result3b3_1[i] & r02 != cleanTest3$result3b3_2[i]) {
    cleanTest3$result3b3_3[i] <- r02
  } else {
    cleanTest3$result3b3_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest2$word)) {
  
  r21 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == cleanTest2$word_2[i] & dfc3b3$word_3 == cleanTest2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == cleanTest2$word_2[i] & dfc3b3$word_3 == cleanTest2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == cleanTest2$word_2[i] & dfc3b3$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    cleanTest2$result3b3_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest2$result3b3_1[i] <- r11
  } else {
    cleanTest2$result3b3_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    cleanTest2$result3b3_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b3_1[i]) {
    cleanTest2$result3b3_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest2$result3b3_2[i] <- r12
  } else if (r01 != cleanTest2$result3b3_1[i]) {
    cleanTest2$result3b3_2[i] <- r01
  } else {
    cleanTest2$result3b3_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    cleanTest2$result3b3_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b3_1[i] & r11 != cleanTest2$result3b3_2[i]) {
    cleanTest2$result3b3_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest2$result3b3_1[i] & r12 != cleanTest2$result3b3_2[i]) {
    cleanTest2$result3b3_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest2$result3b3_3[i] <- r13
  } else if (r01 != cleanTest2$result3b3_1[i] & r01 != cleanTest2$result3b3_2[i]) {
    cleanTest2$result3b3_3[i] <- r01
  } else if (r02 != cleanTest2$result3b3_1[i] & r02 != cleanTest2$result3b3_2[i]) {
    cleanTest2$result3b3_3[i] <- r02
  } else {
    cleanTest2$result3b3_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest1$word)) {
  
  r11 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == cleanTest1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b3$word[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>" & dfc3b3$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    cleanTest1$result3b3_1[i] <- r11
  } else {
    cleanTest1$result3b3_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    cleanTest1$result3b3_2[i] <- r12
  } else if (r01 != cleanTest1$result3b3_1[i]) {
    cleanTest1$result3b3_2[i] <- r01
  } else {
    cleanTest1$result3b3_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    cleanTest1$result3b3_3[i] <- r13
  } else if (r01 != cleanTest1$result3b3_1[i] & r01 != cleanTest1$result3b3_2[i]) {
    cleanTest1$result3b3_3[i] <- r01
  } else if (r02 != cleanTest1$result3b3_1[i] & r02 != cleanTest1$result3b3_2[i]) {
    cleanTest1$result3b3_3[i] <- r02
  } else {
    cleanTest1$result3b3_3[i] <- r03
  }
  
}

cleanTest3$success3b3 <- cleanTest3$word == cleanTest3$result3b3_1 | cleanTest3$word == cleanTest3$result3b3_2 | cleanTest3$word == cleanTest3$result3b3_3
cleanTest2$success3b3 <- cleanTest2$word == cleanTest2$result3b3_1 | cleanTest2$word == cleanTest2$result3b3_2 | cleanTest2$word == cleanTest2$result3b3_3
cleanTest1$success3b3 <- cleanTest1$word == cleanTest1$result3b3_1 | cleanTest1$word == cleanTest1$result3b3_2 | cleanTest1$word == cleanTest1$result3b3_3

##MODEL c3b4 Predictions:

for (i in 1:length(cleanTest3$word)) {
  
  r31 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == cleanTest3$word_1[i] & dfc3b4$word_2 == cleanTest3$word_2[i] & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[1]
  r32 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == cleanTest3$word_1[i] & dfc3b4$word_2 == cleanTest3$word_2[i] & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[2]
  r33 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == cleanTest3$word_1[i] & dfc3b4$word_2 == cleanTest3$word_2[i] & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r21 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == cleanTest3$word_2[i] & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == cleanTest3$word_2[i] & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == cleanTest3$word_2[i] & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest3$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r31) == FALSE) {
    cleanTest3$result3b4_1[i] <- r31
  } else if (is.na(r21) == FALSE) {
    cleanTest3$result3b4_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest3$result3b4_1[i] <- r11
  } else {
    cleanTest3$result3b4_1[i] <- r01
  }
  if (is.na(r32) == FALSE) {
    cleanTest3$result3b4_2[i] <- r32
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b4_1[i]) {
    cleanTest3$result3b4_2[i] <- r21 
  } else if (is.na(r22) == FALSE) {
    cleanTest3$result3b4_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b4_1[i]) {
    cleanTest3$result3b4_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest3$result3b4_2[i] <- r12
  } else if (r01 != cleanTest3$result3b4_1[i]) {
    cleanTest3$result3b4_2[i] <- r01
  } else {
    cleanTest3$result3b4_2[i] <- r02
  }
  if (is.na(r33) == FALSE) {
    cleanTest3$result3b4_3[i] <- r33
  } else if (is.na(r21) == FALSE & r21 != cleanTest3$result3b4_1[i] & r21 != cleanTest3$result3b4_2[i]) {
    cleanTest3$result3b4_3[i] <- r21 
  } else if (is.na(r22) == FALSE & r22 != cleanTest3$result3b4_1[i] & r22 != cleanTest3$result3b4_2[i]) {
    cleanTest3$result3b4_3[i] <- r22
  } else if (is.na(r23) == FALSE) {
    cleanTest3$result3b4_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest3$result3b4_1[i] & r11 != cleanTest3$result3b4_2[i]) {
    cleanTest3$result3b4_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest3$result3b4_1[i] & r12 != cleanTest3$result3b4_2[i]) {
    cleanTest3$result3b4_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest3$result3b4_3[i] <- r13
  } else if (r01 != cleanTest3$result3b4_1[i] & r01 != cleanTest3$result3b4_2[i]) {
    cleanTest3$result3b4_3[i] <- r01
  } else if (r02 != cleanTest3$result3b4_1[i] & r02 != cleanTest3$result3b4_2[i]) {
    cleanTest3$result3b4_3[i] <- r02
  } else {
    cleanTest3$result3b4_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest2$word)) {
  
  r21 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == cleanTest2$word_2[i] & dfc3b4$word_3 == cleanTest2$word_3[i]],3)))[1]
  r22 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == cleanTest2$word_2[i] & dfc3b4$word_3 == cleanTest2$word_3[i]],3)))[2]
  r23 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == cleanTest2$word_2[i] & dfc3b4$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r11 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest2$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest2$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest2$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r21) == FALSE) {
    cleanTest2$result3b4_1[i] <- r21
  } else if (is.na(r11) == FALSE) {
    cleanTest2$result3b4_1[i] <- r11
  } else {
    cleanTest2$result3b4_1[i] <- r01
  }
  if (is.na(r22) == FALSE) {
    cleanTest2$result3b4_2[i] <- r22
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b4_1[i]) {
    cleanTest2$result3b4_2[i] <- r11 
  } else if (is.na(r12) == FALSE) {
    cleanTest2$result3b4_2[i] <- r12
  } else if (r01 != cleanTest2$result3b4_1[i]) {
    cleanTest2$result3b4_2[i] <- r01
  } else {
    cleanTest2$result3b4_2[i] <- r02
  }
  if (is.na(r23) == FALSE) {
    cleanTest2$result3b4_3[i] <- r23
  } else if (is.na(r11) == FALSE & r11 != cleanTest2$result3b4_1[i] & r11 != cleanTest2$result3b4_2[i]) {
    cleanTest2$result3b4_3[i] <- r11 
  } else if (is.na(r12) == FALSE & r12 != cleanTest2$result3b4_1[i] & r12 != cleanTest2$result3b4_2[i]) {
    cleanTest2$result3b4_3[i] <- r12
  } else if (is.na(r13) == FALSE) {
    cleanTest2$result3b4_3[i] <- r13
  } else if (r01 != cleanTest2$result3b4_1[i] & r01 != cleanTest2$result3b4_2[i]) {
    cleanTest2$result3b4_3[i] <- r01
  } else if (r02 != cleanTest2$result3b4_1[i] & r02 != cleanTest2$result3b4_2[i]) {
    cleanTest2$result3b4_3[i] <- r02
  } else {
    cleanTest2$result3b4_3[i] <- r03
  }
  
}

for (i in 1:length(cleanTest1$word)) {
  
  r11 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest1$word_3[i]],3)))[1]
  r12 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest1$word_3[i]],3)))[2]
  r13 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == cleanTest1$word_3[i]],3)))[3]
  
  r01 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[1]
  r02 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[2]
  r03 <- as.character(paste(head(dfc3b4$word[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>" & dfc3b4$word_3 == "<unkn>"],3)))[3]
  
  if (is.na(r11) == FALSE) {
    cleanTest1$result3b4_1[i] <- r11
  } else {
    cleanTest1$result3b4_1[i] <- r01
  }
  if (is.na(r12) == FALSE) {
    cleanTest1$result3b4_2[i] <- r12
  } else if (r01 != cleanTest1$result3b4_1[i]) {
    cleanTest1$result3b4_2[i] <- r01
  } else {
    cleanTest1$result3b4_2[i] <- r02
  }
  if (is.na(r13) == FALSE) {
    cleanTest1$result3b4_3[i] <- r13
  } else if (r01 != cleanTest1$result3b4_1[i] & r01 != cleanTest1$result3b4_2[i]) {
    cleanTest1$result3b4_3[i] <- r01
  } else if (r02 != cleanTest1$result3b4_1[i] & r02 != cleanTest1$result3b4_2[i]) {
    cleanTest1$result3b4_3[i] <- r02
  } else {
    cleanTest1$result3b4_3[i] <- r03
  }
  
}

cleanTest3$success3b4 <- cleanTest3$word == cleanTest3$result3b4_1 | cleanTest3$word == cleanTest3$result3b4_2 | cleanTest3$word == cleanTest3$result3b4_3
cleanTest2$success3b4 <- cleanTest2$word == cleanTest2$result3b4_1 | cleanTest2$word == cleanTest2$result3b4_2 | cleanTest2$word == cleanTest2$result3b4_3
cleanTest1$success3b4 <- cleanTest1$word == cleanTest1$result3b4_1 | cleanTest1$word == cleanTest1$result3b4_2 | cleanTest1$word == cleanTest1$result3b4_3