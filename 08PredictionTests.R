library(stringr)

##Success rates

srate <- data.frame(c("Null model", "Null model 3", "Model 1", "Model 2", "Model 3", "Model 4", "Model 3b", "Model 3b2", "Model 3b3", "Model 3b4", "Clean model 3", "Clean model 3b", "Clean model 3b2", "Clean model 3b3", "Clean model 3b4"),
                    rbind(cbind(mean(test3$nullsuccess), mean(test2$nullsuccess), mean(test1$nullsuccess)),
                          cbind(mean(test3$nullsuccess3),mean(test2$nullsuccess3),mean(test1$nullsuccess3)),
                          cbind(mean(test3$success), mean(test2$success), mean(test1$success)),
                          cbind(mean(test3$success2),mean(test2$success2),mean(test1$success2)),
                          cbind(mean(test3$success3),mean(test2$success3),mean(test1$success3)),
                          cbind(mean(test3$success4),NA,NA),
                          cbind(mean(test3$success3b),mean(test2$success3b),mean(test1$success3b)),
                          cbind(mean(test3$success3b2),mean(test2$success3b2),mean(test1$success3b2)),
                          cbind(mean(test3$success3b3),mean(test2$success3b3),mean(test1$success3b3)),
                          cbind(mean(test3$success3b4),mean(test2$success3b4),mean(test1$success3b4)),
                          cbind(mean(cleanTest3$success3),mean(cleanTest2$success3),mean(cleanTest1$success3)),
                          cbind(mean(cleanTest3$success3b),mean(cleanTest2$success3b),mean(cleanTest1$success3b)),
                          cbind(mean(cleanTest3$success3b2),mean(cleanTest2$success3b2),mean(cleanTest1$success3b2)),
                          cbind(mean(cleanTest3$success3b3),mean(cleanTest2$success3b3),mean(cleanTest1$success3b3)),
                          cbind(mean(cleanTest3$success3b4),mean(cleanTest2$success3b4),mean(cleanTest1$success3b4))))
names(srate) <- c("Model", "3-gram", "2-gram", "1-gram")
print(srate)

##Model size

ssize <- data.frame(c("Null model", "Null model 3", "Model 1", "Model 2", "Model 3", "Model 4", "Model 3b", "Model 3b2", "Model 3b3", "Model 3b4", "Clean model 3", "Clean model 3b", "Clean model 3b2", "Clean model 3b3", "Clean model 3b4"),
                    rbind(cbind(format(object.size(test3$null[1]), units = "b"), 
                                format(object.size(test2$null[1]), units = "b"), 
                                format(object.size(test1$null[1]), units = "b")),
                          cbind(format(object.size(as.character(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)[1:3])), units = "b"), 
                                format(object.size(as.character(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)[1:3])), units = "b"), 
                                format(object.size(as.character(head(df3$word[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>" & df3$word_3 == "<unkn>"],3)[1:3])), units = "b")),
                          cbind(format(object.size(df1), units = "Mb"), 
                                format(object.size(df1[df1$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df1[df1$word_1 == "<unkn>" & df1$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(df2), units = "Mb"), 
                                format(object.size(df2[df2$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df2[df2$word_1 == "<unkn>" & df2$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(df3), units = "Mb"), 
                                format(object.size(df3[df3$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(df4), units = "Mb"), 
                                NA, 
                                NA),
                          cbind(format(object.size(df3b[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b[df3b$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(df3b2[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b2[df3b2$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b2[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(df3b3[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b3[df3b3$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b3[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(df3b4[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b4[df3b4$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(df3b4[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(dfc3[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3[dfc3$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>",c(1,4)]), units = "Mb")), 
                          cbind(format(object.size(dfc3b[,c(1,3,4)]), units = "Mb"),
                                format(object.size(dfc3b[dfc3b$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(dfc3b2[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b2[dfc3b2$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b2[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(dfc3b3[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b3[dfc3b3$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b3[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>",c(1,4)]), units = "Mb")),
                          cbind(format(object.size(dfc3b4[,c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b4[dfc3b4$word_1 == "<unkn>",c(1,3,4)]), units = "Mb"), 
                                format(object.size(dfc3b4[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>",c(1,4)]), units = "Mb"))
                    ))

names(ssize) <- c("Model", "3-gram", "2-gram", "1-gram")
print(ssize)

##Run time

inputText <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"

df33g <- df3
df32g <- df3[df3$word_1 == "<unkn>", c(1,3,4)]
df31g <- df3[df3$word_1 == "<unkn>" & df3$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
m3 <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- df3b
df32g <- df3b[df3b$word_1 == "<unkn>", c(1,3,4)]
df31g <- df3b[df3b$word_1 == "<unkn>" & df3b$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
m3b <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- df3b2
df32g <- df3b2[df3b2$word_1 == "<unkn>", c(1,3,4)]
df31g <- df3b2[df3b2$word_1 == "<unkn>" & df3b2$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
m3b2<- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- df3b3
df32g <- df3b3[df3b3$word_1 == "<unkn>", c(1,3,4)]
df31g <- df3b3[df3b3$word_1 == "<unkn>" & df3b3$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
m3b3 <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- df3b4
df32g <- df3b4[df3b4$word_1 == "<unkn>", c(1,3,4)]
df31g <- df3b4[df3b4$word_1 == "<unkn>" & df3b4$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
m3b4 <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- dfc3
df32g <- dfc3[dfc3$word_1 == "<unkn>", c(1,3,4)]
df31g <- dfc3[dfc3$word_1 == "<unkn>" & dfc3$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
mc3 <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- dfc3b
df32g <- dfc3b[dfc3b$word_1 == "<unkn>", c(1,3,4)]
df31g <- dfc3b[dfc3b$word_1 == "<unkn>" & dfc3b$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
mc3b <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- dfc3b2
df32g <- dfc3b2[dfc3b2$word_1 == "<unkn>", c(1,3,4)]
df31g <- dfc3b2[dfc3b2$word_1 == "<unkn>" & dfc3b2$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
mc3b2<- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- dfc3b3
df32g <- dfc3b3[dfc3b3$word_1 == "<unkn>", c(1,3,4)]
df31g <- dfc3b3[dfc3b3$word_1 == "<unkn>" & dfc3b3$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
mc3b3 <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

df33g <- dfc3b4
df32g <- dfc3b4[dfc3b4$word_1 == "<unkn>", c(1,3,4)]
df31g <- dfc3b4[dfc3b4$word_1 == "<unkn>" & dfc3b4$word_2 == "<unkn>", c(1,4)]
names(df32g) <- c("word","word_1","word_2")
names(df31g) <- c("word","word_1")
mc3b4 <- c(system.time(mod3_3(inputText = inputText))[3],system.time(mod3_2(inputText = inputText))[3],system.time(mod3_1(inputText = inputText))[3])

stime <- data.frame(cbind( c("Model 3", "Model 3b", "Model 3b2", "Model 3b3", "Model 3b4", "Clean model 3", "Clean model 3b", "Clean model 3b2", "Clean model 3b3", "Clean model 3b4"),
                           round(rbind(m3,m3b,m3b2,m3b3,m3b4,mc3,mc3b,mc3b2,mc3b3,mc3b4),2)
))
names(stime) <- c("Model", "3-gram", "2-gram", "1-gram")
rownames(stime) <- c()
print(stime)

#Different size training data

t1 <- sizeTest(0.01)
t2 <- sizeTest(0.04)
t3 <- sizeTest(0.1)

sizetest <- data.frame(rbind(t1,t2,t3))
rownames(sizetest) <- c()
print(sizetest)