## Generated training dataset (Models 1-3, 3b-3b4)

df <- rbind(data.frame(word_1 = smp4WordTotals$word_1, word_2 = smp4WordTotals$word_2, word_3 = smp4WordTotals$word_3, word = smp4WordTotals$word, termFreq = smp4WordTotals$freq),
            data.frame(word_1 = "<unkn>", word_2 = smp3WordTotals$word_1, word_3 = smp3WordTotals$word_2, word = smp3WordTotals$word, termFreq = smp3WordTotals$freq),
            data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = smp2WordTotals$term, word = smp2WordTotals$word, termFreq = smp2WordTotals$freq),
            data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = smpAllSources$word, termFreq = smpAllSources$wordFreq))

##Generated alternative training dataset (Model 4)

smpXoXWordTotals <- aggregate(smp4WordTotals$freq, by=list(word_1 = smp4WordTotals$word_1,word_3 = smp4WordTotals$word_3,word = smp4WordTotals$word), FUN=sum)
smpXXoWordTotals <- aggregate(smp4WordTotals$freq, by=list(word_1 = smp4WordTotals$word_1,word_2 = smp4WordTotals$word_2,word = smp4WordTotals$word), FUN=sum)
smpoXoWordTotals <- aggregate(smp3WordTotals$freq, by=list(word_1 = smp3WordTotals$word_1,word = smp3WordTotals$word), FUN=sum)
smpXooWordTotals <- aggregate(smp4WordTotals$freq, by=list(word_1 = smp4WordTotals$word_1,word = smp4WordTotals$word), FUN=sum)

dfb <- rbind(df,
             data.frame(word_1 = smpXoXWordTotals$word_1, word_2 = "<unkn>", word_3 = smpXoXWordTotals$word_3, word = smpXoXWordTotals$word, termFreq = smpXoXWordTotals$x),
             data.frame(word_1 = smpXXoWordTotals$word_1, word_2 = smpXXoWordTotals$word_2, word_3 = "<unkn>", word = smpXXoWordTotals$word, termFreq = smpXXoWordTotals$x),
             data.frame(word_1 = "<unkn>", word_2 = smpoXoWordTotals$word_1, word_3 = "<unkn>", word = smpoXoWordTotals$word, termFreq = smpoXoWordTotals$x),
             data.frame(word_1 = smpXooWordTotals$word_1, word_2 = "<unkn>", word_3 = "<unkn>", word = smpXooWordTotals$word, termFreq = smpXooWordTotals$x))

## Generated 'clean' training dataset (Models c3, c3b-c3b4)

dfc <- rbind(data.frame(word_1 = smpClean4WordTotals$word_1, word_2 = smpClean4WordTotals$word_2, word_3 = smpClean4WordTotals$word_3, word = smpClean4WordTotals$word, termFreq = smpClean4WordTotals$freq),
             data.frame(word_1 = "<unkn>", word_2 = smpClean3WordTotals$word_1, word_3 = smpClean3WordTotals$word_2, word = smpClean3WordTotals$word, termFreq = smpClean3WordTotals$freq),
             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = smpClean2WordTotals$term, word = smpClean2WordTotals$word, termFreq = smpClean2WordTotals$freq),
             data.frame(word_1 = "<unkn>", word_2 = "<unkn>", word_3 = "<unkn>", word = smpAllSources$word, termFreq = smpAllSources$wordFreq))

##MODEL1: Predict 1 word, maximum ngram = 3, minimum term frequency 1

df1 <- df %>% group_by(word_1, word_2, word_3) %>% top_n(1, termFreq)
df1 <- merge(x = df1, y = smpAllSources, by.x = "word", by.y = "word")
df1 <- df1 %>% group_by(word_1, word_2, word_3) %>% top_n(1, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq)

##MODEL2: Predict 1 word, maximum ngram = 3, minimum term frequency 2

df2 <- df %>% subset(termFreq > 1) %>% group_by(word_1, word_2, word_3) %>% top_n(1, termFreq)
df2 <- merge(x = df2, y = smpAllSources, by.x = "word", by.y = "word")
df2 <- df2 %>% group_by(word_1, word_2, word_3) %>% top_n(1, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq)

##MODEL 3: Predict 3 words, maximum ngram = 3, minimum term frequency 1

df3 <- df %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
df3 <- merge(x = df3, y = smpAllSources, by.x = "word", by.y = "word")
df3 <- df3 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
df3 <- df3[with(df3, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL 4: Predict 3 words, maximum ngram = 3, minimum term frequency 1 using alternative dataset with missing words

df4 <- dfb %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
df4 <- merge(x = df4, y = smpAllSources, by.x = "word", by.y = "word")
df4 <- df4 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
df4 <- df4[with(df4, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]            

##MODEL c3: Predict 3 words, maximum ngram = 3, minimum term frequency 1 using clean dataset

dfc3 <- dfc %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
dfc3 <- merge(x = dfc3, y = smpAllSources, by.x = "word", by.y = "word")
dfc3 <- dfc3 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
dfc3 <- dfc3[with(dfc3, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL 3b: Predict 3 words, maximum ngram = 3, minimum term frequency 2

df3b <- df %>% subset(termFreq > 1) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
df3b <- merge(x = df3b, y = smpAllSources, by.x = "word", by.y = "word")
df3b <- df3b %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
df3b <- df3b[with(df3b, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL 3b2: Predict 3 words, maximum ngram = 3, minimum term frequency 3

df3b2 <- df %>% subset(termFreq > 2) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
df3b2 <- merge(x = df3b2, y = smpAllSources, by.x = "word", by.y = "word")
df3b2 <- df3b2 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
df3b2 <- df3b2[with(df3b2, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL 3b3: Predict 3 words, maximum ngram = 3, minimum term frequency 4

df3b3 <- df %>% subset(termFreq > 3) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
df3b3 <- merge(x = df3b3, y = smpAllSources, by.x = "word", by.y = "word")
df3b3 <- df3b3 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
df3b3 <- df3b3[with(df3b3, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL 3b4: Predict 3 words, maximum ngram = 3, minimum term frequency 5

df3b4 <- df %>% subset(termFreq > 4) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
df3b4 <- merge(x = df3b4, y = smpAllSources, by.x = "word", by.y = "word")
df3b4 <- df3b4 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
df3b4 <- df3b4[with(df3b4, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL c3b: Predict 3 words, maximum ngram = 3, minimum term frequency 2 using clean dataset

dfc3b <- dfc %>% subset(termFreq > 1) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
dfc3b <- merge(x = dfc3b, y = smpAllSources, by.x = "word", by.y = "word")
dfc3b <- dfc3b %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
dfc3b <- dfc3b[with(dfc3b, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL c3b2: Predict 3 words, maximum ngram = 3, minimum term frequency 3 using clean dataset

dfc3b2 <- dfc %>% subset(termFreq > 2) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
dfc3b2 <- merge(x = dfc3b2, y = smpAllSources, by.x = "word", by.y = "word")
dfc3b2 <- dfc3b2 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
dfc3b2 <- dfc3b2[with(dfc3b2, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL c3b3: Predict 3 words, maximum ngram = 3, minimum term frequency 4 using clean dataset

dfc3b3 <- dfc %>% subset(termFreq > 3) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
dfc3b3 <- merge(x = dfc3b3, y = smpAllSources, by.x = "word", by.y = "word")
dfc3b3 <- dfc3b3 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
dfc3b3 <- dfc3b3[with(dfc3b3, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

##MODEL c3b4: Predict 3 words, maximum ngram = 3, minimum term frequency 5 using clean dataset

dfc3b4 <- dfc %>% subset(termFreq > 4) %>% group_by(word_1, word_2, word_3) %>% top_n(3, termFreq)
dfc3b4 <- merge(x = dfc3b4, y = smpAllSources, by.x = "word", by.y = "word")
dfc3b4 <- dfc3b4 %>% group_by(word_1, word_2, word_3, termFreq) %>% top_n(3, wordFreq) %>% select(word, word_1, word_2, word_3, termFreq, wordFreq)
dfc3b4 <- dfc3b4[with(dfc3b4, order(as.numeric(termFreq),as.numeric(wordFreq), decreasing = TRUE)),]

