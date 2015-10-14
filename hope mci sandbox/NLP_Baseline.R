# Hope McIntyre
# Capstone Baseline Prototype for NLP

install.packages('tm')
install.packages('topicmodels')
library(tm)
library(topicmodels)
library(ggplot2)

setwd('/Users/hopeemac/Documents/Education/Classes/UVA MSDS/15F/Capstone/westboro_sermons.pdf')
#sermon <- read.csv('WestboroBaptist_Sermon_20070527.pdf.txt')
#sermon <- 'WestboroBaptist_Sermon_20070527.pdf.txt'
#sermon

#sermons  <- 
# Read in Text File
#sermon1 <- readChar(sermon, file.info(sermon)$size)

corpus <- VCorpus(DirSource('.'))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace) 
# corpus <- tm_map(corpus, tolower) 
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
sort(stopwords("english"))
# tm_map(corpus, stemDocument, language = "english")
?removeWords
# Create Corpus
#VCorpus(VectorSource(sermon1))

# Create Term Matrix
?DocumentTermMatrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = stopwords("english")))
# Find Frequency Terms
write.table(findFreqTerms(dtm, 1000), "wordFreq.csv",sep = ",")

# Return Term Matrix for specific Terms
write.table(inspect(DocumentTermMatrix(corpus, list(dictionary = c("satan", "sin", "plot")))), 
            "inspectTerm.csv", sep = ",")

start <- proc.time()
wbc.topics <- LDA(dtm, 20)
proc.time() - start

WBC_Terms <- terms(wbc.topics, 20)
WBC_Topics <- topics(wbc.topics)

write.table(WBC_Terms, "WBC_Terms.csv", sep=",")
write.table(WBC_Topics, "WBC_Topics.csv", sep=",")

write.table(inspect(DocumentTermMatrix(corpus, list(dictionary = c("satan", "sin", "plot")))), 
            "inspectTerm.csv", sep = ",")

###
# Create n-gram
install.packages('RWeka')
library("RWeka")
onegram <- NGramTokenizer(corpus, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!"))
df_one <- data.frame(table(onegram))
# sorting the words based on frequency  
one_decr <- df_one[order(df_one$Freq,decreasing = TRUE),]
one_top_25 <- one_decr[1:25,]
colnames(one_top_25) <- c("Word","Frequency")
rm(onegram, df_one)

ggplot(one_top_25, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Blue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
