# Hope McIntyre
# Capstone Baseline Prototype for NLP

install.packages('tm')
install.packages('topicmodels')
library(tm)
library(topicmodels)

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
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create Corpus
#VCorpus(VectorSource(sermon1))

# Create Term Matrix
dtm <- DocumentTermMatrix(corpus)
# Find Frequency Terms
findFreqTerms(dtm, 500)

# Return Term Matrix for specific Terms
inspect(DocumentTermMatrix(corpus, list(dictionary = c("satan", "sin", "plot"))))

start <- proc.time()
wbc.topics <- LDA(dtm, 20)
proc.time() - start

WBC_Terms <- terms(wbc.topics, 20)
WBC_Topics <- topics(wbc.topics)

write.table(WBC_Terms, "WBC_Terms", sep=",")
write.table(WBC_Topics, "WBC_Topics", sep=",")
###