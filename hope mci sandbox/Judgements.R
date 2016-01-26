remove(list=ls())

library(tm)
library(topicmodels)
library(openNLP)
library(NLP)
library(stringr)

setwd('/Users/hopeemac/Documents/Education/Classes/UVA MSDS/15F/Capstone/westboro_sermons.pdf')

# Import all WBC Sermons
corpus <- VCorpus(DirSource('.'))
doc1 <- as.String(corpus[[1]])

##### Sentence segmentation -- assumes you have a document (our "text" variable)
#####
sentence.annotator = Maxent_Sent_Token_Annotator()
sentence.annotations = annotate(doc1, sentence.annotator)

#####
##### Tokenization -- buids on sentence segmentation
#####
token.annotator = Maxent_Word_Token_Annotator()
token.annotations = annotate(doc1, token.annotator, sentence.annotations)

# Prints all Tokens (sentences and words)
doc1[token.annotations]

##### Sentence segmentation and tokenization are basic steps required for many other analyses:  POS tagging, NEI, and syntactic parsing.

#####
##### Part-of-speech (POS) tagging -- builds on tokenization
#####
pos.annotator = Maxent_POS_Tag_Annotator()
pos.annotations = annotate(doc1, pos.annotator, token.annotations)

master <- as.data.frame(doc1[sentence.annotations])
View(master)

# Create Word only POS Subset
# POSw <- subset(pos.annotations, type == "word")
POSs <- as.data.frame(subset(pos.annotations, type == "sentence"))

# substr(doc1,1,127)
# View(n[which(n$start >= 1 & n$end < 127),])

# List of Row Numbers for Sentence Words, For first Sentence Only
c <- as.numeric(unlist(((POSs[1,"features"][[1]]))))
View(c)
# pos.annotations[c(148,149)]]

master$words <- NA

### KEY! WORKING 1/12
wordsAll <- list()
for (i in seq(nrow(POSs))) {
  x <- as.numeric(unlist(((POSs[i,"features"][[1]]))))
  y <- as.data.frame(pos.annotations[x])
  words <- c()
  for (j in seq(1,nrow(y))) {
    words <- append(words,substr(doc1,y[j,"start"],y[j,"end"]))
    # print(paste("added ",substr(doc1,y[i,"start"],y[i,"end"])))
  }
  wordsAll[[i]] <- list(words)
}
master$words <- wordsAll
View(master)

### KEY! v0.2 Adding in POS Tags
wordsAll <- list()
partsAll <- list()
for (i in seq(nrow(POSs))) {
  x <- as.numeric(unlist(((POSs[i,"features"][[1]]))))
  y <- as.data.frame(pos.annotations[x])
  words <- c()
  parts <- c()
  for (j in seq(1,nrow(y))) {
    words <- append(words,substr(doc1,y[j,"start"],y[j,"end"]))
    parts <- append(parts,unlist(y[j,"features"]))
    # print(paste("added ",substr(doc1,y[i,"start"],y[i,"end"])))
  }
  wordsAll[[i]] <- list(words)
  partsAll[[i]] <- list(parts)
}
master$words <- wordsAll
master$parts <- partsAll
View(master)


### KEY! v0.3 Checking for all 3 parts
toBe <- c("is","was","am","are","were","been","be","being")
wordsAll <- list()
partsAll <- list()
adjAll <- list()
toBeAll <- list()
nounAll <- list()

for (i in seq(nrow(POSs))) {
  x <- as.numeric(unlist(((POSs[i,"features"][[1]]))))
  y <- as.data.frame(pos.annotations[x])
  words <- c()
  parts <- c()
  adj_flag <- 0
  toBe_flag <- 0
  noun_flag <- 0
  
  for (j in seq(1,nrow(y))) {
    word <- substr(doc1,y[j,"start"],y[j,"end"])
    if (toBe_flag == 0) {
      if (word %in% toBe) {
        toBe_flag <- 1
      }
    }
    words <- append(words,word)
    
    part <- unlist(y[j,"features"])
    if (adj_flag == 0) {
      # print(adj_flag)
      if (part == 'JJ') {
        adj_flag <- 1
        # print(adj_flag)
      }
      # print(adj_flag)
    }
    # print(adj_flag)
    
    # Check for Noun
    if (noun_flag == 0) {
      # print(adj_flag)
      if (part == 'NN') {
        noun_flag <- 1
        # print(adj_flag)
      }
      # print(adj_flag)
    }
    # print(adj_flag)
    parts <- append(parts, part)
    # print(paste("added ",substr(doc1,y[i,"start"],y[i,"end"])))
  }
  wordsAll[[i]] <- list(words)
  partsAll[[i]] <- list(parts)
  adjAll[[i]] <- adj_flag
  toBeAll[[i]] <- toBe_flag
  nounAll[[i]] <- noun_flag
}
master$words <- wordsAll
master$parts <- partsAll
master$adj <- adjAll
master$toBe <- toBeAll
master$noun <- nounAll
View(master)


master$judgement <- ifelse(master$adj == 1 & master$toBe == 1 & master$noun == 1, 1, 0)
View(master)



# Gets all the info for the Words in the First Sentence
d <- as.data.frame(pos.annotations[c])
View(d)
# n$features[1]
# unlist
# Get Sentences
# doc1[POSs]



# Needs to be turned into an Apply Function
words <- c()
for (i in seq(1,nrow(d))) {
  words <- append(words,substr(doc1,d[i,"start"],d[i,"end"]))
  print(paste("added ",substr(doc1,d[i,"start"],d[i,"end"])))
}

View(d)
d$words <- words
n$words <- c()
View(n)
n[1,"words"] <- list(words)
# Needs to be turned into an Apply Function
parts <- c()
for (i in seq(1,nrow(d))) {
  parts <- append(parts,unlist(d[i,"features"]))
  #print(paste("added ",substr(doc1,d[i,"start"],d[i,"end"])))
}

d$parts <- parts
View(d)

# Find Sentences with a Form of To Be (Maybe add 'm and 've later)
toBe <- c("is","was","am","are","were","been","be","being")

for (word in d$words) {
  if (word %in% toBe) {
    print("Contains to Be!")
  }
  else {
    print("not")
  }
}

master$words %in% toBe

# To check if tobe in Words list for a specific row.
master$words[[20]][[1]] %in% toBe


# Check for JJ, JJR, JJS
for (pos in d$parts) {
  if (pos %in% c('JJ','JJR','JJS')) {
    print("Contains ADJ!")
  }
  else {
    print("not")
  }
}

# Check for NN, NNP, NNS, NNPS
for (pos in d$parts) {
  if (pos %in% c('NN','NNP', 'NNS', 'NNPS')) {
    print("Contains Noun")
  }
  else {
    print("not")
  }
}

nounCheck <- function(X) {
  if (X %in% c('NN','NNP', 'NNS', 'NNPS')) {
    print("Contains Noun")
  }
}
lapply(d$parts, nounCheck)

for (sentence in sent) {
  print(sentence)
}



#######################################



sent <- annotate(doc1, sent_token_annotator)
head(sent)

# Turn into Sentences into a Dataframe
sentences <- as.data.frame(doc1[sent], stringsAsFactors = F)
head(sentences, 10)
View(sentences)

# Get T/F for each sentence for each word
# Need to get into workable data structure
b <- sapply(toBe, grepl, apply(sentences, 1, print), ignore.case=TRUE)
View(b)


# Get Words per Sentence as Tokens into List
sentences[13,1]


toBeResults <- apply(sentences, 1, print)
apply(sentences, 1, print)
toBe <-data.frame(is = toBeResults)
View(toBe)
View(toBeResults)
s <- sentences[toBeResults,]
View(s)
paste0(" ","is"," ")

keywords <- c("dog", "cat", "bird")

strings <- c("Do you have a dog?", "My cat ate by bird.", "Let's get icecream!")
lapply(sentences, print)

b <- sapply(toBe, grepl, apply(sentences, 1, print), ignore.case=TRUE)
b$total <- sum(b[,1:7])
b.DF <- as.data.frame(b)
View(sentences)
View(b)
?sapply
# Extract which Form of To Be was found


# Identify if there is an Adjective in the Sentence

posResults <- lapply(s, annotate, f = pos_tag_annotator, a = sent)
doc1a <- annotate(doc1, list(word_token_annotator, sent_token_annotator))

sentPOS <- str_split(Corpus.tagged[[1]],"./.")
sentPOS
Corpus.tagged[[1]]

r <- data.frame(x = c(1,2,4))
View(r)
r$list <- list(3:4, 7:9, 7:10)
r$list[1]

# Only Works with Corpus
doc1[sentence.annotations]  # display original document, viewed by way of sentence annotations
View(doc1[sentence.annotations])
sent <- doc1[sentence.annotations]
View(sent)

adj_flag <- 0
part <- 'JJ'

if (adj_flag == 0) {
  print(adj_flag)
  if (part == 'JJ') {
    adj_flag <- 1
    print(adj_flag)
  }
  print(adj_flag)
}
print(adj_flag)



(part == 'JJ') {
  adj_flag <- 1
}
