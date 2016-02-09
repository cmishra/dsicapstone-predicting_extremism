library(tm)
library(caTools)
library(rpart)
#install.packages("speedglm")
setwd("C:/Users/brian/Documents/UVA/Capstone")

# WBC
wbccorpus  <-Corpus(DirSource("Westboro"), readerControl = list(blank.lines.skip=TRUE))
pipercorpus  <-Corpus(DirSource("Piper/Sermons"), readerControl = list(blank.lines.skip=TRUE))
dorothycorpus  <-Corpus(DirSource("DorothyDay"), readerControl = list(blank.lines.skip=TRUE))
andersoncorpus  <-Corpus(DirSource("PastorAnderson"), readerControl = list(blank.lines.skip=TRUE))
#shepherdcorpus  <-Corpus(DirSource("Shepherd"), readerControl = list(blank.lines.skip=TRUE))

corpus <- c(wbccorpus,pipercorpus, dorothycorpus, andersoncorpus)#, shepherdcorpus)

#some preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removePunctuation)
#corpus = tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stemDocument, language="english")

#creating term matrix with TF-IDF weighting
dtm <-DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
dtm <- removeSparseTerms(dtm,.99)

labeledTerms = as.data.frame(as.matrix(dtm))
elasticity = c(rep(1,length(wbccorpus)),rep(4,length(pipercorpus)),rep(4,length(dorothycorpus)),
               rep(2,length(andersoncorpus)))#,rep(4,length(shepherdcorpus)))
labeledTerms$elasticity = elasticity

#Splitting into train and test data using caTools

set.seed(1738)

spl = sample.split(labeledTerms$elasticity, 0.6)

train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

#Build Model
model <- rpart(elasticity ~ ., data = train)
model$variable.importance

preds <- predict(model, newdata = test)
accuracy <- sum(round(preds, 0) == test$elasticity) / length(preds)





shepherdcorp  <-Corpus(DirSource("Shepherd"), readerControl = list(blank.lines.skip=TRUE))
shepherdcorpus <- c(wbccorpus,pipercorpus, dorothycorpus, andersoncorpus, shepherdcorp)

#some preprocessing
shepherdcorpus <- tm_map(shepherdcorpus, content_transformer(tolower))
#shepherdcorpus <- tm_map(shepherdcorpus, removeWords, stopwords("english"))
shepherdcorpus <- tm_map(shepherdcorpus, stripWhitespace)
shepherdcorpus = tm_map(shepherdcorpus, removePunctuation)
#shepherdcorpus = tm_map(shepherdcorpus, removeNumbers)
shepherdcorpus <- tm_map(shepherdcorpus, stemDocument, language="english")

#creating term matrix with TF-IDF weighting
shepherddtm <-DocumentTermMatrix(shepherdcorpus, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
shepherddtm <- removeSparseTerms(shepherddtm,.99)

shepherdlabeledTerms = as.data.frame(as.matrix(shepherddtm))
shepherdlabeledTerms <- shepherdlabeledTerms[-(1:(nrow(shepherdlabeledTerms)-length(shepherdcorp))),]

diffnames <- colnames(labeledTerms)[!colnames(labeledTerms) %in% colnames(shepherdlabeledTerms)]
diffnames <- diffnames[diffnames != "elasticity"]
newcols <- as.data.frame(matrix(0, nrow = nrow(shepherdlabeledTerms), ncol = length(diffnames)))
colnames(newcols) <- diffnames

shepherdlabeledTerms <- cbind(shepherdlabeledTerms,newcols)

shepherdpreds <- predict(model, newdata = shepherdlabeledTerms)

shepherdaccuracy <- sum(round(shepherdpreds, 0) == 4) / length(shepherdpreds)
