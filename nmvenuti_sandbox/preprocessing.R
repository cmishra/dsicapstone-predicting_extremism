##############################
#####Preprocessing Script#####
##############################

#Define libraries
library(tm)
library(wordspace)
library(RWeka)
library(SnowballC)
library(parallel)
library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
library(stringr)


#Functions from methods lib developed by Chetan Mishra
stpwds <-stopwords()

removePunctuationExceptPeriod <- cmpfun(function(x) {
  gsub("[^[:alpha:][:space:].?!]", "", x)
})

individualizeEndOfSent <- cmpfun(function(x) {
  gsub("[.?!]+", " . ", x)
})

simpleTokenizer <- cmpfun(function(x) {
  unlist(strsplit(x, split="[[:space:]]+"))
})

removeEmptyStrings <- cmpfun(function(x) {
  x[!x == ""]
})

preprocessing<- function(texts,normalSent,noNumbers,tokenize,noPunctuation,lowerCase,stpwds,wordStem,noEmptyStrings){
  if (normalSent)
    texts <- tm_map(texts, individualizeEndOfSent)
  if (noNumbers)
    texts<- tm_map(texts,content_transformer(removeNumbers))
  if (tokenize)
    texts <- tm_map(texts, content_transformer(simpleTokenizer))
  if (noPunctuation)
    texts <- tm_map(texts, content_transformer(removePunctuationExceptPeriod))
  if (lowerCase)
    texts <- tm_map(texts, content_transformer(tolower))
  if (stpwds)
    texts <- tm_map(texts, content_transformer(removeWords), stpwds)
  if (wordStem)
    texts <- tm_map(texts, content_transformer(wordStem))
  if (noEmptyStrings)
    texts <- tm_map(texts, content_transformer(removeEmptyStrings))
  texts
}

wordCoOccurences <- cmpfun(function(content, k, bySentence = F) {
  if (length(content) == 1)
    return(data.table())
  endOfSentences <- which(content == ".")
  ret <- rbindlist(lapply(1:length(content), function(i) {
    if (bySentence) {
      beforePeriodIndexes <- endOfSentences < i
      startingIndex <- max(c(endOfSentences[beforePeriodIndexes], 0))
      endingIndex <- min(c(endOfSentences[!beforePeriodIndexes], length(content)))
      contextWords <- content[setdiff(startingIndex:endingIndex, i)]
    } else {
      contextWords <- c(tail(content[setdiff(1:(i-1), endOfSentences)], k),
                        head(content[
                          setdiff((i+1):length(content), endOfSentences)], k))
    }
    data.frame(target=content[i], context=contextWords, freq=1)
    # as.data.table(data.frame(target=content[i], context=contextWords, freq=1))
  }))
  # ret[,.(freq=sum(freq)), by=.(target, context)][!(target == "." | context == ".")]
})

preprocessDocuments<-function(filepath){

  
  #Define corpuses for data
  dataCorpus <- VCorpus(DirSource(paste(filepath,"Data",sep="/")))
  
  #Preprocess combined documents into clean strings
  #Normalizes
  processedStrings<-individualizeEndOfSent(dataCorpus)
  processedStrings<-tolower(processedStrings)
  processedTokens<-simpleTokenizer(processedStrings)
  processedTokens<-removeWords(processedTokens,stpwds)
  processedTokens<-wordStem(processedTokens)
  processedTokens<-removeEmptyStrings(processedTokens)
  #Save backup of processed strings
  save(processedStrings, file=paste0(filepath,'/processedStrings.RData'))
  
  #Save backup of processed tokens
  save(processedTokens, file=paste0(filepath,'/processedTokens.RData'))
  
  
  #Perform word cocourrence extraction
  cocurrences <- wordCoOccurences(processedTokens, 5, F)
  
  wordcocurrences <- cocurrences[target != "." & context != ".",
                               .(freq=sum(freq)), 
                               .(target, context)]
  
  #Save backup for wordCooccurences
  save(wordCocurrences, file=paste(filepath,'Rdata/wordCocurrences.RData',sep="/"))
}