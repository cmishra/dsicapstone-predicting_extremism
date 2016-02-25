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
    texts <- tm_map(texts, content_transformer(individualizeEndOfSent))
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


preprocessDocuments<-function(filepath,numCores=1){

  
  #Define corpuses for data
  dataCorpus <- VCorpus(DirSource(filepath))
  
  #Preprocess combined documents into clean strings (done in parallel)
  #Normalizes
  splitIndex <- rep_len(1:numCores, length(dataCorpus))
  processedStrings <- parLapply(cl, split(dataCorpus, splitIndex),
                              function (x) preprocessing(x,normalSent=T,noNumbers=T,tokenize=F,noPunctuation=F,lowerCase=T,stpwds=F,wordStem=F,noEmptyStrings=F))
  processedStrings <- do.call(function(...) c(..., recursive=T), 
                           processedStrings)
  
  
  #Preprocess combined documents into clean tokens
  #Tokenizes, splits into sentences, removes stopwords(if true), stems(if true)
  splitIndex <- rep_len(1:numCores, length(processedStrings))
  processedTokens <- parLapply(cl, split(processedStrings, splitIndex),
                               function (x) preprocessing(x,normalSent=F,noNumbers=F,tokenize=T,noPunctuation=T,lowerCase=F,stpwds=T,wordStem=T,noEmptyStrings=T))
  processedTokens <- do.call(function(...) c(..., recursive=T), 
                                processedTokens)
  
  #Save backup of processed strings
  save(processedStrings, file=paste0(filepath,'/processedStrings.RData'))
  
  #Save backup of processed tokens
  save(processedTokens, file=paste0(filepath,'/processedTokens.RData'))
  
  
  #Set up clustering for word occurrences analysis
  clusterEvalQ(cl, {
    k <- 5
    bySentence <- F
  })
  
  
  #Perform word cocourrence extraction
  cocurrences <- rbindlist(parLapplyLB(cl, processedCorp, function(doc) 
    wordCoOccurences(doc$content, k, bySentence)))
  stopCluster(cl)
  
  wordcocurrences <- wordcoocurrences[target != "." & context != ".",
                               .(freq=sum(freq)), 
                               .(target, context)]
  
  #Save backup for wordCooccurences
  save(wordCocurrences, file=paste0(filepath,'/wordCocurrences.RData'))
}