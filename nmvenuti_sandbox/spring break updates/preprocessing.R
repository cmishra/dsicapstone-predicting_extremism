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
# library(RcppArmadillo)
library(stringr)


#Functions from methods lib developed by Chetan Mishra
stpwds <-stopwords()

removePunctuationExceptPeriod <- cmpfun(function(x) {
  gsub("[^[:alpha:][:space:].?!]", "", x)
})

individualizeEndOfSent <- cmpfun(function(x) {
  str_replace_all(x,"[.?!]+", " . ")
})

simpleTokenizer <- cmpfun(function(x) {
  unlist(strsplit(x, split="[[:space:]]+"))
})

removeEmptyStrings <- cmpfun(function(x) {
  x<-gsub("[\r\n]", "", x)
  x[!x == ""]
  x[!nchar(x)==0]
})


# wordCoOccurences <- cmpfun(function(content, k, bySentence = F) {
#   if (length(content) == 1)
#     return(data.table())
#   endOfSentences <- which(content == ".")
#   ret <- rbindlist(lapply(1:length(content), function(i) {
#     if (bySentence) {
#       beforePeriodIndexes <- endOfSentences < i
#       startingIndex <- max(c(endOfSentences[beforePeriodIndexes], 0))
#       endingIndex <- min(c(endOfSentences[!beforePeriodIndexes], length(content)))
#       contextWords <- content[setdiff(startingIndex:endingIndex, i)]
#     } else {
#       contextWords <- c(tail(content[setdiff(1:(i-1), endOfSentences)], k),
#                         head(content[
#                           setdiff((i+1):length(content), endOfSentences)], k))
#     }
#     data.frame(target=content[i], context=contextWords, freq=1)
#     # as.data.table(data.frame(target=content[i], context=contextWords, freq=1))
#   }))
#   # ret[,.(freq=sum(freq)), by=.(target, context)][!(target == "." | context == ".")]
# })

preprocessDocuments<-function(filepath){

  
  #Define corpuses for data
  dataCorpus <- VCorpus(DirSource(paste(filepath,"raw",sep="/")))
  
  #Preprocess combined documents into clean strings
  #Normalizes
  processedStrings<-tm_map(dataCorpus, content_transformer(individualizeEndOfSent))
  processedStrings<-tm_map(processedStrings, content_transformer(removePunctuationExceptPeriod))
  processedStrings<-tm_map(processedStrings, content_transformer(tolower))
  processedStrings<-tm_map(processedStrings, content_transformer(removeNumbers))
  processedTokens<-tm_map(processedStrings, content_transformer(simpleTokenizer))
  processedTokens<-tm_map(processedTokens, content_transformer(removeWords), stpwds)
  processedTokens<-tm_map(processedTokens, content_transformer(wordStem))
  processedTokens<-tm_map(processedTokens, content_transformer(removeEmptyStrings))
  
  #Save backup of processed strings
  save(processedStrings, file=paste0(filepath,'/Rdata/processedStrings.RData'))
  
  #Save backup of processed tokens
  save(processedTokens, file=paste0(filepath,'/Rdata/processedTokens.RData'))

}
# filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro/"
# dataCorpus <- VCorpus(DirSource(paste(filepath,"Data",sep="/")))
# dataCorpus[[1]]$content


createCocurrence<- function(dataCorpus,k=5,bySentence=F){
  #Perform word cocourrence analysis for doc
  # dataCoocurrences <- wordCoOccurences(dataCorpus$content, k, bySentence)
  # dataCoocurrences <- rbindlist(lapply(dataCorpus, function(doc)
  #   wordCoOccurences(doc$content, k, bySentence)))
  dataCoocurrences<-tm_map(dataCorpus, content_transformer(wordCoOccurences),k, bySentence)
  # dataCoocurrences <- dataCoocurrences[target != "." & context != ".", .(freq=sum(freq)), .(target, context)]
  
  #Save backup
  save(dataCoocurrences, file=paste0(filepath,'/Rdata/dataCoocurrences.RData')) 
}



