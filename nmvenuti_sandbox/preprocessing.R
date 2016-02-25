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

