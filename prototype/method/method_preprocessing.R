##############################
#####Preprocessing Script#####
##############################

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


preprocessDocuments<-function(corpus,filepath,datafile_name){
   
  processedStrings<-tm_map(corpus, content_transformer(individualizeEndOfSent))
  processedStrings<-tm_map(processedStrings, content_transformer(removePunctuationExceptPeriod))
  processedStrings<-tm_map(processedStrings, content_transformer(tolower))
  processedStrings<-tm_map(processedStrings, content_transformer(removeNumbers))
  processedTokens<-tm_map(processedStrings, content_transformer(simpleTokenizer))
  #processedTokens<-tm_map(processedTokens, content_transformer(removeWords), stpwds)
  processedTokens<-tm_map(processedTokens, content_transformer(wordStem))
  processedTokens<-tm_map(processedTokens, content_transformer(removeEmptyStrings))
  
  #Save backup of processed tokens
  save(processedTokens, file=paste0(filepath,'/RData/processedTokens_',datafile_name,'.RData'))
  
}
