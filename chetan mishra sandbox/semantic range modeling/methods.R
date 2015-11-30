# install.packages(c("cluster", "tm", "ggplot2", "RWeka", "snowball"))
# install.packages("wordspace", repos="http://R-Forge.R-project.org")

library(tm)
library(wordspace)
library(RWeka)
library(SnowballC)
library(parallel)
library(data.table)
library(compiler)

stpwds <- readLines("../../stopWords_Religious.txt")

removePunctuationExceptPeriod <- cmpfun(function(x) {
  gsub("[^[:alnum:][:space:].?!]", "", x)
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

preprocessing <- function(texts, stpwds=F, wordStem=F){
  texts <- tm_map(texts, content_transformer(individualizeEndOfSent))
  texts <- tm_map(texts, content_transformer(simpleTokenizer))
  texts <- tm_map(texts, content_transformer(removePunctuationExceptPeriod))
  texts <- tm_map(texts, content_transformer(tolower))
  if (!("logical" %in% class(stpwds)))
    texts <- tm_map(texts, content_transformer(removeWords), stpwds)
  if (!("logical" %in% class(wordStem)))
    texts <- tm_map(texts, content_transformer(wordStem))
  texts <- tm_map(texts, content_transformer(removeEmptyStrings))
  texts
}

# k = size of window, ignored if bySentence=True
# bySentence = measure co-occurrances by sentence
# contextFilter = which words can be used as predictors, by default all symbols
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

# tdmify <- function(corp, n){
#   tokenizr <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
#   
#   ctrl <- list(tokenize=tokenizr,
#                tolower=T,
#                removePunctation=T,
#                removeNumbers=T,
#                stopwords=stpwds,
#                stemming=wordStem,
#                wordLengths=c(1,Inf)
#   )
#   TermDocumentMatrix(corp, ctrl)
# }





