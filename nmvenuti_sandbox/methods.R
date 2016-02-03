# install.packages(c("cluster", "tm", "ggplot2", "RWeka", "snowball"))
# install.packages("wordspace", repos="http://R-Forge.R-project.org")

library(tm)
library(wordspace)
library(RWeka)
library(SnowballC)
library(parallel)
library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
# library(testthat)
library(stringr)

# stpwds <- readLines("../../stopWords_Religious.txt")
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

preprocessing <- function(texts, stpwds=T, wordStem=F){
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

# content = input of a document as a string
# k = size of window, ignored if bySentence=True
# bySentence = measure co-occurrances by sentence
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

fasterCooccurrences <- cmpfun(function(content, k, bySentence=F){
  if (length(content) == 1)
    return(data.table())
  if (bySentence)
    load_string_sentence(content)
  else 
    load_string_k(sentence, k)
  cooccurrences <- pull_table()
  
}) 

text_venn_diagram <- function(set1, set2) {
  output <- list()
  output[[2]] <- intersect(set1, set2)
  output[[1]] <- setdiff(set1, set2)
  output[[3]] <- setdiff(set2, set1)
  output
  
}

mock_map_info <- function(word, context, freq) {
  paste0(word, occurrences_sepstr, context, " - ", freq)
}

Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
occurrences_sepstr <- "_!_%_?_"
sepstr_regex <- "_!_%_\\?_"
reload_cooccurrences <- function() {
  if (!is.na(str_match(getwd(), "tests")))
    sourceCpp("../cooccurences.cpp")
  else 
    sourceCpp("cooccurences.cpp")
}

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





