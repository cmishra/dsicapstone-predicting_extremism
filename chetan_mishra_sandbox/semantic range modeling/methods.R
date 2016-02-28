# install.packages(c("cluster", "tm", "ggplot2", "RWeka", "SnowballC"))
# install.packages("wordspace", repos="http://R-Forge.R-project.org")
# install.packages(c("data.table", "testthat", "stringr", "igraph"))
# install.packages(c("Rcpp", "RcppArmadillo", "compiler", "microbenchmark"))

library(tm)
library(wordspace)
library(RWeka)
library(SnowballC)
library(parallel)
library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
library(testthat)
library(stringr)
library(igraph)


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

preprocessing <- function(texts, stpwds=F, wordStem=F, ...){
  texts <- tm_map(texts, content_transformer(individualizeEndOfSent), ...)
  texts <- tm_map(texts, content_transformer(removeNumbers), ...)
  texts <- tm_map(texts, content_transformer(simpleTokenizer), ...)
  texts <- tm_map(texts, content_transformer(removePunctuationExceptPeriod), ...)
  texts <- tm_map(texts, content_transformer(tolower), ...)
  if (!("logical" %in% class(stpwds)))
    texts <- tm_map(texts, content_transformer(removeWords), stpwds, ...)
  if (!("logical" %in% class(wordStem)))
    texts <- tm_map(texts, content_transformer(wordStem), ...)
  texts <- tm_map(texts, content_transformer(removeEmptyStrings), ...)
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

slow_dir_to_cooccurrences <- function(dir_path, k=F) {
  data <- VCorpus(DirSource(dir_path))
  processed <- preprocessing(data, stpwds, wordStem)
  dt <- rbindlist(lapply(processed, function(element) {
    wordCoOccurences(element$content, k, !k)
  }))
  dt[,.(freq=sum(freq)), by=.(target, context)]
}

fasterCooccurrences <- cmpfun(function(content, k=F){
  if (length(content) == 1)
    return(data.table())
  if (!is.numeric(k))
    load_string_sentence(content)
  else 
    load_string_k(content, k)
}) 

dir_to_cooccurrences <- function(dir_path, k=F) {
  data <- VCorpus(DirSource(dir_path))
  processed <- preprocessing(data, stpwds, wordStem)
  set_sepstring(occurrences_sepstr)
  lapply(processed, function(element) {
    fasterCooccurrences(element$content, k)
  })
  dt <- data.table(output_to_df(map_info()))
  reload_cooccurrences()
  dt
}

text_venn_diagram <- function(set1, set2) {
  output <- list()
  output[[2]] <- intersect(set1, set2)
  output[[1]] <- setdiff(set1, set2)
  output[[3]] <- setdiff(set2, set1)
  output
  
}

output_to_df <- function(output) {
  target <- str_extract(output, paste0(".+", sepstr_regex))
  target <- str_sub(target, end=str_length(target)-str_length(occurrences_sepstr))
  context <- str_extract(output, paste0(sepstr_regex, ".+", " - "))
  context <- str_sub(context, start=str_length(occurrences_sepstr)+1, 
                     end=str_length(context) - 3)
  freq <- str_sub(str_extract(output, " - \\d+"), start=4)
  data.frame(target, context, freq)
}

mock_map_info <- function(word, context, freq) {
  paste0(word, occurrences_sepstr, context, " - ", freq)
}

ignore_note <- function() {cat("RCPP functions loaded and data reset")}
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
occurrences_sepstr <- "_!_%_?_"
sepstr_regex <- "_!_%_\\?_"
reload_cooccurrences <- function() {
  if (!is.na(str_match(getwd(), "tests"))) {
    sourceCpp("../cooccurences.cpp")
    ignore_note()
  }
  else {
    sourceCpp("cooccurences.cpp")
    ignore_note()
  }
}

reload_cooccurrences()

delete_weak_links <- function(graph, word, prop_to_preserve) {
  delete_edges(graph,
               E(graph)[from(word)][weight < quantile(weight, 1-prop_to_preserve)])
}

