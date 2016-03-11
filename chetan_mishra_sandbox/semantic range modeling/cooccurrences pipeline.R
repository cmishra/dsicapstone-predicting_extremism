library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
library(stringr)

output_cooccurrences <- function(filepath, window_length) {
  load(paste(filepath, 'RData/processedTokens.RData',sep="/"))
  processed_tokens <- name_of_input_obj
  set_sepstring(occurrences_sepstr)
  invisible(lapply(processed_tokens, function(element) {
    fasterCooccurrences(element$content, 4)
  }))
  wordCoocurrences <- data.table(output_to_df(map_info()))
  wordCoocurrences <- wordCoocurrences[,.(target, context, freq=as.integer(as.character(freq)))]
  reload_cooccurrences()
  
  save(wordCoocurrences, file=paste(filepath, "RData/wordCocurrences.RData", sep='/'))
}






