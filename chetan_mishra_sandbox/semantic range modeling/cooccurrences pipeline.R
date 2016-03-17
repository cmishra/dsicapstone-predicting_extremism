library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
library(stringr)

output_cooccurrences <- function(processed_tokens, folderpath, group_id, window_length) {
  set_sepstring(occurrences_sepstr)
  invisible(lapply(processed_tokens, function(element) {
    fasterCooccurrences(element$content, 4)
  }))
  wordCo <- data.table(output_to_df(map_info()))
  wordCo <- wordCo[,.(target, context, freq=as.integer(as.character(freq)))]
  reload_cooccurrences()
  
  save(wordCo, file=wordCo_filename(folderpath, group_id))
}






