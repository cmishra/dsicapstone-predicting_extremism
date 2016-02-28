library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
library(stringr)

load("path/to/processed/tokens")
processed_tokens <- name_of_input_obj
set_sepstring(occurrences_sepstr)
invisible(lapply(processed_tokens, function(element) {
  fasterCooccurrences(element$content, 4)
}))
cooccurrencesdata.table(output_to_df(map_info()))
cooccurrences <- cooccurrences[,.(target, context, weight=as.integer(as.character(freq)))]
reload_cooccurrences()

save(cooccurrences, file="path/to/saved/file/" + "cooccurrences.RData")





