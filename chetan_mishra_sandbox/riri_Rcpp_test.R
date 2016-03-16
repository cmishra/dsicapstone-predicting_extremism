library(Rcpp)
library(RcppArmadillo)
ignore_note <- function() {cat("RCPP functions loaded and data reset")}
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
occurrences_sepstr <- "_!_%_?_"
sepstr_regex <- "_!_%_\\?_"
reload_cooccurrences <- function() {
  sourceCpp("prototype/cooccurences.cpp")
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
reload_cooccurrences()


set_sepstring(occurrences_sepstr)
data.table(output_to_df(map_info()))
