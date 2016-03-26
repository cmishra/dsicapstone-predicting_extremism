
estimate_cos_simil <- function(vectors, num_runs=1000) {
  if (num_runs > length(vectors)*2)
    num_runs <- length(vectors)*2
  require(lsa)
  cos_sum <- 0
  indexes <- 1:length(vectors)
  for (i in 1:num_runs) {
    index_A <- sample.int(length(vectors), 1)
    index_B <- sample.int(length(vectors), 1)
    cos_sum <- cos_sum + cosine(vectors[[index_A]], 
                           vectors[[index_B]])
  }
  cos_sum/length(vectors)
}

wordCo_filename <- function(folderpath, group_id) {
  paste0(folderpath, "/wordCo_", group_id, ".RData")
}

dsmProj_filename <- function(folderpath, group_id) {
  paste0(folderpath, "/dsmProj_", group_id, ".RData")
}

ignore_note <- function() {cat("RCPP functions loaded and data reset")}
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
occurrences_sepstr <- "_!_%_?_"
sepstr_regex <- "_!_%_\\?_"
reload_cooccurrences <- function() {
  sourceCpp("prototype/method/cooccurences.cpp")
}
reload_cooccurrences()