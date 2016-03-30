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