setwd("Dropbox/College/4_Fourth_Year/Capstone/chetan_mishra_sandbox/semantic range modeling/")
source("methods.R")
sourceCpp("cooccurences.cpp")

test_dir("tests")

data_wbc <- VCorpus(DirSource("../webscraping westboro/sermons/"))
processed_wbc <- preprocessing(data_wbc, stpwds, wordStem)

set_sepstring(occurrences_sepstr)
load_string_k(processed_wbc[[1]]$content, 2)
load_string_sentence(processed_wbc[[1]]$content)
str(pull_table())

samp <- sample.int(12584, 20)

retest <- function() {
  reload_cooccurrences()
  set_sepstring(occurrences_sepstr)
  load_string_k(processed_wbc[[1]]$content, 2)
  str(pull_table()[samp,])
}


