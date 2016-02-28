setwd("Dropbox/College/4_Fourth_Year/Capstone/chetan_mishra_sandbox/semantic range modeling/")
source("methods.R")
sourceCpp("cooccurences.cpp")
library(microbenchmark)

test_dir("tests")

data_wbc <- VCorpus(DirSource("../webscraping westboro/sermons/"))
processed_wbc <- preprocessing(data_wbc, stpwds, wordStem)

set_sepstring(occurrences_sepstr)
load_string_k(processed_wbc[[1]]$content, 2)
load_string_sentence(processed_wbc[[1]]$content)
str(map_info())

samp <- sample.int(12584, 20)

retest <- function() {
  reload_cooccurrences()
  print(paste0("subject: ", str_extract_cpp("subject", "subject")))
  print(paste0("sub: ", str_extract_cpp("subject", "sub")))
  print(paste0("subje: ", str_extract_cpp("subject", "sub.{2}")))
}


# why did i spend so long on this?

piper_data <- "../webscraping sermoncentral/john_piper/"
results <-
  microbenchmark(slow_dir_to_cooccurrences(piper_data, 2),
                 dir_to_cooccurrences(piper_data, 2),
                 slow_dir_to_cooccurrences(piper_data, 8),
                 dir_to_cooccurrences(piper_data, 8),
                 slow_dir_to_cooccurrences(piper_data),
                 dir_to_cooccurrences(piper_data),
                 times=3L)
print(results)
save(results, file="timing_results.RData")
load(file="timing_results.RData")

