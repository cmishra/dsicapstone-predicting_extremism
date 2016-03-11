setwd("Dropbox/College/4_Fourth_Year/Capstone/chetan_mishra_sandbox/semantic range modeling/")
source("methods.R")
library(microbenchmark)

headwords <- c("god", "love", "infidel", "jewish", "jew", "hate", "evil", "ignore", "justice",
               "islamic", "muslim") # how to stem
data_wbc <- VCorpus(DirSource("../webscraping westboro/sermons/"))
data_jp <-VCorpus(DirSource("../webscraping sermoncentral/john_piper/"))
processed_jp <- preprocessing(data_jp, stpwds, wordStem, mc.cores=4)
set_sepstring(occurrences_sepstr)
invisible(lapply(processed_jp, function(element) {
  fasterCooccurrences(element$content, 4)
}))
dt <- data.table(output_to_df(map_info()))[,.(target, context, weight=as.integer(as.character(freq)))]
reload_cooccurrences()

# param1 is alpha and param2 is beta. look at ACOM by ji and ploux for an explanation of these
# parameters

wordtable <- dt
param1 <- 0.05
param2 <- 0.10
param3 <- 0.05
word <- "love"

tot_frequency_DSM <- function(wordtable, dsm, words, top_portion=0.5) {
  wordtable <- wordtable[target %in% words] [
    order(target, -freq)
  ][
    freq >= quantile(freq, 1-top_portion), .(context, freq),  by=target
  ]
  
  # sum across words that constitute overall 'context'
  targets_and_vectorsrbindlist(lapply(words, function(target) {
    data.frame(vector=colSums(dsm[wordtable[,context],]),
               target=target)
  }))
  
  # insert code to save here
}

complete_net <- graph.data.frame(wordtable, directed=T)
subset_net <- delete_weak_links(complete_net, word, param1)
# E(subset_net)[from(word)]
candidate_contextonyms <- neighborhood(subset_net, 1, V(subset_net)[name == word], mode="out")[[1]]$name
larger_graph <- subset_net
for(vertex_name in candidate_contextonyms) {
  larger_graph <- delete_weak_links(larger_graph, vertex_name, param2)
}
nodes_to_keep <- neighborhood(larger_graph, 1, word, "out")[[1]]
