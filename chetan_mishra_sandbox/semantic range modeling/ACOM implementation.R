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
param1 <- 0.025
param2 <- 0.05
param3 <- 0.025
word <- "love"

complete_net <- graph.data.frame(wordtable[sample.int(nrow(wordtable), 1000000)], directed=T)
subset_net <- delete_weak_links(complete_net, word, param1)
# E(subset_net)[from(word)]
candidate_contextonyms <- neighborhood(subset_net, 1, V(subset_net)[name == word], mode="out")[[1]]$name
larger_graph <- subset_net
for(vertex_index in candidate_contextonyms) {
  print(paste(E(subset_net)[from('love') & to('my')], vertex_index))
  larger_graph <- delete_weak_links(larger_graph, 
                                              V(larger_graph)[vertex_index]$name, param2)
}
nodes_to_keep <- neighborhood(larger_graph, 2, word, "out")[[1]]
larger_graph <- delete.vertices(larger_graph, difference(V(larger_graph), nodes_to_keep))
smaller_graph <- larger_graph
vertexes_to_delete <- c()
for(vertex_index in intersect(candidate_contextonyms, V(larger_graph))) {
  print(vertex_index)
  smaller_graph <- delete_weak_links(smaller_graph, V(smaller_graph)[vertex_index]$name, param3)
  if (get.edge.ids(smaller_graph, c(word, V(smaller_graph)[vertex_index]$name)) == 0) {
    vertexes_to_delete <- c(vertexes_to_delete, vertex_index)
  }
}
# which(candidate_contextonyms == 498)

contextonym_set <- neighborhood(smaller_graph, 1, word, "out")
# cliques <- cliques(larger_graph)
results <- microbenchmark(groupings <- cliques(larger_graph), times=1L)
save(groupings, results, file="igraph_cliques_timings.RData")
print(results)


