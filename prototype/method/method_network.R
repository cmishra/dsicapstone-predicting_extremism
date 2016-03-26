
network_signal <- function(dsm.proj, target_words, group_id) {
  cosines <- dist.matrix(dsm.proj, names(dsm.proj), method="cosine")
  adjacency <- cosines
  adjacency[cosines >= 30] <- 0
  adjacency[cosines < 30] <- 1
  net <- graph.adjacency(adjacency,mode='undirected')
  edgelist <- get.edgelist(net)
  ev.centrality <- evcent(net)$vector[target_words]
  centrality <- subgraph.centrality(net)[target_words]
  data.frame(subgraph_centrality=log(mean(centrality, na.rm=T)),
             eigenvector_centrality=mean(ev.centrality, na.rm=T),
             group=group_id)
}
