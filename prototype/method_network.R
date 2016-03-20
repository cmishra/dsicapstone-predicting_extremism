
network_signal <- function(dsm.proj, group_id) {
  cosines <- dist.matrix(dsm.proj, names(dsm.proj), method="cosine")
  adjacency <- cosines
  adjacency[cosines >= 30] <- 0
  adjacency[cosines < 30] <- 1
  net <- graph.adjacency(adjacency,mode='undirected')
  edgelist <- get.edgelist(net)
  ev.centrality <- evcent(net)$vector
  centrality <- subgraph.centrality(net)
  data.frame(subgraph_centrality=log(mean(centrality)),
             eigenvector_centrality=mean(ev.centrality),
             group=group_id)
}
