
network_signal <- function(folderpath, group_id) {
  load(dsmProj_filename(folderpath, group_id))
  dsm.proj <- dsmProj
  cosines <- dist.matrix(dsm.proj, names(dsm.proj), method="cosine")
  adjacency <- cosine
  adjacency[cosine >= 30] <- 0
  adjacency[cosine < 30] <- 1
  net <- graph.adjacency(adjacency,mode='undirected')
  edgelist <- get.edgelist(net)
  ev.centrality <- evcent(net)$vector
  centrality <- subgraph.centrality(net)
  data.frame(subgraph_centrality=log(mean(centrality)),
             eigenvector_centrality=mean(ev.centrality),
             group=group_id)
}
