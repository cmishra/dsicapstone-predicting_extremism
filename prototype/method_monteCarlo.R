
estimate_cos_dist <- function(vectors, num_runs=1000) {
  require(lsa)
  cos_sum <- 0
  indexes <- 1:length(vectors)
  for (i in 1:num_runs) {
    index_A <- sample.int(indexes, 1)
    index_B <- sample.int(indexes, 1)
    cos_sum <- cos_sum + cosine(vectors[[index_A]], 
                           vectors[[index_B]])
  }
  cos_sum/length(vectors)
}