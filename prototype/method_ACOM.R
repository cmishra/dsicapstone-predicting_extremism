

tot_frequency_DSM <- function(wordCo, dsm, group_id, top_portion=0.5) {
  wordCo <- wordCo[
    target %in% words
    ][
    order(target, -freq)
    ][
      freq >= quantile(freq, 1-top_portion), .(context, freq),  by=target
    ]
  
  # sum across words that constitute overall 'context'
  mean_similarity <- mean(unlist(lapply(words, function(target) {
    vectors <- dsmProj[wordCo[,context],]
    vectors <- split(vectors, seq(nrow(vectors)))
    estimate_cos_simil(vectors)
  })))
  
  data.frame(group=group_id,
            acom=mean_similarity)
}