

tot_frequency_DSM <- function(wordtable, filepath, top_portion=0.5) {
  wordtable <- wordtable[
    target %in% words
    ][
    order(target, -freq)
    ][
      freq >= quantile(freq, 1-top_portion), .(context, freq),  by=target
    ]
  
  # sum across words that constitute overall 'context'
  targets_and_vectors <- rbindlist(lapply(words, function(target) {
    data.frame(vector=estimate_cos_simil(dsm[wordtable[,context],]),
               target=target)
  }))
  
  data.frame(group=group_id,
             )
}