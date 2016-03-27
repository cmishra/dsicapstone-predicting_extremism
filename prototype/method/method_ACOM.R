

tot_frequency_DSM <- function(wordCo, dsmProj, words, group_id, top_portion=0.5) {
  wordCo <- wordCo[
    target %in% words & context %in% rownames(dsmProj)
    ][
    order(target, -freq)
    ][
      freq >= quantile(freq, 1-top_portion), .(context, freq),  by=target
    ]
  
  # sum across words that constitute overall 'context'
  mean_similarity <- mean(unlist(lapply(words, function(word) {
    vectors <- wordCo[target == word,as.character(context)]
    vectors <- dsmProj[vectors,]
    vectors <- lapply(split(vectors, seq(nrow(vectors))), unlist)
    estimate_cos_simil(vectors)
  })), na.rm=T)
  
  data.frame(group=group_id,
            acom=mean_similarity)
}

# # test_code
# test_words <- c('littl', 'mani', 'now',
#                'long',
#                'good',
#                'old',
#                'just',
#                'much',
#                'young',
#                'also',
#                'awai',
#                'last',
#                'even',
#                'poor',
#                'still',
#                'alwai',
#                'back',
#                'beauti',
#                'first',
#                'great'
# )
