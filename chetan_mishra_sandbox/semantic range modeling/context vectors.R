setwd("Dropbox/College/4_Fourth_Year/Capstone/chetan_mishra_sandbox/semantic range modeling/")
source("methods.R")

# create TDM
numCores <- 4
cl <- makeCluster(mc <- getOption("cl.cores", numCores))
clusterEvalQ(cl, {source("methods.R")})

data_johnpiper <- VCorpus(DirSource("../webscraping sermoncentral/john_piper/"))
data_wbc <- VCorpus(DirSource("../webscraping westboro/sermons/"))
meta(data_johnpiper, "author") <- "johnpiper"
meta(data_wbc, "author") <- "westboro"
corps <- c(data_johnpiper, data_wbc)

splitIndex <- rep_len(1:numCores, length(corps))
processedCorps <- parLapply(cl, split(corps, splitIndex),
                        function (x) preprocessing(x, stpwds, wordStem))
processedCorp <- do.call(function(...) c(..., recursive=T), 
                         processedCorps)
# tdms <- parLapply(cl, processedCorps, TermDocumentMatrix)
# tdm <- do.call(function(...) c(..., recursive=T), tdms)

# writeCorpus(processedCorp, "processedCorps_wb_johnpiper")

piperCorps <- processedCorp[meta(corps) == "johnpiper"]
wbcCorps <- processedCorp[meta(corps) == "westboro"]

clusterEvalQ(cl, {
    k <- 5
    bySentence <- F
})

johnpiper_wordcoocurrences <- rbindlist(parLapplyLB(cl, piperCorps, function(doc) 
  wordCoOccurences(doc$content, k, bySentence)))
system.time({wbc_wordcoocurrences <- rbindlist(parLapplyLB(cl, wbcCorps, function(doc) 
  wordCoOccurences(doc$content, k, bySentence)))})
stopCluster(cl)


johnpiper_wordcoocurrences <- 
  johnpiper_wordcoocurrences[target != "." & context != ".",
                           .(freq=sum(freq)), 
                           .(target, context)]

wbc_wordcoocurrences <- 
  wbc_wordcoocurrences[target != "." & context != ".",
                       .(freq=sum(freq)), 
                       .(target, context)]

save(johnpiper_wordcoocurrences, 
     wbc_wordcoocurrences,
     file="coccurences_piper-wb_1_stem_noReligStpwds_11_03.RData")

load("coccurences_piper-wb_1_stem_noReligStpwds_11_03.RData")
jp.countWords <- johnpiper_wordcoocurrences[,.(length(context)), 
                                            by=target][V1 > 25]$target
johnpiper_wordcoocurrences <- johnpiper_wordcoocurrences[target %in% jp.countWords &
                                                          context %in% jp.countWords]
johnpiper_wordcoocurrences[,c("target", "context"):=
                             list(as.factor(target), as.factor(context))]
jp.dsm <- dsm(target=johnpiper_wordcoocurrences$target,
              feature=johnpiper_wordcoocurrences$context,
              score=johnpiper_wordcoocurrences$freq)

wb.countwords <- wbc_wordcoocurrences[,.(length(context)), 
                                            by=target][V1 > 25]$target
wbc_wordcoocurrences <- wbc_wordcoocurrences[target %in% wb.countwords &
                                                           context %in% wb.countwords]
wbc_wordcoocurrences[,c("target", "context"):=
                             list(as.factor(target), as.factor(context))]
wb.dsm <- dsm(target=wbc_wordcoocurrences$target,
              feature=wbc_wordcoocurrences$context,
              score=wbc_wordcoocurrences$freq)

jp.dsm <- subset(jp.dsm, nnzero >= 10, nnzero >= 10, T)
system.time({jp.dsm.proj <- dsm.projection(jp.dsm, "rsvd")})
wb.dsm <- subset(wb.dsm, nnzero >= 10, nnzero >= 10, T)
system.time({wb.dsm.proj <- dsm.projection(wb.dsm, "rsvd")})
save(jp.dsm.proj, wb.dsm.proj, file="dsm_decomposed.RData")

load("dsm_decomposed.RData")

pair.distances("love", "hate", jp.dsm.proj, method="cosine", convert=F)
nearest.neighbours(wb.dsm.proj, "holy")
