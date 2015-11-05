setwd("Capstone/chetan mishra sandbox/semantic range modeling/")
source("methods.R")

# create TDM
numCores <- 2
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

writeCorpus(processedCorp, "processedCorps_wb_johnpiper")

clusterEvalQ(cl, {
  k <- 5
  bySentence <- F
})
cooccurences <- rbindlist(parLapply(cl, processedCorps, function(processedcorp) {
  rbindlist(lapply(processedcorp, function(doc) 
    wordCoOccurences(doc$content, k, bySentence)))
}))

format(object.size(coocurrences),"MB")
save(coocurrences, file="coccurences_piper-wb_1_stem_noReligStpwds_11_03.RData")
stopCluster(cl)

load("piper_wb_1_stem_noReligStpwds_10_27.RData")
tdm.dsm <- as.dsm(tdm)
dim(tdm.dsm)
dim(subset(tdm.dsm, nnzero>=3, nnzero>=3))

dsm.scored <- dsm.score(tdm.dsm, )

# code profiling and speeding for wordCoOccurences
Rprof("wordCoOccurrences Profiling.txt", interval=0.1)
rbindlist(lapply(processedCorps[[1]][1], function(doc) 
  wordCoOccurences(doc$content, 5)))
Rprof(NULL)
summaryRprof("wordCoOccurrences Profiling.txt")
