setwd("C:/Users/nmvenuti/Desktop/Network_Retest")
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
     file="coccurences_piper-wb_1_stem_noReligStpwds_02_02.RData")

load("coccurences_piper-wb_1_stem_noReligStpwds_02_02.RData")
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
# user  system elapsed 
# 5278.25   13.55 5716.81 
wb.dsm <- subset(wb.dsm, nnzero >= 10, nnzero >= 10, T)
system.time({wb.dsm.proj <- dsm.projection(wb.dsm, "rsvd")})
# user  system elapsed 
# 3371.44    5.75 3378.63 
save(jp.dsm.proj, wb.dsm.proj, file="dsm_decomposed.RData")

load("dsm_decomposed.RData")

library(igraph)

#Get cosine similarity of all words in jp and wbc
jp.cosine<- dist.matrix(jp.dsm.proj, names(jp.dsm.proj), method="cosine")
wb.cosine<- dist.matrix(wb.dsm.proj, names(wb.dsm.proj), method="cosine")
save(jp.cosine, wb.cosine, file="dsm_cosine.RData")




#Create adjaceny matricies and add to network
#Assume cosine distance <30 is 1 and greater than 30 =0
jp.adjancey<-jp.cosine
jp.adjancey[jp.cosine>=30]<-0
jp.adjancey[jp.cosine<30]<-1

wb.adjancey<-wb.cosine
wb.adjancey[wb.cosine>=30]<-0
wb.adjancey[wb.cosine<30]<-1

#SAve adjancey networks
save(jp.adjancey,wb.adjancey,file='dsm_adjancey.RData')

#Load adjancey networks
load('dsm_adjancey.RData')

#Create networks and remove matries
jp.net<-graph.adjacency(jp.adjancey,mode='undirected')
rm(jp.adjancey)
wb.net<-graph.adjacency(wb.adjancey,mode='undirected')
rm(wb.adjancey)

#Get edge list
jp.edgelist<-get.edgelist(jp.net)
wb.edgelist<-get.edgelist(wb.net)

#Centrality analysis
#From the following: http://horicky.blogspot.com/2012/04/basic-graph-analytics-using-igraph.html
# Degree centrality gives a higher score to a node that has a high in/out-degree
# Closeness centrality gives a higher score to a node that has short path distance to every other nodes
# Betweenness centrality gives a higher score to a node that sits on many shortest path of other node pairs
# Local cluster coefficient measures how my neighbors are inter-connected with each other, which means the node becomes less important.
# Eigenvector centrality gives a higher score to a node if it connects to many high score nodes

jp.centrality<-subgraph.centrality(jp.net)
jp.closeness<-closeness(jp.net)
jp.betweeness<-betweenness(jp.net)
jp.transitivity<-transitivity(jp.net,type = 'local')
jp.ev.centrality<-evcent(jp.net)$vector
jp.summary<-data.frame(cbind(jp.centrality,jp.closeness,jp.betweeness,jp.transitivity,jp.ev.centrality))
colnames(jp.summary)<-c('subgraph_centrality','closeness','betweenness','transitivity','eigenvector_centrality')


wb.centrality<-subgraph.centrality(wb.net)
wb.closeness<-closeness(wb.net)
wb.betweeness<-betweenness(wb.net)
wb.transitivity<-transitivity(wb.net,type = 'local')
wb.ev.centrality<-evcent(wb.net)$vector
wb.summary<-data.frame(cbind(wb.centrality,wb.closeness,wb.betweeness,wb.transitivity,wb.ev.centrality))
colnames(wb.summary)<-c('subgraph_centrality','closeness','betweenness','transitivity','eigenvector_centrality')

#Save adjancey networks
save(jp.summary,wb.summary,file='dsm_summary.RData')

#Load adjancey networks
load('dsm_summary.RData')

#Get Overall summary
summary(jp.summary)
 
# subgraph_centrality   closeness          betweenness      
# Min.   :1.000e+00   Min.   :1.140e-08   Min.   :   0.000  
# 1st Qu.:1.000e+00   1st Qu.:1.140e-08   1st Qu.:   0.000  
# Median :1.000e+00   Median :1.140e-08   Median :   0.000  
# Mean   :8.500e+11   Mean   :1.141e-08   Mean   :   4.285  
# 3rd Qu.:1.000e+00   3rd Qu.:1.140e-08   3rd Qu.:   0.000  
# Max.   :4.303e+14   Max.   :1.169e-08   Max.   :8292.196  
# transitivity      eigenvector_centrality
# Min.   :0.000000   Min.   :0.000000      
# 1st Qu.:0.000000   1st Qu.:0.000000      
# Median :0.000000   Median :0.000000      
# Mean   :0.008777   Mean   :0.004389      
# 3rd Qu.:0.000000   3rd Qu.:0.000000      
# Max.   :0.778947   Max.   :1.000000   

summary(wb.summary)
# subgraph_centrality   closeness          betweenness      
# Min.   :1.000e+00   Min.   :1.521e-08   Min.   :   0.000  
# 1st Qu.:1.000e+00   1st Qu.:1.521e-08   1st Qu.:   0.000  
# Median :1.000e+00   Median :1.521e-08   Median :   0.000  
# Mean   :4.808e+06   Mean   :1.522e-08   Mean   :   1.961  
# 3rd Qu.:1.000e+00   3rd Qu.:1.521e-08   3rd Qu.:   0.000  
# Max.   :2.972e+09   Max.   :1.547e-08   Max.   :3282.527  
# transitivity      eigenvector_centrality
# Min.   :0.000000   Min.   :0.000000      
# 1st Qu.:0.000000   1st Qu.:0.000000      
# Median :0.000000   Median :0.000000      
# Mean   :0.005532   Mean   :0.003364      
# 3rd Qu.:0.000000   3rd Qu.:0.000000      
# Max.   :0.681818   Max.   :1.000000 

#Add node names in
jp.summary$node<-row.names(jp.summary)
wb.summary$node<-row.names(wb.summary)

#Get top twenty terms from eigenvector centrality for each corpus
library(plyr)
jp.evcen<-arrange(jp.summary,desc(eigenvector_centrality))
jp.evcen[1:20,c('eigenvector_centrality','node')]
# eigenvector_centrality     node
# 1               1.0000000      god
# 2               0.9807844      now
# 3               0.8781484     make
# 4               0.8774369      sai
# 5               0.8618442      wai
# 6               0.8505755    peopl
# 7               0.7792088     know
# 8               0.7724054     just
# 9               0.7407333     show
# 10              0.7014057    thing
# 11              0.6863631    great
# 12              0.6620744      see
# 13              0.6569228 therefor
# 14              0.6373552     mean
# 15              0.5890617     even
# 16              0.5849988     like
# 17              0.5557562   believ
# 18              0.5486666      can
# 19              0.5356259    final
# 20              0.5355005     caus

wb.evcen<-arrange(wb.summary,desc(eigenvector_centrality))
wb.evcen[1:20,c('eigenvector_centrality','node')]
# eigenvector_centrality     node
# 1               1.0000000      god
# 2               0.9370898      now
# 3               0.7627313     just
# 4               0.7577815    thing
# 5               0.7411605      sai
# 6               0.7366615     make
# 7               0.7265771     give
# 8               0.7170461      wai
# 9               0.6933429      see
# 10              0.6889945 therefor
# 11              0.6446228    peopl
# 12              0.6310836      can
# 13              0.6217809     work
# 14              0.6202885     word
# 15              0.6136253     know
# 16              0.6073142     even
# 17              0.6010633     mean
# 18              0.5921653    heart
# 19              0.5906012     show
# 20              0.5854241    power

#Get top twenty terms from subgraph centrality for each corpus
jp.sgcen<-arrange(jp.summary,desc(subgraph_centrality))
jp.sgcen[1:20,c('subgraph_centrality','node')]
# subgraph_centrality     node
# 1         4.303205e+14      god
# 2         4.139417e+14      now
# 3         3.318393e+14     make
# 4         3.313018e+14      sai
# 5         3.196315e+14      wai
# 6         3.113277e+14    peopl
# 7         2.612761e+14     know
# 8         2.567335e+14     just
# 9         2.361108e+14     show
# 10        2.117047e+14    thing
# 11        2.027215e+14    great
# 12        1.886278e+14      see
# 13        1.857038e+14 therefor
# 14        1.748055e+14     mean
# 15        1.493185e+14     even
# 16        1.472658e+14     like
# 17        1.329109e+14   believ
# 18        1.295416e+14      can
# 19        1.234568e+14    final
# 20        1.233990e+14     caus

wb.sgcen<-arrange(wb.summary,desc(subgraph_centrality))
wb.sgcen[1:20,c('subgraph_centrality','node')]
# subgraph_centrality     node
# 1           2972482569      god
# 2           2610247976      now
# 3           1729268904     just
# 4           1706896906    thing
# 5           1632840843      sai
# 6           1613077687     make
# 7           1569215866     give
# 8           1528317022      wai
# 9           1428944744      see
# 10          1411077905 therefor
# 11          1235181237    peopl
# 12          1183840680      can
# 13          1149195800     work
# 14          1143686018     word
# 15          1119246819     know
# 16          1096343077     even
# 17          1073890364     mean
# 18          1042330115    heart
# 19          1036830958     show
# 20          1018733457    power
#Look at value words
all_nodes<-unique(row.names(jp.summary),row.names(wb.summary))
all_nodes<-sort(all_nodes)
write.csv(all_nodes,file = 'all_nodes.csv')


value_words=c('disbelief', 'evil', 'fidel', 'god', 'hate', 'ignor', 'islam', 'jew', 'jewish', 'justic', 
              'love', 'loyalti', 'muslim', 'sin', 'treacher', 'treacheri', 'ungrat')

# Value Matricies
jp.value<-jp.summary[jp.summary$node %in% value_words,]
jp.value
# subgraph_centrality    closeness betweenness transitivity eigenvector_centrality      node
# love             9.727002e+11 1.168653e-08       0.000    0.1666667           0.0475437556      love
# god              4.303205e+14 1.168657e-08    8292.196    0.1275951           1.0000000000       god
# hate             1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000      hate
# sin              2.178184e+00 1.140574e-08       1.000    0.0000000           0.0000000000       sin
# ungrat           1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000    ungrat
# evil             1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000      evil
# ignor            1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000     ignor
# justic           5.230988e+07 1.168648e-08       0.000    0.0000000           0.0003486549    justic
# jewish           1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000    jewish
# jew              1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000       jew
# muslim           1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000    muslim
# loyalti          1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000   loyalti
# treacher         1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000  treacher
# disbelief        1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000 disbelief
# islam            1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000     islam
# fidel            1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000     fidel
# treacheri        1.000000e+00 1.140331e-08       0.000    0.0000000           0.0000000000 treacheri

wb.value<-wb.summary[wb.summary$node %in% value_words,]
wb.value
# subgraph_centrality    closeness betweenness transitivity eigenvector_centrality     node
# god             2.972483e+09 1.547485e-08    3282.527    0.1462963           1.000000e+00      god
# love            1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15     love
# evil            1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15     evil
# sin             1.543081e+00 1.521527e-08       0.000    0.0000000           1.297208e-15      sin
# jew             1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15      jew
# jewish          1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15   jewish
# ignor           1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15    ignor
# justic          1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15   justic
# muslim          1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15   muslim
# hate            1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15     hate
# islam           1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15    islam
# fidel           1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15    fidel
# ungrat          1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15   ungrat
# loyalti         1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15  loyalti
# treacher        1.000000e+00 1.521339e-08       0.000    0.0000000           1.281503e-15 treacher

#Find cliques
jp.cliques<-max_cliques(jp.net,max=20)

wb.cliques<-max_cliques(wb.net,max=20)

plot.igraph(jp.net)

library(InteractiveIGraph)
g = InteractiveIGraph.Constructor(jp.net)
g = plot(g)
