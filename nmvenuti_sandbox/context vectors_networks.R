setwd("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/")
source("methods.R")

# Set up clustering parameters
numCores <- 4
cl <- makeCluster(mc <- getOption("cl.cores", numCores))
clusterEvalQ(cl, {source("methods.R")})

#Define corpuses for Piper and WBC. Segment and combine for parrelization
data_johnpiper <- VCorpus(DirSource("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/Data Sources/john_piper"))
data_wbc <- VCorpus(DirSource("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/Data Sources/westboro_sermons/"))
meta(data_johnpiper, "author") <- "johnpiper"
meta(data_wbc, "author") <- "westboro"
corps <- c(data_johnpiper, data_wbc)

#Preprocess combined documents (done in parallel)
splitIndex <- rep_len(1:numCores, length(corps))
processedCorps <- parLapply(cl, split(corps, splitIndex),
                        function (x) preprocessing(x, stpwds, wordStem))
processedCorp <- do.call(function(...) c(..., recursive=T), 
                         processedCorps)


#Write and separate corpuses
# writeCorpus(processedCorp, "processedCorps_wb_johnpiper")

piperCorps <- processedCorp[meta(corps) == "johnpiper"]
wbcCorps <- processedCorp[meta(corps) == "westboro"]

#Set up clustering for word occurrences analysis
clusterEvalQ(cl, {
    k <- 5
    bySentence <- F
})

#Perform word cocourrence analysis for piper and wbc
johnpiper_wordcoocurrences <- rbindlist(parLapplyLB(cl, piperCorps, function(doc) 
  wordCoOccurences(doc$content, k, bySentence)))
system.time({wbc_wordcoocurrences <- rbindlist(parLapplyLB(cl, wbcCorps, function(doc) 
  wordCoOccurences(doc$content, k, bySentence)))})
stopCluster(cl)

#Reference runtime from NMV run
# user  system elapsed 
# 1.20    0.46  253.62 

johnpiper_wordcoocurrences <- 
  johnpiper_wordcoocurrences[target != "." & context != ".",
                           .(freq=sum(freq)), 
                           .(target, context)]

wbc_wordcoocurrences <- 
  wbc_wordcoocurrences[target != "." & context != ".",
                       .(freq=sum(freq)), 
                       .(target, context)]

#Save backup
save(johnpiper_wordcoocurrences, 
     wbc_wordcoocurrences,
     file="coccurences_piper-wb_1_stem_noReligStpwds_11_03.RData")

#Load backup from here if needed
load("coccurences_piper-wb_1_stem_noReligStpwds_11_03.RData")


#Subset vectors only greater than 25 matches for piper
jp.countWords <- johnpiper_wordcoocurrences[,.(length(context)), 
                                            by=target][V1 > 25]$target
johnpiper_wordcoocurrences <- johnpiper_wordcoocurrences[target %in% jp.countWords &
                                                          context %in% jp.countWords]
johnpiper_wordcoocurrences[,c("target", "context"):=
                             list(as.factor(target), as.factor(context))]

#run Distributional semantic model(DSM) for piper
jp.dsm <- dsm(target=johnpiper_wordcoocurrences$target,
              feature=johnpiper_wordcoocurrences$context,
              score=johnpiper_wordcoocurrences$freq,
              N=100)

#Subset vectors only greater than 25 matches for wbc
wb.countwords <- wbc_wordcoocurrences[,.(length(context)), 
                                            by=target][V1 > 25]$target
wbc_wordcoocurrences <- wbc_wordcoocurrences[target %in% wb.countwords &
                                                           context %in% wb.countwords]
wbc_wordcoocurrences[,c("target", "context"):=
                             list(as.factor(target), as.factor(context))]

#run Distributional semantic model(DSM) for wbc
wb.dsm <- dsm(target=wbc_wordcoocurrences$target,
              feature=wbc_wordcoocurrences$context,
              score=wbc_wordcoocurrences$freq,
              N=100)

#Subset DSM for piper, project DSM into lower-dimenstional subspace
jp.dsm <- subset(jp.dsm, nnzero >= 10, nnzero >= 10, T)
system.time({jp.dsm.proj <- dsm.projection(jp.dsm, "svd")})

# user  system elapsed 
# 4711.00   10.45 4764.97

#Subset DSM for wbc, project DSM into lower-dimenstional subspace
wb.dsm <- subset(wb.dsm, nnzero >= 10, nnzero >= 10, T)
system.time({wb.dsm.proj <- dsm.projection(wb.dsm, "svd")})

# user  system elapsed 
# 2185.13    3.40 2214.07 

save(jp.dsm.proj, wb.dsm.proj, file="dsm_decomposed.RData")

load("dsm_decomposed.RData")

pair.distances("love", "hate", jp.dsm.proj, method="cosine", convert=F)
value_words<-c('love','hate','god','sin',"beauti","ignorance", "evil", 
               "justice", "disbelief", "ungratefulness", "jewish", "fidelity", "loyalty", "treachery")
i=1

#Create Neighboorhod maps for WBC
nn<-nearest.neighbours(wb.dsm.proj, value_words[i], 10)
nn
#NN for love/god 23.30456
pair.distances("love", "god", jp.dsm.proj, method="cosine", convert=T)
# 23.78865 

nn.terms<-c("love", names(nn))
nn.dist <- dist.matrix(wb.dsm.proj, terms=nn.terms, method="cosine")


plot(nn.dist, pch=20, col="red")
title(main="WBC Neighborhood Map-Love")

#Create Neighboorhood Maps for JP
nn<-nearest.neighbours(jp.dsm.proj, value_words[i], 15)
nn

nn.terms<-c("love", names(nn))
nn.dist <- dist.matrix(wb.dsm.proj, terms=nn.terms, method="cosine")


plot(nn.dist, pch=20, col="red")
title(main="JP Neighborhood Map-Love")



#Plot Similary correlations for WBC
plot(eval.similarity.correlation(RG65, wb.dsm.proj, format="HW", details=TRUE))

#Plot Similarity Correlations for JP
plot(eval.similarity.correlation(RG65, jp.dsm.proj, format="HW", details=TRUE))


# Ward Hierarchical Clustering
d <- dist(jp.dsm.proj, method = "euclidean") # distance matrix
fit <- hclust(nn, method="ward") 
plot(nn) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")


##########################
#####Network analysis#####
##########################

library(network)
library(sna)


load("dsm_decomposed.RData")

#Get cosine similarity of all words in jp and wbc
jp.cosine<- dist.matrix(jp.dsm.proj, names(jp.dsm.proj), method="cosine")
wbc.cosine<- dist.matrix(wb.dsm.proj, names(wb.dsm.proj), method="cosine")

#Create networks
jp.network<-network(jp.cosine)

head(jp.cosine)
#Network analysis on love and 1000 closest words for john piper
nn<-nearest.neighbours(jp.dsm.proj, 'love', 1000)

nn.terms<-c("love", names(nn))
jp.love.cosine<- dist.matrix(jp.dsm.proj, terms=nn.terms, method="cosine")

jp.love.network<-network(jp.love.cosine,directed=FALSE)

#Get network summary
network.dyadcount(jp.love.network)
# 500500
network.edgecount(jp.love.network)
# 500500
network.size(jp.love.network)
# 1001

plot(jp.love.network,displaylabels=T)

#Basic connectivity/distance measurement and cohesion

#Check if graph is strongly connected
is.connected(jp.love.network)
#True

#Get geo distance
x<-geodist(jp.love.network)


#Get the reachability matrix
jp.love.reachability<-reachability(jp.love.network)
head(jp.love.reachability)

library(igraph)

#Get cosine similarity of all words in jp and wbc
jp.cosine<- dist.matrix(jp.dsm.proj, names(jp.dsm.proj), method="cosine")
wb.cosine<- dist.matrix(wb.dsm.proj, names(wb.dsm.proj), method="cosine")
save(jp.cosine, wb.cosine, file="dsm_cosine.RData")
min(jp.cosine)
# 0
max(jp.cosine)
# 90
mean(jp.cosine)
# 77.00492

min(wb.cosine)
# 0
max(wb.cosine)
# 90
mean(wb.cosine)
# 77.78912



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
# subgraph_centrality   closeness          betweenness        transitivity     eigenvector_centrality
# Min.   :1.000e+00   Min.   :8.811e-09   Min.   :    0.00   Min.   :0.00000   Min.   :0.00000       
# 1st Qu.:1.000e+00   1st Qu.:8.811e-09   1st Qu.:    0.00   1st Qu.:0.00000   1st Qu.:0.00000       
# Median :1.000e+00   Median :8.811e-09   Median :    0.00   Median :0.00000   Median :0.00000       
# Mean   :1.215e+54   Mean   :8.851e-09   Mean   :   33.58   Mean   :0.03455   Mean   :0.01333       
# 3rd Qu.:1.000e+00   3rd Qu.:8.811e-09   3rd Qu.:    0.00   3rd Qu.:0.00000   3rd Qu.:0.00000       
# Max.   :1.816e+56   Max.   :9.429e-09   Max.   :22577.19   Max.   :0.90852   Max.   :1.00000  

summary(wb.summary)
# subgraph_centrality   closeness          betweenness        transitivity    eigenvector_centrality
# Min.   :1.000e+00   Min.   :1.349e-08   Min.   :    0.00   Min.   :0.0000   Min.   :0.00000       
# 1st Qu.:1.000e+00   1st Qu.:1.349e-08   1st Qu.:    0.00   1st Qu.:0.0000   1st Qu.:0.00000       
# Median :1.000e+00   Median :1.349e-08   Median :    0.00   Median :0.0000   Median :0.00000       
# Mean   :3.843e+31   Mean   :1.353e-08   Mean   :   13.86   Mean   :0.0249   Mean   :0.01031       
# 3rd Qu.:1.000e+00   3rd Qu.:1.349e-08   3rd Qu.:    0.00   3rd Qu.:0.0000   3rd Qu.:0.00000       
# Max.   :7.586e+33   Max.   :1.418e-08   Max.   :12500.47   Max.   :0.8571   Max.   :1.00000   

#Add node names in
jp.summary$node<-row.names(jp.summary)
wb.summary$node<-row.names(wb.summary)

#Get top ten terms from eigenvector centrality for each corpus
library(plyr)
jp.evcen<-arrange(jp.summary,desc(eigenvector_centrality))
jp.evcen[1:10,]

wb.evcen<-arrange(wb.summary,desc(eigenvector_centrality))
wb.evcen[1:10,]

#Look at value words
all_nodes<-unique(row.names(jp.summary),row.names(wb.summary))
all_nodes<-sort(all_nodes)
write.csv(all_nodes,file = 'all_nodes.csv')
# value_words<-c('love','hate','god','sin',"beauti","ignorance", "evil", 
# "justice", "disbelief", "ungratefulness", "jewish", "fidelity", "loyalty", "treachery")

value_words=c('disbelief', 'evil', 'fidel', 'god', 'hate', 'ignor', 'islam', 'jew', 'jewish', 'justic', 
              'love', 'loyalti', 'muslim', 'sin', 'treacher', 'treacheri', 'ungrat')

# Value Matricies
jp.value<-jp.summary[jp.summary$node %in% value_words,]
jp.value

#           subgraph_centrality    closeness  betweenness transitivity eigenvector_centrality      node
# love             3.543297e+54 9.429382e-09 2.212441e+01    0.6969697             0.13966958      love
# god              1.753584e+56 9.429427e-09 2.257719e+04    0.1954865             0.98256515       god
# hate             6.535902e+54 9.429384e-09 1.575829e+00    0.8573574             0.18969278      hate
# sin              2.912785e+55 9.429394e-09 2.589208e+03    0.6174692             0.40045364       sin
# ungrat           1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000    ungrat
# evil             3.891734e+53 9.429369e-09 3.562092e-01    0.6000000             0.04628813      evil
# jew              1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000       jew
# justic           3.082093e+53 9.429370e-09 3.393895e+01    0.6274510             0.04119276    justic
# ignor            4.663724e+54 9.429383e-09 2.739726e-02    0.8620690             0.16023760     ignor
# treacheri        1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000 treacheri
# jewish           1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000    jewish
# muslim           1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000    muslim
# islam            1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000     islam
# loyalti          1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000   loyalti
# treacher         1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000  treacher
# fidel            1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000     fidel
# disbelief        1.000000e+00 8.810801e-09 0.000000e+00    0.0000000             0.00000000 disbelief

wb.value<-wb.summary[wb.summary$node %in% value_words,]
wb.value
#         subgraph_centrality    closeness betweenness transitivity eigenvector_centrality     node
# god             7.335000e+33 1.418454e-08 12500.46685    0.1736952           9.833030e-01      god
# love            3.032819e+32 1.418448e-08    24.51621    0.6403941           1.999450e-01     love
# fidel           1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16    fidel
# sin             3.713754e+32 1.418448e-08  1267.08151    0.5546218           2.212554e-01      sin
# jew             1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16      jew
# jewish          1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16   jewish
# justic          2.695113e+30 1.418443e-08     0.00000    0.3000000           1.884846e-02   justic
# hate            3.968774e+31 1.418447e-08     0.00000    0.6545455           7.232950e-02     hate
# evil            4.373701e+31 1.418447e-08     0.00000    0.6222222           7.592973e-02     evil
# ignor           5.867138e+31 1.418447e-08     0.00000    0.6545455           8.794283e-02    ignor
# treacher        1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16 treacher
# muslim          1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16   muslim
# ungrat          1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16   ungrat
# islam           1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16    islam
# loyalti         1.000000e+00 1.349100e-08     0.00000    0.0000000           3.859560e-16  loyalti

