
parJudgements <- function(filepath,i) {
  
  # All Required Packages
  # Need to figure out 'wordspace'
  requiredLibs <- c('tm','RWeka','SnowballC','parallel','data.table',
                    'compiler','Rcpp','RcppArmadillo','stringr','plyr', 'openNLP', 'lsa')

  checkandloadlibrary <- function(package){
    if(!package %in% .packages(all.available = TRUE)){
      print(paste0("installing ",package,"..."))
      install.packages(package, repos = 'http://cran.cnr.berkeley.edu/')
    }
    print(paste0("loading ",package,"..."))
    library(package, character.only = T)
  }
  
  for (lib in requiredLibs){
    checkandloadlibrary(lib)
  }
  

  
  function.sources = list.files('./prototype', pattern="method_")
  function.sources = sapply(function.sources, function(x) paste0('./prototype/', x))
  sapply(function.sources,source,.GlobalEnv)
  
  # Get List of Groups in Folder Structure
  files <- list.files('./data_dsicap')
  groups  <- files[files != 'ref']
  print(groups)
  

  subgroupList<-c()
  for (group in groups) {
    # print(group)
    for (type in c('train','test')){
      dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
      IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
      IDs <- IDs[!is.na(IDs)]
      for (id in IDs) {
        groupID <- paste0(group,"_",id)
        print(groupID)
        subgroupList<-c(subgroupList,groupID)
      }
    }
  }
  
  id = NULL
  group = NULL
  
  groupID <- subgroupList[i]
  bin <- str_split(groupID,"_")
  id <- bin[[1]][2]
  group <- bin[[1]][1]
  type <- gsub("[^a-z]","",id)
  index <- gsub("[^0-9]","",id)
  
  indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
  corpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[index,]))

  toBe <- c("is","was","am","are","were","been","be","being")
  
  sentence.annotator = Maxent_Sent_Token_Annotator()
  token.annotator = Maxent_Word_Token_Annotator()
  pos.annotator = Maxent_POS_Tag_Annotator()
  
  # Iterate through all Documents
  bigMaster <- c()
  # for (d in 1:4){
  #for (d in 1:4) {
  for (d in 1:length(corpus)){
    #doc <- corpus[[d]]$content
    doc <- as.String(corpus[[d]])
    ##### Sentence segmentation -- assumes you have a document (our "text" variable)
    #####
    # sentence.annotator = Maxent_Sent_Token_Annotator()
    sentence.annotations = annotate(doc, sentence.annotator)
    
    #####
    ##### Tokenization -- buids on sentence segmentation
    #####
    # token.annotator = Maxent_Word_Token_Annotator()
    token.annotations = annotate(doc, token.annotator, sentence.annotations)
    
    # Prints all Tokens (sentences and words)
    
    ##### Sentence segmentation and tokenization are basic steps required for many other analyses:  POS tagging, NEI, and syntactic parsing.
    
    #####
    ##### Part-of-speech (POS) tagging -- builds on tokenization
    #####
    # pos.annotator = Maxent_POS_Tag_Annotator()
    pos.annotations = annotate(doc, pos.annotator, token.annotations)
    
    ### PROBLEM
    master <- as.data.frame(doc[sentence.annotations])
    # View(master)
    
    # Create Word only POS Subset
    # POSw <- subset(pos.annotations, type == "word")
    POSs <- as.data.frame(subset(pos.annotations, type == "sentence"))
    # View(POSs)
    
    
    wordsAll <- list()
    partsAll <- list()
    adjAll <- list()
    toBeAll <- list()
    nounAll <- list()
    
    for (i in seq(nrow(POSs))) {
      x <- as.numeric(unlist(((POSs[i,"features"][[1]]))))
      y <- as.data.frame(pos.annotations[x])
      words <- c()
      parts <- c()
      adj_flag <- 0
      toBe_flag <- 0
      noun_flag <- 0
      
      for (j in seq(1,nrow(y))) {
        word <- substr(doc,y[j,"start"],y[j,"end"])
        if (toBe_flag == 0) {
          if (word %in% toBe) {
            toBe_flag <- 1
          }
        }
        words <- append(words,word)
        
        part <- unlist(y[j,"features"])
        if (adj_flag == 0) {
          # print(adj_flag)
          if (part %in% c("JJ", "JJR", "JJS", "RB","RBR", "RBS")) {
            adj_flag <- 1
            # print(adj_flag)
          }
          # print(adj_flag)
        }
        # print(adj_flag)
        
        # Check for Noun
        if (noun_flag == 0) {
          # print(adj_flag)
          if (part %in% c('NN','NNP','NNS','NNPS')) {
            noun_flag <- 1
            # print(adj_flag)
          }
          # print(adj_flag)
        }
        # print(adj_flag)
        parts <- append(parts, part)
        # print(paste("added ",substr(doc1,y[i,"start"],y[i,"end"])))
      }
      wordsAll[[i]] <- list(words)
      partsAll[[i]] <- list(parts)
      adjAll[[i]] <- adj_flag
      toBeAll[[i]] <- toBe_flag
      nounAll[[i]] <- noun_flag
    }
    master$words <- wordsAll
    master$parts <- partsAll
    master$adj <- adjAll
    master$toBe <- toBeAll
    master$noun <- nounAll
    # View(master)
    master$judgement <- ifelse(master$adj == 1 & master$toBe == 1 & master$noun == 1, 1, 0)
    # View(master)
    
    master$docName <- meta(corpus[[d]], "id")
    # print(head(master))
    bigMaster <- rbind(bigMaster, master)
  }
  
  
  #avgJudgements <- data.frame(docName = c(), rowNumber = c(), judgementsNum = c(), percentJudgements = c())
  avgJ <- c()
  numJ <- c()
  for (i in unique(bigMaster$docName)){
    rowTot <- nrow(bigMaster[which(bigMaster$docName == i),])
    judges <- nrow(bigMaster[which(bigMaster$docName == i & bigMaster$judgement == 1),])
    
    percentJ <- round(judges/rowTot,4)
    avgJ <- append(avgJ, percentJ)
    numJ <- append(numJ, judges)
    
  }
  
  metrics <- data.frame("group" = groupName, 
                        "avgPercJ" = mean(avgJ),
                        "avgNumJ" = mean(numJ))
  
  write.csv(metrics, paste0("./data_dsicap/ref/signal_judgements_",groupID,".csv"))
  
  
  
  
}

