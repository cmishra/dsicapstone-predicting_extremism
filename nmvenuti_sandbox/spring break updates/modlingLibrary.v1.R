#Modeling library
#Will use artifical neural networdks, ordered logistic, random forest, and support vector machines
#Load test data for analysis
setwd("~/GitHub/dsicapstone-predicting_extremism/nmvenuti_sandbox")
filepath='C:/Users/nmvenuti/Documents/GitHub/dsicapstone-predicting_extremism/nmvenuti_sandbox/spring break updates/Test_files'

####################
#####Data Input#####
####################
library('stringr')

# dat<-data.frame()

dataFiles <- list.files(paste0(filepath))

#Get all inputs except semco
for (inputFile in dataFiles[str_detect(dataFiles,paste0('signal'))]) {
  
  #Read in data
  signalData<-data.frame(read.csv(paste0(filepath,'/',inputFile)))
  signalData$parameter<-gsub('.csv','',gsub('signal_', "", inputFile))
  ifelse(exists('dat'),dat<-cbind(dat,signalData),dat<-signalData)
}

#Add semco columns
dat[,c('semco1','semco2','semco3','semco4','semco5','semco6','semco7','semco8','semco9','semco10')]<-NA

#Get semco files
dataFiles <- list.files(paste0(filepath,'/semco'))

#Get all inputs except semco
for (inputFile in dataFiles[str_detect(dataFiles,paste0('signal'))]) {
  
  #Read in data
  signalData<-data.frame(read.csv(paste0(filepath,'/semco','/',inputFile),stringsAsFactors = F))
  groupName<-signalData$groupName[1]
  dat$semco1[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='X0']
  dat$semco2[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V2']
  dat$semco3[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V3']
  dat$semco4[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V4']
  dat$semco5[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V5']
  dat$semco6[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V6']
  dat$semco7[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V7']
  dat$semco8[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V8']
  dat$semco9[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V9']
  dat$semco10[dat$group==groupName]<-signalData$t.cvCosineSim.[signalData$X=='V10']
}

nrow(dat)
length(dataFiles)
#Remove first column (not used)
dat<-data.frame(dat[,-1])

#Add average semco
dat$averageSemco<-apply(dat[,c('semco1','semco2','semco3','semco4','semco5','semco6','semco7','semco8','semco9','semco10')],1,mean)


#Create group lookup table
groupName<-c('WBC', 'PastorAnderson', 'NaumanKhan', 'DorothyDay', 'JohnPiper', 'Shepherd',
'Rabbinic', 'Unitarian', 'MehrBaba')
groupRank<-c(1,2,3,4,4,4,6,7,8)
groupRank<-cbind.data.frame(groupName,groupRank)

#Add in response variable for groups
dat$groupRank<-999
for (name in groupName){
  dat$groupRank[grep(name,dat$group)]=print(groupRank$groupRank[groupRank$groupName==name])
}

#Split into test and training

testDat<-dat[grep("test", dat$group),]
trainDat<-dat[grep("train", dat$group),]


y<-'groupRank'
hiddenLayers=1
thresholdValue=0.1
learningrate=0.01
activationFunction='logistic' #can also use tanh
trees<-100

#Full model-1
x<-c('acom',"subgraph_centrality","eigenvector_centrality","X.PosWords","X.NegWords" ,'semco1','semco2','semco3',
     'semco4','semco5','semco6','semco7','semco8','semco9','semco10')

MSE=F

##################
#####Function#####
##################

testVariables<-function(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees,MSE=F){
  #Create formula
  dataFormula<-paste(c(y," ~ ",paste(x, collapse ="+")),collapse = "")
  
  
  
  ##################################
  #####Artifical Neural Network#####
  ##################################
  library(neuralnet)
  netModel <- neuralnet(as.formula(dataFormula), trainDat, hidden=hiddenLayers,threshold=thresholdValue,learningrate=learningrate,algorithm = 'rprop+' ,act.fct=activationFunction)
  netSummary<-summary(netModel)
  netTrainResults<-compute(netModel,trainDat[,x])
  netTestResults<-compute(netModel,testDat[,x])
  
  #Get results
  
  nn_accuracy<-ifelse(MSE,mean((testDat$groupRank-netTestResults$net.result)^2),nrow(testDat[abs(testDat$groupRank-netTestResults$net.result)<1,])/nrow(testDat))

  
  ##########################
  #####Ordered logistic#####
  ##########################
  
  #Load library
  library(MASS)
  
  #Create data for ol
  olTestDat<-testDat
  olTrainDat<-trainDat
  
  #Ensure response is factor
  olTestDat[,y]<-factor(olTestDat[,y],ordered=TRUE)
  olTrainDat[,y]<-factor(olTrainDat[,y],ordered=TRUE)
  
  #Create model
  olModel<-polr(as.formula(dataFormula),data=olTrainDat,Hess=TRUE)
  
  #Extract summary data
  olSummary<-summary(olModel)
  
  #Create prediction
  olTestResults<-as.integer(predict(olModel,olTestDat[,x]))
  olTrainResults<-as.integer(predict(olModel,olTrainDat[,x]))
  
  
  #Get results
  ol_accuracy<-ifelse(MSE,mean((testDat$groupRank-olTestResults)^2),nrow(testDat[abs(testDat$groupRank-olTestResults)<1,])/nrow(testDat))
  
  
  # #Save model and summary information
  # save(olModel,olSummary,file="olOutput.RData")
  
  ####Should we be using factors or continuos here (kinda feelin contiuous)
  
  #######################
  #####Random Forest#####
  #######################
  library(randomForest)
  rfModel<-randomForest(as.formula(dataFormula),data = trainDat,ntree=trees)
  rfSummary<-summary(rfModel)
  rfTrainResults<-predict(rfModel,newdata = trainDat)
  rfTestResults<-predict(rfModel,newdata = testDat)
  
  #Output train and test accuracy
  rf_accuracy<-ifelse(MSE,mean((testDat$groupRank-rfTestResults)^2),nrow(testDat[abs(testDat$groupRank-rfTestResults)<1,])/nrow(testDat))
  
  
  #################################
  #####Support Vector Machines#####
  #################################
  library(e1071)
  svmModel=svm(trainDat[,x],trainDat[,y], kernel ="radial", degree = 3, probability = TRUE)
  svmSummary<-summary(svmModel)
  svmTrainResults<-predict(svmModel,trainDat[,x])
  svmTestResults<-predict(svmModel,testDat[,x])
  
  #Output train and test accuracy
  svm_accuracy<-ifelse(MSE,mean((testDat$groupRank-svmTestResults)^2),nrow(testDat[abs(testDat$groupRank-svmTestResults)<1,])/nrow(testDat))
  output<-data.frame(c(nn_accuracy,ol_accuracy,rf_accuracy,svm_accuracy))
  
  results<-data.frame(cbind(testDat$groupRank,netTestResults$net.result,olTestResults,rfTestResults,svmTestResults))
  colnames(results)<-c('Actual','ANN','OL','RF','SVM')
  
  output$test<-c('ANN','OL','RF','SVM')
  colnames(output)<-c('accuracy','test')
  return(output,results)
}
#Set up variable testing
y<-'groupRank'
hiddenLayers=1
thresholdValue=0.1
learningrate=0.01
activationFunction='logistic' #can also use tanh
trees<-100

#Full model-1
x<-c('acom',"subgraph_centrality","eigenvector_centrality","X.PosWords","X.NegWords" ,'semco1','semco2','semco3',
     'semco4','semco5','semco6','semco7','semco8','semco9','semco10')
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees,MSE = F)
#         accuracy test
# 1 0.7142857143  ANN
# 2 0.7040816327   OL
# 3 0.7959183673   RF
# 4 0.7346938776  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees,MSE = T)
#       accuracy test
# 1 1.4601734366  ANN
# 2 2.0714285714   OL
# 3 0.6991745088   RF
# 4 1.0004589971  SVM

#Full model-2
x<-c('acom',"subgraph_centrality","eigenvector_centrality","X.PosWords","X.NegWords" ,'averageSemco')
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#       accuracy test
# 1 0.6632653061  ANN
# 2 0.7142857143   OL
# 3 0.8571428571   RF
# 4 0.8265306122  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.5320508387  ANN
# 2 1.9489795918   OL
# 3 0.6552333141   RF
# 4 0.7066572538  SVM

#Sentiment
x<-c("X.PosWords","X.NegWords")
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#       accuracy test
# 1 0.6326530612  ANN
# 2 0.7040816327   OL
# 3 0.6836734694   RF
# 4 0.7040816327  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.531164308  ANN
# 2 1.928571429   OL
# 3 2.108778486   RF
# 4 1.846408470  SVM

#Networks
x<-c("subgraph_centrality","eigenvector_centrality")
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#         accuracy test
# 1 0.6632653061  ANN
# 2 0.6122448980   OL
# 3 0.6734693878   RF
# 4 0.7448979592  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 1.739372834  ANN
# 2 3.000000000   OL
# 3 1.068825543   RF
# 4 1.417830446  SVM

#ACOM and semco-1
x<-c("acom",'averageSemco')
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#       accuracy test
# 1 0.6632653061  ANN
# 2 0.6326530612   OL
# 3 0.5918367347   RF
# 4 0.6224489796  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.532054138  ANN
# 2 2.908163265   OL
# 3 2.123578318   RF
# 4 2.171020374  SVM

#Subset 1
x<-c('acom',"subgraph_centrality","eigenvector_centrality","X.PosWords","X.NegWords" )
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees,MSE = F)
#       accuracy test
# 1 0.6632653061  ANN
# 2 0.7448979592   OL
# 3 0.7653061224   RF
# 4 0.8163265306  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees,MSE = T)
#       accuracy test
# 1 2.5320598530  ANN
# 2 1.7244897959   OL
# 3 0.7499281213   RF
# 4 0.7966692418  SVM

#Subset 2
x<-c("subgraph_centrality","eigenvector_centrality","X.PosWords","X.NegWords" ,'averageSemco')
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#       accuracy test
# 1 0.6632653061  ANN
# 2 0.7244897959   OL
# 3 0.8163265306   RF
# 4 0.8265306122  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.5320667331  ANN
# 2 2.0204081633   OL
# 3 0.6076663968   RF
# 4 0.7284267158  SVM

#Subset 3
x<-c('acom',"subgraph_centrality","eigenvector_centrality",'averageSemco')
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#       accuracy test
# 1 0.6224489796  ANN
# 2 0.6224489796   OL
# 3 0.7959183673   RF
# 4 0.7653061224  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.5320168829  ANN
# 2 2.7653061224   OL
# 3 0.8181573441   RF
# 4 1.1078665099  SVM

#Subset 4
x<-c('acom',"subgraph_centrality","eigenvector_centrality")
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
# accuracy test
# 1 0.6632653061  ANN
# 2 0.6224489796   OL
# 3 0.7346938776   RF
# 4 0.7448979592  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.5320522632  ANN
# 2 2.7653061224   OL
# 3 0.9168390266   RF
# 4 1.2409427410  SVM

#Subset 5
x<-c('averageSemco',"subgraph_centrality","eigenvector_centrality")
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = F)
#     accuracy test
# 1 0.6632653061  ANN
# 2 0.6326530612   OL
# 3 0.7551020408   RF
# 4 0.7551020408  SVM
testVariables(x,y,testDat,trainDat,hiddenLayers,thresholdValue,learningrate,activationFunction,trees, MSE = T)
#       accuracy test
# 1 2.5320064027  ANN
# 2 2.8163265306   OL
# 3 0.8557295612   RF
# 4 1.3050392179  SVM