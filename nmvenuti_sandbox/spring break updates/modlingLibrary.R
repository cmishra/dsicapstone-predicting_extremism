#Modeling library
#Will use artifical neural networdks, ordered logistic, random forest, and support vector machines

#Load test data for analysis
setwd("~/GitHub/dsicapstone-predicting_extremism/nmvenuti_sandbox")
filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/"

#Test data and parameters
dat=read.csv('test_data_models.csv')
y<-'y'
# x<-c('x1','x2','x3','x4','x5','x6','x7','x8','x9')
x<-c('x1','x2')
hiddenLayers=10
thresholdValue=0.01
activationFunction='logistic' #can also use tanh


###############
#####Setup#####
###############

#Create formula
dataFormula<-paste(c(y," ~ ",paste(x, collapse ="+")),collapse = "")



##################################
#####Artifical Neural Network#####
##################################
library(neuralnet)
netModel <- neuralnet(as.formula(dataFormula), dat, hidden=hiddenLayers,act.fct=activationFunction)
netSummary<-summary(netModel)
netResults<-compute(netModel,dat[,x])



##########################
#####Ordered logistic#####
##########################

#Load library
library(MASS)

#Create data for ol
olDat<-dat

#Ensure response is factor
olDat[,y]<-as.factor(olDat[,y])

#Create model
olModel<-polr(as.formula(dataFormula),data=olDat)

#Extract summary data
olSummary<-summary(olModel)

#Create prediction
olResults<-predict(olModel,newData=olDat)

#Save model and summary information
save(olModel,olSummary,file="olOutput.RData")

####Should we be using factors or continuos here (kinda feelin contiuous)

#######################
#####Random Forest#####
#######################
library(randomForest)
rfModel<-randomForest(as.formula(dataFormula),data = olDat)
rfSummary<-summary(rfModel)
rfResults<-predict(rfModel,newdata = olDat)


#################################
#####Support Vector Machines#####
#################################
library(e1071)
svmModel=svm(olDat[,x],olDat[,y], kernel ="radial", degree = 3, probability = TRUE)
svmSummary<-summary(svmModel)
svmResults<-predict(svmModel,newData=olDat)

