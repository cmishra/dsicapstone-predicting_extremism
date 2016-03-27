# script_runLDA

source('./prototype/script_masterparallel.R')
# Loading all Libaries/Packages by running Master Script with all Falses
runParallelPrototype('.', resample = F, tokenize = F, sentiment= F, getTopWords = F,
             judgements = F,BOW = F,createCo = F,createDSM = F,semContext = F, semACOM = F, 
             network = F) 
source('./prototype/main_LDA.R')
runLDA('.')
