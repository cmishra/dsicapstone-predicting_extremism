
# Set up For RiRi
source('./prototype/script_masterparallel.R')
runParallelPrototype('.', 1, resample = T, tokenize = T, sentiment= T, getTopWords = T,
             judgements = T,BOW = T,createCo = T,createDSM = T,semContext = T, semACOM = T, network = T) 