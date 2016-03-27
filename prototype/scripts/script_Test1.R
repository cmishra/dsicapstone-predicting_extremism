
# Set up For LDA LOCAL
source('./prototype/main_LDA.R')
runPrototype('.', resample = F, tokenize = F, sentiment= F, getTopWords = F,
             judgements = F,BOW = F,createCo = F,createDSM = F,semContext = F, semACOM = F, 
             network = F, LDA = T) 
