
# Set up For RiRi
source('./prototype/script_master.R')
runPrototype('.', resample = F, tokenize = F, sentiment= F, getTopWords = F,
             judgements = T,BOW = F,createCo = F,createDSM = F,semContext = F, semACOM = F, network = F) 
