
# Set up For RiRi
source('./prototype/script_master.R')
runPrototype('.', resample = F, tokenize = T, sentiment= F, getTopWords = F,
             judgements = F,BOW = F,createCo = F,createDSM = F,semContext = F, semACOM = F, network = F) 