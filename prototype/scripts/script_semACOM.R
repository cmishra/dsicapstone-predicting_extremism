
# Set up For RiRi
source('./prototype/script_master.R')
runPrototype('.', resample = F, tokenize = F, sentiment= F, getTopWords = F,
             judgements = F,BOW = F,createCo = F,createDSM = F,semContext = F, semACOM = T, network = F) 