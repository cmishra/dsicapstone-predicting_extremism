
# Set up For RiRi
source('./prototype/script_master.R')
runPrototype('.', resample = F,
             tokenize = F, sentiment= F, getTopWords = T, judgements = F, BOW = F, semanticContext = F) 
