
cmdArgs = commandArgs(trailingOnly=TRUE)
if (length(cmdArgs) >= 1){
  i = as.numeric(cmdArgs[1])
}
cat("i = ", i, "\n")
# Set up For RiRi
#source('./prototype/parSemCo.R')
# i = 1
#parSemCo('.',i)

source("prototype/scripts/script_masterparallel.R")
runParallelPrototype(".", i, resample = T,
                     tokenize = T,
                     sentiment= T,
                     getTopWords = T,
                     judgements = T,
                     BOW = T,
                     createCo = T,
                     createDSM = T,
                     semContext = T,
                     semACOM = T,
                     network = T)