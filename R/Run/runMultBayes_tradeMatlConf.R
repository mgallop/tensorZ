if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') 
}

#### 
# Load ALM function
source(paste0(rFuncs, "functions_als.r"))
source(paste0(rFuncs, "functions_bayes.r"))
#### 

#### 
# data
load(paste0(inPath, "YX.rda"))
####

#### MCMC 
# mcmc function parameters
NS = 7500 ; NB = 500 ; sdens = 100 ; plot = TRUE
seed = 6886 ; rstart = FALSE
# output name
fname="tensorTradeConf" 
setwd(outPath)
# run
source(paste0(rFuncs, "mcmc.r") )