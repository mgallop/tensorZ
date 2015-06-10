####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

####
# source data building scripts
source(paste0(rPath, 'Data/sampFrame.R'))
source(paste0(rPath, 'Data/runGets.R'))
####

####
# Load processed data
setwd(inPath)

# Trade data 
load('dyadExpImp.rda')

# Monadic variables (polity from CRISP, gdp & pop from worldbank)
load('crisp.rda'); load('wbData.rda')

# Dyadic variables (alliance from COW, geo from cshapes, quad from ICEWS)
load('ally.rda'); load('distData.rda'); load('quad.rda')
####

####
# Merge
# dirData = 
####