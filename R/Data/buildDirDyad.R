####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

# ####
# # source data building scripts
# source(paste0(rPath, 'Data/sampFrame.R'))
# source(paste0(rPath, 'Data/runGets.R'))
# ####

####
# Load and combine processed data
setwd(inPath)

# Trade data 
load('dyadExp.rda') # loads expData object
names(expData)[4] = 'exports'

# Monadic variables (polity from CRISP, gdp & pop from worldbank)
load('crisp.rda')
expData = mutate(expData, i_id= paste(i, t, sep = '_'))
expData = mutate(expData, j_id= paste(j, t, sep = '_'))

tmp = full_join(x=expData, y=crisp[,c('id','polity')],
	by.x='i_id', by.y='id', suffixes='i_'
	)

expData = merge(
	expData, 
	crisp[,c('id', 'polity')], 
	by.x='i_id', by.y='id', suffixes='i_')

load('wbData.rda')

# Dyadic variables (alliance from COW, geo from cshapes, quad from ICEWS)
load('ally.rda'); load('distData.rda'); load('quad.rda')
####

