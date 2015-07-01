if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# data
load(paste0(inPath, "YX.rda"))

# results
load(paste0(outPath, 'tensorTradeConf'))

# Change outpath to presentation
outPath = '~/Research/WardProjects/tensorZ/Presentation/Graphics/'
############################

############################
# Load mcmc mult results
# Set burn in
burn = 1200
BPS = lapply(BPS, function(x){ x[,,-(1:burn)] })

# Add labels
cntries=dimnames(Y)[[1]]
dvs = dimnames(Y)[[3]]
ivs = dimnames(X)[[3]]
pds = dimnames(X)[[4]]

# Add labels to posterior object
dimnames(BPS[[1]])[[1]] = cntries
dimnames(BPS[[1]])[[2]] = cntries
dimnames(BPS[[2]])[[1]] = cntries
dimnames(BPS[[2]])[[2]] = cntries

dimnames(BPS[[3]])[[1]] = dvs
dimnames(BPS[[3]])[[2]] = ivs
############################

############################
# Marg effect of PTA vars
getMeanPost = function(beta){
	b1 = apply(beta[[1]], c(1:2), mean)
	b2 = apply(beta[[2]], c(1:2), mean)
	b3 = apply(beta[[3]], c(1:2), mean)
	return( list(b1, b2, b3) )
}

# Get scenario values
varDesc = matrix(NA, nrow=length(ivs), ncol=1, dimnames=list(ivs, c('directed')))
varDesc[,'directed'] = c(T, T, T, T, T, T, F, T, T, F, F, F, F)
scenVals = getScenVals(data=X, vars=ivs, time=pds, infoMat=varDesc)

# Organize into scenario array
# Parameters to determine size of scenario
scenX = getScenArray(varToVary='pta_ijk', valsForScen=scenVals, 
	dim12names=cntries, dim3names=ivs)
dimnames(scenX)[[4]] = scenVals$'pta_ijk'[[2]] %>% unique()

scenX = scenX[,,,1:4]
BPS = lapply(BPS, function(b){ b[,,1:4] })

# Parallelize calculation of marginal effects
parrPacks = c('foreach', 'doParallel'); loadPkg(parrPacks)
cl = makeCluster(8); registerDoParallel(cl)
scenPreds <- foreach( ii = 1:dim(BPS[[1]])[3] ) %dopar% {
	beta = lapply(BPS, function(b){ b[,,ii] })
	b1Mu = mean(as.vector(beta[[1]]))
	beta[[1]] = matrix(b1Mu, nrow=nrow(beta[[1]]), ncol=ncol(beta[[1]]))
	beta[[2]] = matrix(b1Mu, nrow=nrow(beta[[2]]), ncol=ncol(beta[[2]]))
	scenP = tprod(scenX, beta)
	for(v in 1:dim(scenP)[3]){ for(z in 1:dim(scenP)[4]){ diag(scenP[,,v,z]) = NA } }
	dimnames(scenP)[c(1:2,4)] = dimnames(scenX)[c(1:2,4)]
	dimnames(scenP)[[3]] = dvs
	return(scenP)
}; stopCluster(cl)

# Reorganize into usable dataframe for agg and plotting
scenPredMelt = melt(scenPreds) %>% 
	.[,c('Var3','Var4','value','L1')] %>%
	unique(.) %>% 
	na.omit(.)

# Get mean and quantiles for iterations to plot

############################

############################
# Plot predictions with error

############################