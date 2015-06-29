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
# Add labels
cntries=dimnames(Y)[[1]]
dvs = dimnames(Y)[[3]]
ivs = dimnames(X)[[3]]

# Add labels to posterior object
dimnames(BPS[[1]])[[1]] = cntries
dimnames(BPS[[1]])[[2]] = cntries
dimnames(BPS[[2]])[[1]] = cntries
dimnames(BPS[[2]])[[2]] = cntries

dimnames(BPS[[3]])[[1]] = dvs
dimnames(BPS[[3]])[[2]] = ivs
############################

############################
# Calculate posterior means for B
getPost = function(beta, burn=-(1:600)){
	b1 = apply(beta[[1]][,,burn], c(1:2), mean)
	b2 = apply(beta[[2]][,,burn], c(1:2), mean)
	b3 = apply(beta[[3]][,,burn], c(1:2), mean)
	return( list(b1, b2, b3) )
}

# Calculate predicted values
pred = tprod(X, getPost(BPS))
for(var in 1:dim(pred)[3] ){
	for(yr in 1:dim(pred)[4] ){
		diag(pred[,,var,yr]) = 0 } }

# Aggregate performance
rsq = function(YP, Y){ 1-apply((YP-Y)^2,3,mean)/apply(Y^2,3,mean) }
rmse = function(YP, Y){ apply((YP-Y)^2, 3, function(x){ sqrt(mean(x)) }) }
rmsePerf = rmse(pred, Y )
rsqPerf = rsq(pred, Y)
aggPerf = cbind(rmsePerf, rsqPerf)
colnames(aggPerf) = c('RMSE', 'R$^{2}$')
rownames(aggPerf) = c('Log(Exports)', 'Std(Matl. Conf.)')
aggPerf = round(aggPerf, 2)

print.xtable(xtable(aggPerf), 
	file=paste0(outPath, 'rsq.tex'),
	include.rownames=TRUE, 
	sanitize.text.function=identity
	)

# Indiv perf, calc: RMSE by i-j for var
indivPerf = apply((pred-Y)^2, 1:3, function(x){ sqrt( mean( x ) ) })
############################

############################
# Tile maps
perf = indivPerf
ggData = NULL
for(var in 1:dim(perf)[3]){
	pull = perf[,,var]
	meltPull = melt( pull )
	meltPull = cbind(var = dimnames(perf)[[3]][var], meltPull)
	ggData = rbind(ggData, meltPull)
}
# ggData$var = makeLabel(ggData$var)
ggData$var = factor(ggData$var, levels=unique(ggData$var))

tilePerf = function(dv){
	ggIPerf=ggplot(ggData[ggData$var==dv,], aes(x=Var1, y=Var2, fill=value)) 
	ggIPerf=ggIPerf + xlab('') + ylab('')
	ggIPerf=ggIPerf + geom_tile(colour='lightgrey')
	ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
	ggIPerf=ggIPerf + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
	ggIPerf=ggIPerf + theme(
		axis.ticks=element_blank(), 
		legend.position='top', legend.key.width=unit(2,'cm'),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1, size=4),
		axis.text.y = element_text(size=4)
		)
	return(ggIPerf)
}

ePerf=tilePerf('exports')
fname=paste0(outPath, 'expiperf.pdf')	
ggsave(filename=fname, plot=ePerf, width=10, height=10)

mPerf=tilePerf('matlConf')
fname=paste0(outPath, 'mconfiperf.pdf')	
ggsave(filename=fname, plot=mPerf, width=10, height=10)
############################