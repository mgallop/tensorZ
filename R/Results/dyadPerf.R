if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# data
load(paste0(inPath, "YX.rda"))

# Change outpath to presentation
outPath = '~/Research/WardProjects/tensorZ/Presentation/Graphics/'
############################

############################
# Add labels
cntries=dimnames(Y)[[1]]
dvs = dimnames(Y)[[3]]
ivs = dimnames(X)[[3]]
############################

############################
# Recreate dyadic frames for data
dyadData = melt(Y[,,'exports',])
names(dyadData) = c('source','target','date','exports')
dyadData$matlConf = melt(Y[,,'matlConf',])[,4]

# Add in independent variables
for(iv in ivs){
	dyadData$tmp = melt(X[,,iv,])[,4]
	names(dyadData)[ncol(dyadData)]=iv }

# Drop i - i observations
dyadData = dyadData[which(dyadData$source != dyadData$target),]
############################

############################
# Run dyadic model
forms = lapply(dvs, function(dv){
	formula( paste0( dv, ' ~ ', paste(ivs, collapse=' + ') ) ) })

mods = lapply(forms, function(form) mod = lm(form, data=dyadData) )

# Model results
loadPkg('apsrtable')
fname = file(paste0(outPath, 'dyadcoef.tex'))
writeLines( apsrtable(
	mods[[1]], mods[[2]], 
	model.names=c('Log(Exports)', 'Std(Matl. Conf.)')
	) , fname)
close(fname)

# Aggregate performance
rmses = lapply(mods, function(mod) sqrt( mean( resid(mod)^2 ) ) ) %>% unlist()
rsq = lapply(mods, function(mod) summary(mod)$'r.squared' ) %>% unlist()

aggPerf = cbind(rmses, rsq)
colnames(aggPerf) = c('RMSE', 'R$^{2}$')
rownames(aggPerf) = c('Log(Exports)', 'Std(Matl. Conf.)')
aggPerf = round(aggPerf, 2)

print.xtable(xtable(aggPerf), 
	file=paste0(outPath, 'dyadrsq.tex'),
	include.rownames=FALSE, 
	sanitize.text.function=identity
	)
############################

############################
# Org resids by data
dats = lapply(mods, function(mod){
	data.frame( dyadData[,c('source','target','date')], resid=resid(mod) ) })
# Calc rmse by time series
dats = lapply(dats, function(dat){ 
	dat$id = paste(dat$source, dat$target, sep='_'); return(dat) })
rmseTime = lapply(dats, function(dat){
	rmse = tapply(dat$resid, dat$id, FUN=function(x){ sqrt(mean(x^2)) } )
	source = unlist(lapply(strsplit(names(rmse),'_'),function(x) x[1]))
	target = unlist(lapply(strsplit(names(rmse),'_'),function(x) x[2]))
	return( data.frame(source, target, rmse, row.names=NULL) )
	})

# Combine for gg
ggData = do.call('rbind', rmseTime)
cnt = ( length(cntries)*length(cntries) ) - length(cntries)
ggData$var = c( 
	rep('Log(Exports)', cnt), 
	rep('Std(Matl. Conf.)', cnt)
	)

# Subset
toPlot = c('Log(Exports)', 'Std(Matl. Conf.)')
ggData = ggData[which(ggData$var %in% toPlot ),]
ggData$var = factor(ggData$var, levels=toPlot)

tilePerf = function(dv){
	ggIPerf=ggplot(ggData[ggData$var==dv,], aes(x=source, y=target, fill=rmse)) 
	ggIPerf=ggIPerf + xlab('') + ylab('')
	ggIPerf=ggIPerf + geom_tile(colour='darkgrey')
	ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
	ggIPerf=ggIPerf + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
	ggIPerf=ggIPerf + facet_wrap(~var)
	ggIPerf=ggIPerf + theme(
		axis.ticks=element_blank(), 
		legend.position='top', legend.key.width=unit(2,'cm'),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1, size=4),
		axis.text.y = element_text(size=4)
		)
	return(ggIPerf)
}

ePerf=tilePerf('Log(Exports)')
fname=paste0(outPath, 'dyadexpiperf.pdf')	
ggsave(filename=fname, plot=ePerf, width=10, height=10)

mPerf=tilePerf('Std(Matl. Conf.)')
fname=paste0(outPath, 'dyadmconfiperf.pdf')	
ggsave(filename=fname, plot=mPerf, width=10, height=10)
############################