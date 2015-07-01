if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# Load rmse by dyad for MLTR and normal dyadic model
load(paste0(outPath, 'rmseDyad.rda')) # perfDyad
load(paste0(outPath, 'rmseMLTR.rda')) # perfMltr

# Change outpath to presentation
outPath = '~/Research/WardProjects/tensorZ/Presentation/Graphics/'
############################

############################
# Combine dataframes
perf = perfMltr
names(perf)[3] = c('rmseMltr')
# Create id to merge in normal dyad data
perf$id = paste0(perf$source, perf$target, perf$var)
perfDyad$id = paste0(perfDyad$source, perfDyad$target, perfDyad$var)
# Bring in normal dyad data
perf$rmseDyad = perfDyad$rmse[match(perf$id, perfDyad$id)]
# Drop unnecessary datasets
rm(list=c('perfDyad', 'perfMltr'))
############################

############################
# Compare performance by dyad for MLTR and dyad approaches
perf$mltrW = perf$rmseMltr < perf$rmseDyad

# Split into list by var for some easy comparisons
perfL = lapply(unique(perf$var), function(v){ perf[perf$var==v,] })
names(perfL) = unique(perf$var)

# MLTR is superior 
# ...for exports ~57% of time
# ...for mconf ~80% of time
lapply(perfL, function(d){ mean(d$mltrW) })

# By how much
# ...for exports ~.30
# ...for mconf ~.011
lapply(perfL, function(d){
	mean( d$rmseDyad[d$mltrW==1] - d$rmseMltr[d$mltrW==1] ) })

# Distributions of difference
lapply(perfL, function(d){
	diff = d$rmseDyad[d$mltrW==1] - d$rmseMltr[d$mltrW==1]
	diffMu = mean( diff ) 
	plot(density(diff[diff<=quantile(diff, probs=seq(0,1,.01))['99%']]))
	})
############################

############################
# are there diffs in mltr perf by OECD and not?
perfTile = melt(perf, id=c('source','target','var','id','mltrW'))

tilePerf = function(dv, oecd=TRUE){

	# Isolate to OECD countries?
	oecdCntries = c('AUSTRALIA', 'AUSTRIA', 'BELGIUM', 'CANADA', 'CHILE', 'CZECH REPUBLIC', 'DENMARK', 'ESTONIA', 
		'FINLAND', 'FRANCE', 'GERMANY', 'GREECE', 'HUNGARY', 'ICELAND', 'IRELAND', 'ISRAEL', 'ITALY', 'JAPAN', 
		'KOREA, REPUBLIC OF', 'LUXEMBOURG', 'MEXICO', 'NETHERLANDS', 'NEW ZEALAND', 'NORWAY', 'POLAND', 'PORTUGAL', 'SLOVAKIA', 
		'SLOVENIA', 'SPAIN', 'SWEDEN', 'SWITZERLAND', 'TURKEY', 'UNITED KINGDOM', 'UNITED STATES')
	if(oecd){
		perfTile = perfTile[perfTile$source %in% oecdCntries,]
		perfTile = perfTile[perfTile$target %in% oecdCntries,]

		perfTile$variable = char( perfTile$variable )
		perfTile$variable[perfTile$variable=='rmseMltr'] = paste0('DV=', dv, ': Approach=MLTR: Sample=OECD')
		perfTile$variable[perfTile$variable=='rmseDyad'] = paste0('DV=', dv, ': Approach=DD: Sample=OECD')
	}

	if(!oecd){
		perfTile = perfTile[!perfTile$source %in% oecdCntries,]
		perfTile = perfTile[!perfTile$target %in% oecdCntries,]

		perfTile$variable = char( perfTile$variable )
		perfTile$variable[perfTile$variable=='rmseMltr'] = paste0('DV=', dv, ': Approach=MLTR: Sample=Not OECD')
		perfTile$variable[perfTile$variable=='rmseDyad'] = paste0('DV=', dv, ': Approach=DD: Sample=Not OECD')
	}

	# Replace country name with ISO abbreviation
	cntry = c(char(perfTile$source), char(perfTile$target)) %>% unique() %>% data.frame()
	names(cntry) = 'cname'; cntry$iso = countrycode(cntry$cname, 'country.name', 'iso3c')
	perfTile$source = cntry$iso[match(perfTile$source, cntry$cname)]
	perfTile$target = cntry$iso[match(perfTile$target, cntry$cname)]

	# Subset to relev var
	perfTile = perfTile[perfTile$var==dv,]
	# Add cat indicator for RMSE
	perfTile$valueCat = perfTile$value %>% cut(., 
		breaks=quantile(., probs=seq(0,1,100/9/100)), 
		include.lowest=TRUE, ordered_result=TRUE, dig.lab=1)
	# Plot	
	ggIPerf=ggplot(perfTile, aes(x=source, y=target, fill=valueCat)) 
	ggIPerf=ggIPerf + xlab('') + ylab('')
	ggIPerf=ggIPerf + geom_tile(colour='darkgrey')
	# ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
	ggIPerf=ggIPerf + scale_fill_brewer(name='', palette='Reds')	
	ggIPerf=ggIPerf + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
	ggIPerf=ggIPerf + facet_wrap(~variable)
	if(oecd){ggIPerf=ggIPerf + theme(
			axis.ticks=element_blank(), 
			legend.position='right', legend.key.width=unit(.1,'cm'), legend.key.height=unit(1,'cm'),
			panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
			axis.text.x = element_text(angle=45, hjust=1, size=6),
			axis.text.y = element_text(size=6)
			)}
	if(!oecd){ggIPerf=ggIPerf + theme(
			axis.ticks=element_blank(), 
			legend.position='right', legend.key.width=unit(.1,'cm'), legend.key.height=unit(1,'cm'),
			panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
			axis.text.x = element_text(angle=45, hjust=1, size=3),
			axis.text.y = element_text(size=3)
			)}	
	return(ggIPerf)
}

loadPkg('gridExtra')
oecdExpPerf=tilePerf('Log(Exports)', oecd=TRUE)
otherExpPerf=tilePerf('Log(Exports)', oecd=FALSE)
fname=paste0(outPath, 'oecdexpiperf.pdf')	
pdf(file=fname, width=14, height=10)
grid.arrange(oecdExpPerf, otherExpPerf, nrow=2, ncol=1)
dev.off()

oecdConfPerf=tilePerf('Std(Matl. Conf.)', oecd=TRUE)
otherConfPerf=tilePerf('Std(Matl. Conf.)', oecd=FALSE)
fname=paste0(outPath, 'oecdconfiperf.pdf')	
pdf(file=fname, width=14, height=10)
grid.arrange(oecdConfPerf, otherConfPerf, nrow=2, ncol=1)
dev.off()
############################