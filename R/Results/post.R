if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# data
load(paste0(inPath, "YX.rda"))

# results
load(paste0(outPath, 'tensorTradeConf'))
############################

############################
# Load mcmc mult results
# Set burn in
burn = 300

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

# mapping = rbind(
# 	c('exports', 'Exports', 'Exports'),
# 	c('matlConf', 'Material Conflict', 'Matl.~Conf.'),
# 	c('verbConf', 'Verbal Conflict', 'Verb.~Conf.'),
# 	c('matlConf', 'Material Conflict', 'Matl.~Conf.') )

# makeLabel = function(x, long=TRUE){
# 	if(long){ col = 2 } else { col = 3 }
# 	x = as.character(x)
# 	ids = unique(x)
# 	for(id in ids){
# 		x = gsub(strsplit(id, '[', fixed=TRUE)[[1]][1], 
# 				mapping[match(strsplit(id, '[', fixed=TRUE)[[1]][1], mapping[,1]),col], 
# 				x, fixed=TRUE) }
# 	return( factor( x ) )
# }
############################

############################
outPath='~/Research/WardProjects/tensor/Text/Graphics/'
############################

############################
# Trace plots by row
dvs=dim(BPS[[3]])[1]
traceData = NULL
for(ii in 1:dvs){
	tbeta = data.frame( t(BPS[[3]][ii,,]) )
	tbeta = cbind(dv=dimnames(BPS[[3]])[[1]][ii], tbeta, Iteration=1:nrow(tbeta))
	traceData = rbind(traceData, tbeta)
}
traceData=traceData[traceData$Iteration %% 5 == 0,] # thin
traceData = melt(traceData, id=c('dv', 'Iteration'))

# traceData$dv = makeLabel(traceData$dv, long=TRUE)

ggTrace=ggplot(traceData, aes(x=Iteration, y=value, color=variable))
ggTrace=ggTrace + geom_line() + facet_wrap(~dv)
ggTrace=ggTrace + xlab('') + ylab('')
# ggTrace=ggTrace + scale_color_brewer(name='', type='qual')
ggTrace=ggTrace + theme(
	axis.ticks=element_blank(),
	legend.position='top',
	panel.background=element_blank()
	)
fname=paste0(outPath, 'trace.pdf')
print( ggTrace )
ggsave(filename=fname, plot=ggTrace, width=6, height=4)
############################

############################
# Summary of beta[3] Posterior distributions
summStats = function(x){
	mu=mean(x)
	qts=quantile(x, probs=c(0.025,0.975,0.05,0.95))
	return( c(mu, qts) )
}

# Generate data for posterior distributions
tmp=BPS[[3]]
tmp=tmp[,,burn:dim(tmp)[3]] # Burn 1k
# Assemble data for coef
coefData=NULL
for(ii in 1:dim(tmp)[1]){
	mod=t(tmp[ii,,])
	modSumm=apply(mod, 2, summStats)
	rownames(modSumm)=c('mu', paste0(c('lo','up'),95), paste0(c('lo','up'),90))
	coefSlice = data.frame(iv=rownames(t(modSumm)), t(modSumm), row.names=NULL)
	coefSlice = cbind(dv = dimnames(tmp)[[1]][ii], coefSlice)
	coefData = rbind(coefData, coefSlice) }

# Add in variable for colors
coefData$sig = NULL
coefData$sig[coefData$lo90 > 0 & coefData$lo95 < 0] = "Positive at 90"
coefData$sig[coefData$lo95 > 0] = "Positive"
coefData$sig[coefData$up90 < 0 & coefData$up95 > 0] = "Negative at 90"
coefData$sig[coefData$up95 < 0] = "Negative"
coefData$sig[coefData$lo90 < 0 & coefData$up90 > 0] = "Insig"
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
                "Negative"= rgb(222, 45, 38, maxColorValue=255),
                "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
                "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
                "Insig" = rgb(150, 150, 150, maxColorValue=255))

# Map variables to labels (very lazy way)
# coefData$dv = makeLabel(coefData$dv, long=TRUE) 
coefData$dv = factor(coefData$dv, levels=unique(coefData$dv))
# coefData$iv = makeLabel(coefData$iv, long=FALSE) 
coefData$iv = factor(coefData$iv, levels=unique(coefData$iv))

# Plot
# coefData$iv = factor(coefData$iv, levels=dimnames(BPS[[3]])[[2]])
ggCoef=ggplot(coefData, aes(x=iv, y=mu, color=sig))
ggCoef=ggCoef + geom_point() + facet_wrap(~dv, nrow=2, scales = 'free_y')
ggCoef = ggCoef + geom_linerange(aes(ymin=lo95, ymax=up95), alpha = .3, size = 0.3)
ggCoef = ggCoef + geom_linerange(aes(ymin=lo90, ymax=up90),alpha = 1, size = 1)
ggCoef = ggCoef + geom_errorbar(aes(ymin=lo95,ymax=up95),linetype = 1,width = 0.1)	
ggCoef=ggCoef + geom_hline(yintercept=0, color='red', linetype='dashed')
ggCoef=ggCoef + geom_vline(xintercept=c(2.5, 4.5), color='grey', linetype='dashed')
ggCoef=ggCoef + ylab('') + scale_colour_manual(values = coefp_colors)
# ggCoef=ggCoef + scale_x_discrete("", labels = parse(text = levels(coefData$iv)))
ggCoef=ggCoef + theme(
	axis.ticks=element_blank(),
	axis.text.x = element_text(angle=45, hjust=1),
	panel.background = element_blank(),
	legend.position='none',
	panel.grid.major=element_blank(),
	panel.grid.minor=element_blank()
	)
fname=paste0(outPath, 'coef.pdf')		
ggCoef
ggsave(filename=fname, plot=ggCoef, width=6, height=6)
############################

############################
# Network plots
loadPkg('shape')
source(paste0(rFuncs, "rda.r") )
genCntryMap=FALSE; source(paste0(rFuncs, "genColors.r") )
proc_rr=function(Y,X){
	k=dim(X)[2]
	A=t(Y)%*%(X%*%t(X))%*%Y
	eA=eigen(A,symmetric=T)
	Ahalf=eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
	t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }

alpha = .0000001
set.seed(6886) 

beta = BPS
B = beta[[1]]
LB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=alpha )
UB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=1-alpha )
BSIG = 1*( LB*UB >0 )
BPOS = 1*( LB>0 ) 

loadPkg('igraph')
# Laziness
tcols = rep('white', length(cntries))
tcols[which(
	cntries %in% 
		c('RUS', 'NOR', 'FIN', 'SWE', 'CAN', 'DNK')
		)]='black'

# fname=paste0(outPath, 'net.pdf')	
# pdf(file=fname, width=12, height=5)
par(mfrow=c(1,2), mar=c(1,1,1,1), mgp=c(1.5,.5,0))		
B = beta[[1]]
LB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=alpha )
UB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=1-alpha )
BSIG = 1*( LB*UB >0 )
BPOS = 1*( LB>0 ) 
rownames(BPOS)=colnames(BPOS)=NULL
g = graph.adjacency(BPOS, mode='directed', diag=FALSE)
set.seed(6886)
plot.igraph(g, 
	layout=layout.auto,
	vertex.label.color=tcols, 
	vertex.size=log(igraph::degree(g)+1)+1.5,
	# vertex.label.cex = NA,
	edge.arrow.size=0.4,
	asp=FALSE
	)

B = beta[[2]]
LB = apply(B,c(1,2),quantile,prob=alpha)
UB = apply(B,c(1,2),quantile,prob=1-alpha)
BSIG = 1*( LB*UB >0 )
BPOS = 1*(LB>0)
rownames(BPOS)=colnames(BPOS)=NULL
g = graph.adjacency(BPOS, mode='directed', diag=FALSE)
set.seed(6886)
plot.igraph(g, 
	layout=layout.auto,
	vertex.label.color=tcols, 
	vertex.size=log(igraph::degree(g)+1)+1.5,
	# vertex.label.cex = NA,
	edge.arrow.size=0.4,
	asp=FALSE
	)
# dev.off()

# Other checks
b = BPS[[1]]
bMu = apply( b[,,burn:dim(b)[3]], c(1,2), mean )
bDiag = diag(bMu)
bOffDiag = bMu
diag(bOffDiag) = NA
mean(abs(bDiag))/mean(abs(as.vector(bOffDiag)), na.rm=TRUE)

b = BPS[[2]]
bMu = apply( b[,,burn:dim(b)[3]], c(1,2), mean )
bDiag = diag(bMu)
bOffDiag = bMu
diag(bOffDiag) = NA
mean(abs(bDiag))/mean(abs(as.vector(bOffDiag)), na.rm=TRUE)
############################