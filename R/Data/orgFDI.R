####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

####
# Load FDI files
fdiPath = paste0(inPath, 'fdi/')
setwd(fdiPath)

expFiles = paste0(letters[1:11], '__ Goods, Value of Exports, FOB, US Doll.csv')
impFiles = paste0(letters[1:11], '__ Goods, Value of Imports, CIF, US Doll.csv')

extractCntries = function(file){
	cntries=unique( strsplit(readLines(file, n=1), '",\"')[[1]] )
	return( cntries[c(-1, -length(cntries))] )
}
cntries = unlist( lapply(expFiles, function(f){ extractCntries(f) }) )
####

####
# Create directed dyadic datasets

# Function to process IMF DOT csv Sheets
processIMF = function(file){
	slice = read.csv(file, header=FALSE)
	names(slice) = paste(
		char(unlist(slice[1,])), 
		char(unlist(slice[2,])), sep='_')
	slice = slice[c(-1,-2),]
	names(slice)[1] = 'j'
	slice = melt(slice, id='j')
	slice$i = unlist(lapply(strsplit(char(slice$variable), '_'), function(x) x[1]))
	slice$t = unlist(lapply(strsplit(char(slice$variable), '_'), function(x) x[2]))
	slice = slice[,c('i', 'j', 't', 'value')]
	slice$value = num(slice$value)

	# Drop i - i
	slice = slice[which(slice$i != slice$j), ]
	return(slice) 	
}

expData = do.call('rbind', lapply(expFiles, function(f){ processIMF(f) }) )
impData = do.call('rbind', lapply(impFiles, function(f){ processIMF(f) }) )
####

####
# Save data
setwd(inPath)
save(expData, impData, file='dyadExpImp.rda')
####