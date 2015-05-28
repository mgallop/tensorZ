####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
cntries=char(unique(cntries$imf))
dates=unique(dates$t)
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
	ids = strsplit(char(slice$variable), '_')
	slice$i = unlist(lapply(ids, function(x) x[1]))
	slice$t = unlist(lapply(ids, function(x) x[2]))
	slice = slice[,c('i', 'j', 't', 'value')]
	slice$value = num(slice$value)

	# Drop i - i
	slice = slice[which(slice$i != slice$j), ]

	# Drop items not in cntries vector
	slice = slice[which(slice$j %in% cntries), ]

	# Org dates
	dateText = strsplit(slice$t, ' ')
	slice$month = unlist(lapply(dateText,function(x) x[1]))
	slice$year = unlist(lapply(dateText,function(x) x[2]))
	slice = slice[,setdiff(names(slice), 't')]
	slice$date=as.Date(paste0(1, slice$month, slice$year), '%d%b%Y')

	# Subset by date
	slice = slice[which(slice$date %in% dates),]

	# Return object
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