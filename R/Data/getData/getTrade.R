####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
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
	slice$j = char(slice$j)
	slice$value = num(slice$value)

	# Drop i - i
	slice = slice[which(slice$i != slice$j), ]

	# Drop items not in cntries vector
	slice = slice[which(slice$i %in% cntries$imf), ]
	slice = slice[which(slice$j %in% cntries$imf), ]

	# Convert to cname
	slice$i = cntries$cname[match(slice$i, cntries$imf)] %>% char()
	slice$j = cntries$cname[match(slice$j, cntries$imf)] %>% char()

	# Subset slice by dates in sampFrame
	slice = slice[which(slice$t %in% dates$tdate), ]

	# Add in cleaned date variable
	slice$t = dates$date[match(slice$t, dates$tdate)]

	# Add in unique id
	slice = mutate(slice, id = paste(i, j, t, sep='_'))
	slice = mutate(slice, i_id= paste(i, t, sep = '_'))
	slice = mutate(slice, j_id= paste(j, t, sep = '_'))

	# Return object
	return(slice)
}

expData = do.call('rbind', lapply(expFiles, function(f){ processIMF(f) }) )
# impData = do.call('rbind', lapply(impFiles, function(f){ processIMF(f) }) )
####

####
# Save data
save(expData, file=paste0(inPath, 'dyadExp.rda'))
####