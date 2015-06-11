####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
####

####
# csv files with export data
fdiPath = paste0(inPath, 'trade/')
expFiles = list.files(fdiPath)[grepl('Value of Exports', list.files(fdiPath))]
####

####
# Create directed dyadic datasets
# Function to process IMF DOT csv Sheets
processIMF = function(file, path=fdiPath, verbose=FALSE){
	if(verbose){print(paste0('Processing ', file, '...'))}
	slice = read.csv(paste0(path, file), header=FALSE)
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
	slice$j = slice$j %>% char()
	slice$value = num(slice$value)

	# Drop i - i
	slice = slice[which(slice$i != slice$j), ]

	# Drop items not in cntries vector
	slice = slice[which(slice$i %in% cntries$imf), ]
	slice = slice[which(slice$j %in% cntries$imf), ]
	if(verbose){cat(paste0('\t\t',length(unique(slice$j)), ' out of ', nrow(cntries)-1,' j countries included\n' ))	}

	# Convert to cname
	slice$i = cntries$cname[match(slice$i, cntries$imf)] %>% char()
	slice$j = cntries$cname[match(slice$j, cntries$imf)] %>% char()

	# Subset slice by dates in sampFrame
	slice = slice[which(slice$t %in% dates$tdate), ]
	if(verbose){cat(paste0('\t\t',length(unique(slice$t)), ' out of ', nrow(dates),' t dates included\n' ))		}

	# Return object
	return(slice)
}

expData = lapply(expFiles, function(f){ processIMF(f) }) %>% do.call('rbind', .)
####

####
# Cleaning 
# Add in cleaned date variable
expData$t = dates$date[match(expData$t, dates$tdate)]

# Add in unique id
expData = data.table( expData )
expData[,id:=paste(i,j,t,sep='_')]

# Merge with frame
load(paste0(inPath,'frame.rda'))
frame$exports = expData$value[match(frame$id, expData$id)]
frame$exports[is.na(frame$exports)] = 0
expData = frame
####

####
# Save data
save(expData, file=paste0(inPath, 'dyadExp.rda'))
####