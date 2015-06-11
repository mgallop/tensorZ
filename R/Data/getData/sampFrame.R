####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

####
# Read sample info from IMF files
fdiPath = paste0(inPath, 'trade/')
expFiles = list.files(fdiPath)[grepl('Value of Exports', list.files(fdiPath))]

extractSamp = function(file,line, path=fdiPath){
	file = paste0(path, file)
	stuff = strsplit(readLines(file, n=2)[line], '",\"')[[1]] %>% unique()
	stuff = str_replace_all(stuff, '[[:punct:]]', '') %>% unique()
	return(stuff)
}
####

####
# Establish countries to be included in panel by matching in IMF and CRISP
# Cntries in IMF data
units = unlist( lapply(expFiles, function(f){ extractSamp(f,1) }) )
cname = countrycode(units, 'country.name', 'country.name') 
cntries = data.frame(imf=units,cname=cname)
rm(list=c('cname','units')) # Cleanup

# Cntries in CRISP data
data(crisp.data); crisp=crisp.data; rm(list='crisp.data')
crispCntry = append(char(unique(crisp$country)), 'United States')
crispCname = countrycode(crispCntry, 'country.name', 'country.name')
crispCname[crispCntry=='COSTA_RICA'] = "COSTA RICA"
crispCname[crispCntry=='UNITED_KINGDOM'] = "UNITED KINGDOM"
crispSamp = data.frame(country=crispCntry, cname=crispCname); rm(list=c('crispCname', 'crispCntry')) # Cleanup
unique(crispSamp[is.na(crispSamp$cname),c('country', 'cname')]) # check for NAs

# Cntries in IMF and CRISP
cntries$inCrisp = ifelse(cntries$cname %in% crispSamp$cname, 1, NA)
cntries = na.omit(cntries)
cntries$crisp = crispSamp$country[match(cntries$cname, crispSamp$cname)]
cntries = cntries[,setdiff(names(cntries), 'inCrisp')]
####

####
# Get dates to be included in panel based on info in IMF and CRISP
dates = data.frame( tdate = unique( 
	unlist( lapply(expFiles, function(f){ extractSamp(f,2) }) ) ), stringsAsFactors=F )
dates$date = as.Date(paste0(1, 
	unlist(lapply(strsplit(dates$tdate, ' '), function(x) x[1])), 
	unlist(lapply(strsplit(dates$tdate, ' '), function(x) x[2]))), 
	'%d%b%Y')
dates$month = unlist(lapply(strsplit(dates$tdate, ' '),function(x) x[1]))
dates$year = unlist(lapply(strsplit(dates$tdate, ' '),function(x) x[2]))

# Cut to beginning of ICEWS dataset
dates = dates[dates$date >= min(crisp$date),]
####

####
# Set up frame
frame = expand.grid(i=cntries$cname, j=cntries$cname, t=dates$date)
frame = frame[frame$i != frame$j, ]
frame = merge(frame, dates, by.x='t', by.y='date')
frame$year = num(frame$year)
####

####
# Create id variables
frame = data.table( frame )
frame[,id:=paste(i,j,t,sep='_')]
frame[,i_id:=paste(i,t,sep='_')]
frame[,j_id:=paste(j,t,sep='_')]

frame = data.frame( frame )
####

####
# Save objects
save(cntries, dates, file=paste0(inPath,'sampInfo.rda'))
save(frame, file=paste0(inPath,'frame.rda'))
####