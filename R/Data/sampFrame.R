####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

####
# Get country list
fdiPath = paste0(inPath, 'fdi/')
expFiles = paste0(fdiPath, letters[1:11], '__ Goods, Value of Exports, FOB, US Doll.csv')
impFiles = paste0(fdiPath, letters[1:11], '__ Goods, Value of Imports, CIF, US Doll.csv')

extractSamp = function(file,line){
	stuff = unique(strsplit(readLines(file, n=2)[line], '",\"')[[1]] )
	return( stuff[ c( -1, -length(stuff) ) ] )
}

# Cntries in IMF data
units = unlist( lapply(expFiles, function(f){ extractSamp(f,1) }) )
loadPkg('countrycode') # Version 0.16
cname = countrycode(units, 'country.name', 'country.name') 
cntries = data.frame(imf=units,cname=cname); rm(list=c('cname','units')) # Cleanup

# Cntries in CRISP data
library(CRISP); data(crisp.data); crisp=crisp.data; rm(list='crisp.data')
crispCntry = append(char(unique(crisp$country)), 'United States')
loadPkg('countrycode') # Version 0.16
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

# Get dates 
dates = data.frame(tdate=unique( 
	unlist( lapply(expFiles, function(f){ extractSamp(f,2) }) ) ))
dates$tdate = char(dates$tdate)
dates$month = unlist(lapply(strsplit(dates$tdate, ' '),function(x) x[1]))
dates$year = unlist(lapply(strsplit(dates$tdate, ' '),function(x) x[2]))
dates$date = as.Date(paste0(1, dates$month, dates$year), '%d%b%Y')

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
# Save objects
save(expFiles, impFiles, cntries, dates, file=paste0(inPath,'sampInfo.rda'))
save(frame, file=paste0(inPath,'frame.rda'))
####