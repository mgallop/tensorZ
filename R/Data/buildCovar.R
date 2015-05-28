####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
dates=unique(dates$t)
####

# Load monadic covariates
monVar = c(
	'DEMOC', 'AUTOC', # Polity
	'NY.GDP.MKTP.KD', # GDP
	'SP.POP.TOTL' # population
	 )

####
# get monthly level polity
library(CRISP)
data(crisp.data); crisp=crisp.data; rm(list='crisp.data')

crisp = crisp[,c('country','ccode','date','year',monVar[1:2])]
crisp$country = char(crisp$country)
crisp$ccode = num(crisp$ccode)
crisp$polity = crisp$DEMOC - crisp$AUTOC + 11
crisp = crisp[,setdiff(names(crisp), monVar[1:2])]

# Add in US data
slice = crisp[crisp$country=='FRANCE',]
slice$country = 'United States';slice$ccode = 2;slice$polity = 10
crisp = rbind(crisp, slice)

# Limit to countries in sample
# Removes New Zealand, Papua New Guinea, S. Korea
crisp = crisp[which(crisp$country %in% cntries$crisp),]
####

####
# get gdp/pop variables
loadPkg('WDI')
wdi=WDI(country='all', indicator=monVar[3:4], start=2001, end=2015, extra=TRUE)
loadPkg('countrycode') # Version 0.16
wdi$cname = countrycode(wdi$iso2c, 'iso2c', 'country.name')
wdi = wdi[which(wdi$cname %in% cntries$cname),c('cname','year',monVar[3:4])]
####

####
# 
####