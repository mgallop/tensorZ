####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
dates=unique(dates$t)
####

####
# get monthly level polity from CRISP
crispVars = c(  'DEMOC', 'AUTOC' ) # Polity
library(CRISP) # version 2015.02.20
data(crisp.data); crisp=crisp.data; rm(list='crisp.data')

crisp = crisp[,c('country','ccode','date','year',crispVars)]
crisp$country = char(crisp$country)
crisp$ccode = num(crisp$ccode)
crisp$polity = crisp$DEMOC - crisp$AUTOC + 11
crisp = crisp[,setdiff(names(crisp), crispVars)]

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
wbVars = c(	
	'NY.GDP.MKTP.KD', # GDP, constant 2005 US dollars
	'NY.GDP.PCAP.KD', # GDP per capita, constant 2005 US dollars
	'SP.POP.TOTL' # population	
	)
loadPkg('WDI')
wdi=WDI(country='all', indicator=wbVars, start=2001, end=2015, extra=TRUE)

# Add logged versions of GDP and population
wdi$gdpLog = log(wdi$NY.GDP.MKTP.KD)
wdi$gdpCapLog = log(wdi$NY.GDP.PCAP.KD)
wdi$popLog = log(wdi$SP.POP.TOTL)

# Match to panel frame
loadPkg('countrycode') # Version 0.16
wdi$cname = countrycode(wdi$iso2c, 'iso2c', 'country.name')
wdi = wdi[which(wdi$cname %in% cntries$cname),
	c('cname', 'year', wbVars, 'gdpLog', 'gdpCapLog', 'popLog')]
####

####
# 
####