####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
####

####
# get monthly level polity from CRISP
crispVars = c(  'DEMOC', 'AUTOC', # Polity
	'NY.GDP.MKTP.KD', # GDP, constant 2005 US dollars
	'NY.GDP.PCAP.KD', # GDP per capita, constant 2005 US dollars
	'SP.POP.TOTL') # population	  
data(crisp.data); crisp=crisp.data; rm(list='crisp.data')

crisp = crisp[,c('country','ccode','date','year',crispVars)]
crisp$country = char(crisp$country)
crisp$ccode = num(crisp$ccode)
crisp$polity = crisp$DEMOC - crisp$AUTOC + 11
crisp = crisp[,setdiff(names(crisp), crispVars)]

# Add in US data...10s all the way through
## Polity
slice = crisp[crisp$country=='FRANCE',]
slice$country = 'United States';slice$ccode = 2;slice$polity = 10
## IMF WEI Dataset
imf = read.csv(paste0(inPath, 'weoreptc.csv'))
relVars = c('Population', 'Gross domestic product, constant prices', 'Gross domestic product per capita, constant prices')
relUnits = c('National currency', 'Persons')
imfUS = imf %>% .[.$Country=='United States',] %>% .[.$Subject.Descriptor %in% relVars,] %>% .[.$Units %in% relUnits,]
names(imfUS)[7:(ncol(imfUS) - 1)] = 1999:2014
imfUS = imfUS[, c( 'Subject.Descriptor',char(1999:2014) ) ] %>% melt(.,id='Subject.Descriptor') %>% dcast(., variable ~ Subject.Descriptor)
names(imfUS) = c('year', 'NY.GDP.PCAP.KD', 'NY.GDP.MKTP.KD', 'SP.POP.TOTL')
for(ii in 1:ncol(imfUS)){ imfUS[,ii] = num(imfUS[,ii]) }

# 999993458   996536.8

crisp = rbind(crisp, slice)

# Limit to countries in sample
# Removes New Zealand, Papua New Guinea, S. Korea
crisp = crisp[which(crisp$country %in% cntries$crisp),]

# Add cname and id vector
crisp$cname = cntries$cname[match(crisp$country, cntries$crisp)]
crisp$id = paste(crisp$cname, crisp$date, sep='_')
####

####
# Save
save(crisp, file=paste0(inPath, 'crisp.rda'))
####