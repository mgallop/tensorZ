####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
####

####
# get monthly level polity from CRISP
crispVars = c(  'DEMOC', 'AUTOC' ) # Polity
data(crisp.data); crisp=crisp.data; rm(list='crisp.data')

crisp = crisp[,c('country','ccode','date','year',crispVars)]
crisp$country = char(crisp$country)
crisp$ccode = num(crisp$ccode)
crisp$polity = crisp$DEMOC - crisp$AUTOC + 11
crisp = crisp[,setdiff(names(crisp), crispVars)]

# Add in US polity data...10s all the way through
slice = crisp[crisp$country=='FRANCE',]
slice$country = 'United States';slice$ccode = 2;slice$polity = 10
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