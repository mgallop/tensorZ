####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

####
# Load and combine processed data
setwd(inPath)

# Trade data 
load('dyadExp.rda') # loads expData object

# Monadic variables (polity from CRISP, gdp & pop from worldbank)

## CRISP
load('crisp.rda')
### Check for matches
stopifnot( length(intersect(expData$i_id, crisp$id))==nrow(crisp) )
stopifnot( length(intersect(expData$j_id, crisp$id))==nrow(crisp) )
### Merge in variables
expData$i_polity = crisp$polity[match(expData$i_id, crisp$id)]
expData$j_polity = crisp$polity[match(expData$j_id, crisp$id)]
rm(list='crisp')

## World Bank Data
load('wbData.rda')
### Check for matches
stopifnot( length(intersect(expData$i_id, wbData$id))==nrow(wbData) )
stopifnot( length(intersect(expData$j_id, wbData$id))==nrow(wbData) )
### Merge in variables
wbVars = names(wbData)[-1]
for(var in wbVars){
	expData$tmp = wbData[,var][match(expData$i_id, wbData$id)]
	names(expData)[ncol(expData)] = paste('i', var, sep='_')
	expData$tmp = wbData[,var][match(expData$j_id, wbData$id)]
	names(expData)[ncol(expData)] = paste('j', var, sep='_') }
rm(list='wbData')

# Dyadic variables (alliance from COW, geo from cshapes, quad from ICEWS)

## Aliance
load('ally.rda')
### Check for matches
stopifnot( length( intersect(ally$id, expData$id) ) == nrow(expData) )
### Merge in variable
expData$ally = ally$ally[match(expData$id, ally$id)]
rm(list='ally')

## Distance
load('distData.rda')
### Check for matches
stopifnot( length( intersect(distData$id, expData$id) ) == nrow(distData) )
### Merge in variables
distVars = c('capDist', 'minDist', 'capDistLog', 'minDistLog')
for(var in distVars){
	expData$tmp = distData[,var][match(expData$id, distData$id)]
	names(expData)[ncol(expData)] = var }

## ICEWS Quad variables
load('quad.rda')
### Check for matches
stopifnot( length( intersect(quadData$id, expData$id) ) == nrow(quadData) )
### Merge in variables
quadVars = c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
for(var in quadVars){
	expData$tmp = quadData[,var][match(expData$id, quadData$id)]
	names(expData)[ncol(expData)] = var }
####

####
# Save
dirDyad = expData
save(dirDyad, file=paste0(inPath, 'dirDyad.rda'))
####