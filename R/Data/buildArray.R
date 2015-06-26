####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
####

####
# Load directed dyadic dataset
load(paste0(inPath, 'dirDyad.rda'))

# Alliance dataframe ends at 2012
# dirDyad = dirDyad[dirDyad$year<2013,]

# Log Exports
dirDyad$exportsLog = log( dirDyad$exports + 1 )

# Standardize material conflict variable
dirDyad$matlConfStd = ( dirDyad$matlConf - mean( dirDyad$matlConf ) ) / sd( dirDyad$matlConf )

# Check for NAs
# stopifnot( sum(is.na(dirDyad)) == 0 )
####

####
# Set up array for endogenous covariates

# Cast into array format
expArr = acast(dirDyad, i ~ j ~ t, value.var='exportsLog')
confArr = acast(dirDyad, i ~ j ~ t, value.var='matlConfStd')

# Replace diagonal elements with zeros
expArr[is.na(expArr)] = 0
confArr[is.na(confArr)] = 0

# Combine arrays
Z = array( dim=append( dim(expArr), 2, after=2 ) )
Z[,,1,] = expArr
Z[,,2,] = confArr

# Set up relational covariates
ZM = Z # ij, main effect

ZR = aperm(Z, c(2,1,3,4)) # ji, reciprocal effect

ZT = Z # ijk, transitive effect
for( var in 1:dim(ZT)[3] ){
	for( mnth in 1:dim(ZT)[4] ){
		XS = ( ZT[,,var,mnth] + t(ZT[,,var,mnth]) )/2
		ZT[,,var,mnth] = XS %*% XS
	}
}

# Set up lagging strategy and create DV array
Y = Z[,,,-1]

# Combine relational covariates into one array
Xendo = array( dim=append( dim(Y), 3, after=3 ) )
Xendo[,,,1,] = ZM[,,,-dim(Z)[4]]
Xendo[,,,2,] = ZR[,,,-dim(Z)[4]]
Xendo[,,,3,] = ZT[,,,-dim(Z)[4]]
####

####
# Create arrays for exogenous covariates
source(paste0(rFuncs, 'mltrHelpers.R'))
ptaArr = acast(dirDyad, i ~ j ~ t, value.var='ptaCnt')
ptaArr[is.na(ptaArr)] = 0
ptaX = createRelCovar(ptaArr)
distArr = acast(dirDyad, i ~ j ~ t, value.var='minDistLog')
distArr[is.na(distArr)] = 0
distX = createRelCovar(distArr)

# Account for lag
ptaX = ptaX[,,,-dim(Z)[4]]
distX = distX[,,,-dim(Z)[4]]

# Include in equations for both matlconflict and exports
tmp = array(dim=dim(Xendo))
tmp[,,1,,] = ptaX; tmp[,,2,,] = ptaX
ptaX = tmp
tmp[,,1,,] = distX; tmp[,,2,,] = distX
distX = tmp

# Add exogenous covariates to X array
X = array( dim = append(dim(Xendo), 3, after=4) )
X[,,,,1,] = Xendo
X[,,,,2,] = ptaX
X[,,,,3,] = distX
####

####
# Label and save

## labels for DV
dimnames(Y)[1:2] = dimnames(expArr)[1:2]
dimnames(Y)[[3]] = c('exports', 'matlConf')
dimnames(Y)[[4]] = dimnames(expArr)[[3]][-1]

## Labels for X
dimnames(X)[1:3] = dimnames(Y)[1:3]
dimnames(X)[[4]] = c("ij","ji","ijk") 
dimnames(X)[[5]] = c('endo', 'pta', 'dist')
dimnames(X)[[6]] = dimnames(Y)[[4]]

# Finalize arrays
dnX = dimnames(X)
X = aperm( apply(X, c(1,2,5,6), 'c'), c(2,3,1,4,5) )
dimnames(X)[1:2] = dnX[1:2]
dimnames(X)[[4]] = dnX[[5]]
dimnames(X)[[3]] = c( outer( dnX[[3]], dnX[[4]], paste0 ) )

dnY = dimnames(Y)
Y = array(Y, dim=append( dim(Y), c(1), after=3 ) )
dimnames(Y)[1:3] = dnY[1:3]
dimnames(Y)[[5]] = dnY[[4]]

## save 
save(Y, X, file=paste0(inPath, "YX.rda"))
####