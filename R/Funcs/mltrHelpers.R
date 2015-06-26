# Cast directed dyadic variable into array
castArray = function(dyadData, var){
	library(reshape2)
	arr = acast(dyadData, i ~ j ~ t, value.var=var)
	arr[is.na(arr)] = 0	
	return(arr)
}

# Creates relational covariate matrix for single variable array
createRelCovar = function(arr, incMain=TRUE, incRecip=TRUE, incTrans=TRUE){
	if(incMain){ main = arr }
	if(incRecip){ recip = aperm(arr, c(2,1,3)) }
	if(incTrans){
		trans = arr
		for(mnth in 1:dim(arr)[3]){
			XS = ( trans[,,mnth] + t(trans[,,mnth]) )/2
			trans[,,mnth] = XS %*% XS }
	} 
	# Create new array with relational covariates
	# Additional dimensions
	relDim = incMain + incRecip + incTrans
	# Empty array
	X = array( dim=append(dim(arr), relDim, after=2) )
	# Organizing entry of elements in array
	logic = c(incMain, incRecip, incTrans) %>% cbind()
	rownames(logic) = c('ij', 'ji', 'ijk')
	logic = logic[logic[,1]==1,,drop=FALSE]
	logic = logic %>% cbind(., cumsum(.[,1]))
	# Insert into empty array
	if(incMain){ X[,,logic['ij',2],] = main }
	if(incRecip){ X[,,logic['ji',2],] = recip }
	if(incTrans){ X[,,logic['ijk',2],] = trans }
	return(X)
}

# Prep data for mltr
prepMLTR = function(
	dyadData=dirDyad, var, 
	incMain=TRUE, incRecip=TRUE, incTrans=TRUE,
	lag = TRUE, tDim=dim(Z)[4]
	){
	arr = castArray(dyadData, var)
	X = createRelCovar(arr, incMain, incRecip, incTrans)
	if(lag){ X = X[,,,-tDim,drop=FALSE] }
	return(X)
}
