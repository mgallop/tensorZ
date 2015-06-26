# Creates relational covariate matrix for single variable array
createRelCovar = function(arr){
	main = arr
	recip = aperm(arr, c(2,1,3))
	trans = arr
	for(mnth in 1:dim(arr)[3]){
		XS = ( trans[,,mnth] + t(trans[,,mnth]) )/2
		trans[,,mnth] = XS %*% XS }
	X = array( dim=append(dim(arr), 3, after=2) )
	X[,,1,] = main
	X[,,2,] = recip
	X[,,3,] = trans
	return(X)
}