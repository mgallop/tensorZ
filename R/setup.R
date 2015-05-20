rm(list=ls())

if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	inPath='~/Dropbox/Research/WardProjects/tensorZ/Data/toModel/';
	outPath='~/Dropbox/Research/WardProjects/tensorZ/Data/fromModel/';
	rFuncs='~/Research/WardProjects/tensor/R/Funcs/';
}

# General functions/libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

toLoad=c( 'ggplot2', 'reshape2', 'data.table' )
loadPkg(toLoad)

char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }

# Set a theme for gg
theme_set(theme_bw())

# Global params
seed=6886
set.seed(seed)