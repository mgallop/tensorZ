rm(list=ls())

if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	inPath='~/Dropbox/Research/WardProjects/tensorZ/Data/toModel/';
	outPath='~/Dropbox/Research/WardProjects/tensorZ/Data/fromModel/';
	rPath='~/Research/WardProjects/tensorZ/R/';
	rFuncs=paste0(rPath, 'Funcs/')
}

# General functions/libraries
## See info on package versions and other session info
### at bottom of script
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

toLoad=c('foreign' , 'countrycode' ,
	'ggplot2', 
	'reshape2', 'magrittr', 'dplyr', 'stringr','data.table'
	)
loadPkg(toLoad)

char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }

# Set a theme for gg
theme_set(theme_bw())

# Global params
seed=6886
set.seed(seed)

# File with helpful functions
source(paste0(rFuncs, 'sqlHelpers.R'))

# Session info
sessionInfo()