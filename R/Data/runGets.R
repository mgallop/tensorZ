if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

# Get list of cleaning scripts
setwd(paste0(rPath, 'Data/getData/')) # navigate to clean dir
files = list.files() # get cleaning files
files = files[substrRight(files, 2) == '.R'] # only R files
cleanSripts = paste0(
	paste0(rPath, 'Data/getData/'), files )

# Parameters for parallelization
cl = makeCluster(6)
registerDoParallel(cl)

# Run cleaning scripts in paralle
foreach(script = cleanSripts) %dopar% { source(script) }
