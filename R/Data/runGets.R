if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

# Get list of cleaning scripts
setwd(paste0(rPath, 'Data/getData/')) # navigate to clean dir
files = list.files() # get cleaning files
files = files[substrRight(files, 2) == '.R'] # only R files
files = setdiff(files, 'getIQuad.R') # sql and parallel errors
cleanSripts = paste0(
	paste0(rPath, 'Data/getData/'), files )

# Parameters for parallelization
cl = makeCluster(6)
registerDoParallel(cl)

# Run cleaning scripts in parallel
foreach(script = cleanSripts) %dopar% { source(script) }
stopCluster(cl)

# Run iquad
source(paste0(rPath, 'Data/getData/getIQuad.R'))