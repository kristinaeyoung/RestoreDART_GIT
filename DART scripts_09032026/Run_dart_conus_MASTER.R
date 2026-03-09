#####################################
## R implementation of DART 2.0
## polygon edition
####################################

# set up working directory - this is where the output will go
setwd("/90daydata/aeroldc/DART_MASTER/")

#####################################################
stime <-Sys.time()
functions <- 'dart_functions_conus_MASTER.R'
params <- 'params_conus_MASTER.R'

source(params)
source(functions)

# ######################################################
# ## Load packages
required.packages <- c("sf", "stats", "gower", 'data.table', 'terra', 'tidyverse', 'rapr')


# # also make sure that rapr is installed - but not available on cran
# remotes::install_github("brownag/rapr")

lapply(required.packages, require, character.only=T) ## should all be true if you have all the needed packages
rm(required.packages)

######################################################
## Load areas of interest and create output folder
polygons <- st_read(dpar$polygons)
pids <- as.numeric(polygons$polyID)

dir.create(dpar$outdir, recursive = TRUE)
dir.create(file.path(dpar$outdir,'metadata'), recursive = TRUE)
file.copy(functions, file.path(dpar$outdir,'metadata'))
save(dpar, file = file.path(dpar$outdir, 'metadata','dpar.RData'))
print(dpar)
#####################################
## Set up the cluster
# library(foreach, quietly = T)
# library(parallel, quietly = T)
# library(doParallel, quietly = T)
#
# cl <- makeCluster(spec = dpar$ncores, # number of cores to use
#                   type = "PSOCK",
#                   methods = FALSE)
# registerDoParallel(cl)

######################################
## Initialize

## Run DART using lapply
lapply(pids, function(id){try(dart_fn(id))})


# #######################################
# ## Close the cluster when you're done
# registerDoSEQ()
# stopCluster(cl)
