library(terra)

shp_fl <- '../../RestoreDART_DATA/SPATIAL_DATA/all_LTDL_WRI_projects.shp'
out_dir <- '../../RestoreDART_DATA/SPATIAL_DATA/LTDL_WRI_shp_250'

if (!dir.exists(out_dir)) dir.create(out_dir)

shp <- terra::vect(shp_fl)

chunks0 <- list('1' = seq(250))
chunks <- split(seq(length(shp))[-c(1:250)], ceiling(seq_along(shp)[-c(1:250)] / 150))
chunks <- c(chunks0, chunks[1:2])
chunks1 <- split(seq(length(shp))[-c(1:450)], ceiling(seq_along(shp)[-c(1:450)] / 100))
chunks <- c(chunks, chunks1)
names(chunks) <- seq(length(chunks))

for (i in seq_along(chunks)) {
  ii <- as.integer(unlist(chunks[i]))
  
  i_shp <- shp[ii, ]
  
  ifl <- file.path(out_dir, paste0('plyID_chunk_', i, '.shp'))
  
  if (!file.exists(ifl)) terra::writeVector(i_shp, ifl)
  
}
