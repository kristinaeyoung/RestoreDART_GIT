### creating treatment masks for DART 

### this script brings in a sf feature with multiple treatment polygons,
## then creates a mask for all other treatments other than treatment of interest 
## saves as a static raster


library(terra)
library(sf)
library(tidyterra)
library(tidyverse)
# Load the polygon shapefile as an sf object
polygons <- st_read("C:/Users/gharrison/OneDrive - USDA/Documents/RAP Research/RestoreNM_RAP/Data/RNM_trt_forDART.shp")
# Load reference raster
refrast <- terra::rast("D:/DART_rasters/refrast_conus.tif")

# Define the DART function
create_treatment_mask <- function(id) {
  try({
    cat("Starting polygon ID:", id, "\n")

    # Step 1: Filter polygons to include all but the treatment of interest
    other_treatments <- polygons[polygons$polyID != id, ]
    
    # Set a field value to rasterize as 1 for other treatments
    other_treatments$value <- 1
    
    # Step 2: Rasterize other treatments (they will be set as 1 in the raster)
    treatment_rast <- terra::rasterize(
      x = other_treatments,
      y = refrast,
      field = "value")
    # Step 3: Create mask (non-treatment area = 1, treatment area = 0)
    treatment_mask <- terra::classify(treatment_rast, cbind(1, 1), include.lowest = TRUE)
    treatment_mask[is.na(treatment_mask)] <- 0  # Non-treatment area set to 0
    # Save the treatment mask as a raster file
    output_filename <- paste0("D:/DART_rasters/trt_masks/trt_mask_", id, ".tif")
    terra::writeRaster(treatment_mask, output_filename, overwrite = TRUE)
    cat("Treatment mask saved for polygon ID:", id, "at", output_filename, "\n")
  })
}

# Define your polygon IDs
pids <- unique(polygons$polyID)

# Run DART using lapply
lapply(pids, function(id) { create_treatment_mask(id) })



# ## check 
# test <- terra::rast("D:/DART_rasters/trt_masks/trt_mask_12238.tif")
test <- terra::rast("D:/DART_rasters/RNM_trt_mask.tif")

#rnm_trts = "D:/DART_rasters/RNM_trt_mask.tif",
test_cropped <- terra::crop(test, polygons)
plot(test_cropped)

trt_pixel_map <- ggplot()+  geom_spatraster(data = test_cropped)+
  geom_spatvector(data = terra::vect(polygons|> filter(polyID == "12238")), fill = 'coral1')+
  theme_void()
trt_pixel_map














## try with roads layer
