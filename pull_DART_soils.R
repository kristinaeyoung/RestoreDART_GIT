# BEM Feb/Mar 2026
library(dplyr)
library(sf)

#soil_vars <- c('MELTON', 'soilec', 'sand')

dart_data_folder      <- "../RestoreDART_DATA/RDATA_batch/"
dart_rdata_folder     <- "../RestoreDART_DATA/RDATA_ID/"
dart_save_base_folder <- "../RestoreDART_DATA/RDATA_save/"
d0_fl <- "../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data.csv"
out_fl <- "../RestoreDART_DATA/MIXED_MODELS/xx_soil_df.csv"
coord_fl <- "../RestoreDART_DATA/MIXED_MODELS/xx_coord_df.csv"

d0 <- read.csv(d0_fl)

zip_files <- list.files(dart_data_folder, pattern = "\\.zip$", full.names = TRUE)[-1]

for (zip_file in zip_files) {
  zip_contents <- unzip(zip_file, list = TRUE)$Name
  rdata_names  <- basename(zip_contents[grepl("\\.RData$", zip_contents)])
  to_extract   <- zip_contents[!file.exists(file.path(dart_rdata_folder, rdata_names))]
  if (length(to_extract) > 0) {
    unzip(zip_file, files = to_extract, exdir = dart_rdata_folder, junkpaths = TRUE)
    cat("Extracted", length(to_extract), "file(s) from", basename(zip_file), "\n")
  } else {
    cat("All files already extracted from", basename(zip_file), "- skipping.\n")
  }
}

rdata_files <- list.files(dart_rdata_folder, pattern = "\\.RData$", full.names = TRUE)
unique_ids <- unique(as.numeric(gsub(".*ID_(\\d+)\\.RData$", "\\1", rdata_files)))

csv_files <- list.files(dart_save_base_folder, pattern = "_trt_effects\\.csv$", full.names = TRUE)
processed_ids <- unique(as.numeric(gsub(".*ID_(\\d+)_trt_effects\\.csv$", "\\1", csv_files)))
ids_to_run <- setdiff(unique_ids, processed_ids)

soil_df <- data.frame()
coord_df <- data.frame()

#n_check <- 138
# run 3/27/26 for 950 polygons, 1014247 rows (PolyID x target_id)

for (i in seq_along(ids_to_run)) {
  
  # skip for checking
  #if (i < n_check) next
  
  id <- ids_to_run[i]
  cat("i:", i, "\n")

  rdata_path <- file.path(dart_rdata_folder, paste0("ID_", id, ".RData"))
  if (!file.exists(rdata_path)) {
    stop(paste("RData file not found for ID:", id, "- expected at:", rdata_path))
  }
  load(rdata_path)
  
  trt_year <- padpoly |> sf::st_drop_geometry() |> dplyr::select(trtYear) |> dplyr::mutate(trtYear = as.numeric(trtYear)) |> dplyr::pull(trtYear)
  polyID   <- padpoly |> sf::st_drop_geometry() |> dplyr::select(polyID)  |> dplyr::mutate(polyID  = as.numeric(polyID))  |> dplyr::pull(polyID)
  
  # check to make sure "ID" in extraction$extractedTarget is equal to the row index in padpixels
  p0 <- padpixels
  p0$SOLIS_pixel <- as.numeric(seq(nrow(p0)))
  t0 <- extraction$extractedTarget
  t0$RAP_pixel <- seq(nrow(t0))
  t0_pts <- sf::st_as_sf(t0[, c('ID', 'RAP_pixel', 'x', 'y')], coords = c('x', 'y'), crs = sf::st_crs(p0$geometry))
  i0 <- sf::st_join(t0_pts, p0)
  stopifnot(identical(i0$ID, i0$SOLIS_pixel))
  # if this passes, it means that the ID column in i0 (which has the soil data) is the same as ID in t0

  # check d0$PolyID against polyID and d0$target_id against i0$ID
  # skip if previously checked to speed up
  #if (i < n_check) {
  if (FALSE) {
    # this works with the previous version of 1_combined_filter_input_data.csv, but not with updated version
    di <- d0 |>
      filter(PolyID == polyID) |>
      select(response, fun_group, target_id, year.index) |>
      arrange(target_id, fun_group, year.index) |>
      mutate(response = round(response, 1))
    t00 <- t0 |>
      select(-RAP_pixel) |>
      tidyr::pivot_longer(!any_of(c('ID', 'x', 'y'))) |>
      tidyr::separate_wider_delim('name', '_', names = c('var1', 'var2', 'var3', 'var4', 'var5', 'year', 'v'), too_few = 'align_end') |>
      tidyr::unite('fun_group', any_of(c('var1', 'var2', 'var3', 'var4', 'var5')), sep = '_', na.rm = T, remove = T) |>
      rename(year.index = 'year', target_id = 'ID', response = 'value') |>
      select(-v) |>
      filter(year.index %in% di$year.index) |>
      group_by(target_id, fun_group, year.index) |>
      summarise(response = mean(response), .groups = 'drop') |>
      arrange(target_id, fun_group, year.index) |>
      mutate(response = round(response, 1))
    
    # just dont check tree cover or bare ground cover
    # 8, 54, 96 have tree_cover issues, probably others
    # 18, 27, 138 have bare_ground_cover issues, probably others
    # 138 has litter cover issues, dropped after that
    t00 <- filter_out(t00, fun_group %in% c('tree_cover', 'bare_ground_cover', 'litter_cover'))
    di <- filter_out(di, fun_group %in% c('tree_cover', 'bare_ground_cover', 'litter_cover'))
    
    if (i == 18) {
      # t00 is longer
      # bare ground missing from 204, 206, 220, 222 in di
      # shrub cover missing from 222 in di
      t00 <- filter_out(t00, fun_group == 'shrub_cover' & target_id == 222)
    }
    if (i == 27) {
      # just nuked whole functional groups to check
      t00 <- filter_out(t00, fun_group %in% c('bare_ground_cover', 'shrub_cover', 'annual_forb_and_grass_cover'))
      di <- filter_out(di, fun_group %in% c('bare_ground_cover', 'shrub_cover', 'annual_forb_and_grass_cover'))
    }
    
    stopifnot(
      all(t00$target_id %in% di$target_id),
      nrow(t00) == nrow(di),
      identical(t00$response, di$response)
    )
    # if this passes, it means that 1_combined_filter_input_data.csv$target_id is the same as ID in t0
    # therefore, ID column in i0 (soil data) can be matched up to target_id in di
    
    # debug
    if (F) {
      t00$year.index <- as.integer(t00$year.index)
      di <- di[, c('target_id', 'fun_group', 'year.index', 'response')]
      di_pad <- nrow(t00) - nrow(di)
      di <- tibble::add_row(di, target_id = rep(NA, di_pad))
      t0_mis <- t00[!(t00$response == di$response), ]
      di_mis <- di[!(t00$response == di$response), ]
      mis <- cbind(t0_mis, di_mis)
    }
    
  }
  
  # pull the soil data
  idf <- i0 |>
    as.data.frame() |>
    mutate(PolyID = rep(polyID, nrow(i0))) |>
    select(-RAP_pixel, -refrast, -nlcd, -mtbs_wildfires, -historic_fires, -ltdl_wri_trts, -roads, -riparian, -ltdl_wri_overlapping_trt, -SOLIS_pixel, -geometry) |>
    rename(target_id = 'ID') |>
    relocate(target_id) |>
    relocate(PolyID)
  
  # pull the coordinate data
  cdf <- i0 |>
    subset(select = c(ID, RAP_pixel, SOLIS_pixel))
  cdf_c <- sf::st_coordinates(cdf)
  cdf <- cbind(data.frame(polygon = rep(polyID, nrow(cdf))), sf::st_drop_geometry(cdf), cdf_c)
  # CRS: "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  
  # collate the soil data
  if (i == 1) {
    soil_df <- idf
  } else {
    soil_df <- rbind(soil_df, idf)
  }
  
  # collate the coordinate data
  if (i == 1) {
    coord_df <- cdf
  } else {
    coord_df <- rbind(coord_df, cdf)
  }

}

if (!file.exists(out_fl)) {
  write.csv(soil_df, out_fl, row.names = F)
}
if (!file.exists(coord_fl)) {
  write.csv(coord_df, coord_fl, row.names = F)
}