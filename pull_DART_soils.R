## MAIN GOAL
## Bring in the DART outputs (RData files, stored as .zip)
## Summarize mean RAP cover in 100m DART pixels
## Conduct pixel-wise BSTS/CausalImpact modeling for a treatment of interest
## Create summary tables for model fit and treatment effects

import::from(magrittr, "%>%")

source("processing_functions.R")

n_cores <- 1

soil_vars <- c('MELTON', 'soilec', 'sand')

dart_data_folder      <- "../RestoreDART_DATA/RDATA_batch/"
dart_rdata_folder     <- "../RestoreDART_DATA/RDATA_ID/"
dart_save_base_folder <- "../RestoreDART_DATA/RDATA_save/"
refrast_fl            <- "../RestoreDART_DATA/DART_rasters/refrast_conus.tif"
refrast_fl0           <- "../RestoreDART_DATA/DART_rasters/refrast_UT.tif"

if (!file.exists(refrast_fl0)) {
  refrast <- terra::rast(refrast_fl)
  UT <- spData::us_states[which(spData::us_states$NAME == 'Utah'), ]
  UT <- terra::vect(UT)
  UT <- terra::project(UT, refrast)
  refrast <- terra::crop(refrast, UT)
  refrast <- terra::mask(refrast, UT)
  terra::writeRaster(refrast, refrast_fl0, overwrite = F)
  rm(UT, refrast_fl, refrast_fl0)
} else {
  refrast <- terra::rast(refrast_fl0)
  rm(refrast_fl, refrast_fl0)
}

### Unzip all batch zips into the flat RData directory (skipping already-extracted files)
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

### Identify IDs to run

rdata_files <- list.files(dart_rdata_folder, pattern = "\\.RData$", full.names = TRUE)
unique_ids <- unique(as.numeric(gsub(".*ID_(\\d+)\\.RData$", "\\1", rdata_files)))

csv_files <- list.files(dart_save_base_folder, pattern = "_trt_effects\\.csv$", full.names = TRUE)
processed_ids <- unique(as.numeric(gsub(".*ID_(\\d+)_trt_effects\\.csv$", "\\1", csv_files)))
ids_to_run <- setdiff(unique_ids, processed_ids)

rm(zip_file, zip_files, zip_contents, to_extract, dart_data_folder, rdata_files, rdata_names, csv_files, processed_ids, unique_ids)


for (id in ids_to_run) {
  cat("Processing ID:", id, "\n")

  rdata_path <- file.path(dart_rdata_folder, paste0("ID_", id, ".RData"))
  if (!file.exists(rdata_path)) {
    stop(paste("RData file not found for ID:", id, "- expected at:", rdata_path))
  }
  load(rdata_path)
  
  cat("Data loaded for ID:", id, "\n")

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

  
  break

  extracted_target <- extraction$extractedTarget |>
    tidyr::pivot_longer(
      cols = -c(ID, x, y),
      names_to = c("functional_group", "year"),
      names_pattern = "(.*?)_(\\d{4})_v\\d+",
      values_to = "cover_value"
    ) |>
    dplyr::select(ID, x, y, functional_group, year, cover_value) |>
    dplyr::group_by(ID, functional_group, year) |>
    dplyr::summarize(cover = mean(cover_value, na.rm = TRUE), .groups = 'drop')
  
  chosenCandidates_xx <- chosenCandidates |>
    tibble::rownames_to_column(var = "reference_full_id") %>%
    #dplyr::mutate(extraction_id = dplyr::row_number()) |>
    dplyr::mutate(extracted.pixel.id = dplyr::row_number()) |>
    sf::st_drop_geometry() |> 
    dplyr::select(reference_full_id, extracted.pixel.id)
  
  stopifnot(nrow(chosenCandidates_xx) == length(unique(extraction$extractedReference$ID)))
  
  extracted_reference <- extraction$extractedReference |>
    dplyr::rename("extracted.pixel.id" = "ID") |>
    #dplyr::left_join(chosenCandidates_xx, by = c("extracted.pixel.id" = 'extraction_id')) |>
    dplyr::left_join(chosenCandidates_xx, by = c("extracted.pixel.id")) |>
    dplyr::select(-extracted.pixel.id) |>
    dplyr::rename("ID" = "reference_full_id")
    # the renaming here means that ID is maybe the row number from the buffered, rasterized padpoly SF collection
    tidyr::pivot_longer(
      cols = -c(ID, x, y),
      names_to = c("functional_group", "year"),
      names_pattern = "(.*?)_(\\d{4})_v\\d+",
      values_to = "cover_value"
    ) |>
    dplyr::select(ID, x, y, functional_group, year, cover_value) |>
    dplyr::group_by(ID, functional_group, year) |>
    dplyr::summarize(cover = mean(cover_value, na.rm = TRUE), .groups = 'drop')
    
    ref_table <- top_sims$index %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(target = dplyr::row_number()) %>%
      dplyr::select(target, dplyr::everything()) %>%
      tidyr::pivot_longer(dplyr::starts_with('V'), names_to = 'refnumber', values_to = 'reference') %>%
      dplyr::select(-refnumber)

  future::plan(future::multisession, workers = n_cores)

  # target_ids comes from extraction$extractedTarget, passed in .GlobalEnv 
  # i think target_ids are the pixel numbers
  #target_ids <- 1:dplyr::n_distinct(extracted_target$ID)
  # but if this line reassigns them, I'm not sure I can work backwards
  #fun_groups <- unique(extracted_target$functional_groups
 
  cat("Starting BSTS and CI modeling for ID:", id, "\n")
  
  combined_model_series_list <- future.apply::future_lapply(fun_groups, function(fun_group) {
    
    message(paste("Starting modeling for functional group:", fun_group))
    start_time <- Sys.time()
    target_id_chunks <- split(target_ids, ceiling(seq_along(target_ids) / ceiling(length(target_ids) / n_cores)))
    model_series_list <- future.apply::future_lapply(target_id_chunks, function(target_id_chunk) {
      
      chunk_results <- list()
      
      for (target_id in target_id_chunk) {
        
        #ts_data <- prep_ts(target_id = target_id, fun_group = fun_group, treat_year = trt_year)
        #prep_ts <- function(target_id, fun_group, treat_year) {
        years <- extracted_target %>%
          dplyr::filter(ID == target_id & functional_group == fun_group) %>%
          dplyr::pull(year) %>%
          sort()
        if (length(years) == 0) {
          warning(paste("No years found for target_id:", target_id, "and functional_group:", fun_group))
          return(NULL)
        }
        t0 <- min(years)
        tL <- max(years)
        y <- seq(t0, tL)
        ref <- ref_table %>%
          dplyr::filter(target == target_id) %>%
          dplyr::pull(reference)
        
        Xtrain <-
          extracted_reference %>%
          dplyr::filter(ID %in% ref & functional_group == fun_group) %>%
          tidyr::pivot_wider(
            names_from = ID,
            values_from = cover,
            names_prefix = 'X'
          ) %>%
          dplyr::filter(year %in% y) %>%
          dplyr::arrange(year) %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::contains('X'))
        if (nrow(Xtrain) == 0 || any(is.na(Xtrain))) {
          warning(paste("Missing or incomplete time series data for target_id:", target_id, "and functional_group:", fun_group))
          return(NULL)
        }
        Xtrain <- ts(Xtrain, start = t0, end = tL)
        Y <-
          extracted_target %>%
          dplyr::filter(ID == target_id & functional_group == fun_group) %>%
          dplyr::filter(year %in% y) %>%
          dplyr::arrange(year) %>%
          dplyr::ungroup() %>%
          dplyr::select(cover)
        if (nrow(Y) == 0 || any(is.na(Y$cover))) {
          warning(paste("Missing or incomplete cover data for target_id:", target_id, "and functional_group:", fun_group))
          return(NULL)
        }
        Y <- ts(Y$cover, start = t0, end = tL)
        Ytrain <- Y
        Ytrain[which(y >= trt_year)] <- NA
        Ypost <- Y[which(y >= trt_year)]
        #ts_data <- list(Ytrain = Ytrain, Xtrain = Xtrain, Ypost = Ypost)
          #return(list(Ytrain = Ytrain, Xtrain = Xtrain, Ypost = Ypost))
        #}
        
        #test.model <- fit_model(ts_data = ts_data, iter = 2000)
        #Ytrain <- ts_data$Ytrain
        #Xtrain <- ts_data$Xtrain
        #Ypost <- ts_data$Ypost
        
        #impact <- tryCatch({
        test.model <- tryCatch({
          ss.G <- bsts::AddLocalLevel(list(), Ytrain)
          mod.G <- bsts::bsts(
            Ytrain ~ Xtrain,
            state.specification = ss.G,
            niter = 2000,
            #niter = iter,
            ping = 0
          )
          CausalImpact::CausalImpact(bsts.model = mod.G, post.period.response = Ypost)
        }, error = function(e) e)
        #return(impact)
        
        if (!inherits(test.model, "error")) {
          if (pull_soils_skip_bsts) {
            model_series <- data.frame(target_id = integer(1), fun_group = character(1), trtYear = integer(1), PolyID = integer(1))
          } else {
            model_series <- as.data.frame(test.model$series)
            model_series$year.index <- (zoo::index(test.model$series)) + 1985
          }
          model_series$target_id <- target_id
          model_series$fun_group <- fun_group
          model_series$trtYear <- trt_year
          model_series$PolyID <- polyID
          chunk_results[[length(chunk_results) + 1]] <- model_series
        } else {
          warning(paste("Model fitting failed for target_id:", target_id, "and functional group:", fun_group))
        }
      }
      
      dplyr::bind_rows(chunk_results)
      
    }, future.seed = TRUE)
    combined_results <- dplyr::bind_rows(model_series_list)
    end_time <- Sys.time()
    duration <- end_time - start_time
    message(paste("Completed modeling for functional group:", fun_group, "in", duration, "seconds"))
    return(combined_results)
  }, future.seed = T)

#   combined_model_series <- dplyr::bind_rows(combined_model_series_list) %>%
#     dplyr::select(-cum.response, -cum.pred, -cum.pred.lower, -cum.pred.upper, -cum.effect, -cum.effect.lower, -cum.effect.upper)
#   write.csv(combined_model_series, file = paste0(dart_save_base_folder, "ID_", polyID, "_trt_effects.csv"), row.names = F)
#   cat("Completed model runs for ID:", id, "\n")
#   

}

# out_df <- as.data.frame(out_df)
# out_df <- within(out_df, rm(geometry))

### Combine per-ID trt_effects outputs into single CSV

# csv_files <- list.files(path = dart_save_base_folder, pattern = "trt_effects.*\\.csv$", full.names = TRUE)
# combined_trt_effects <- dplyr::bind_rows(lapply(csv_files, readr::read_csv))
# readr::write_csv(combined_trt_effects, file.path("../results/trt_effects_all_RestoreDART.csv"))
