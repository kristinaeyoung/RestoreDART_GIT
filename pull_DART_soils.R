## MAIN GOAL
## Bring in the DART outputs (RData files, stored as .zip)
## Summarize mean RAP cover in 100m DART pixels
## Conduct pixel-wise BSTS/CausalImpact modeling for a treatment of interest
## Create summary tables for model fit and treatment effects

import::from(magrittr, "%>%")

source("processing_functions.R")

n_cores <- 1

pull_soils_skip_bsts <- TRUE

soil_vars <- c('MELTON', 'soilec', 'sand')

dart_data_folder      <- "../RestoreDART_DATA/RDATA_batch/"
dart_rdata_folder     <- "../RestoreDART_DATA/RDATA_ID/"
dart_save_base_folder <- "../RestoreDART_DATA/RDATA_save/"

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

# out_df <- data.frame()

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

  # soil_df <- padpixels[, which(colnames(padpixels) %in% soil_vars)]
  # soil_df$polyID <- polyID
  # 
  # if (nrow(out_df) == 0) {
  #   out_df <- soil_df
  # } else {
  #   out_df <- rbind(out_df, soil_df)
  # }
  # 
  # # target_id is the pixel
  # 
  # l0 <- ls()
  # l0 <- l0[-which(l0 %in% c("dart_rdata_folder", "dart_save_base_folder", "ids_to_run", "out_df", "soil_vars"))]
  # rm(list = l0)
  
  #id_c0 <- extraction$extractedTarget[, c('ID', 'x', 'y')]
  #id_c0 <- id_c0[!duplicated(id_c0), ]
  
  chosenCandidates <- chosenCandidates |>
    tibble::rownames_to_column(var = "reference_full_id") %>%
    dplyr::mutate(extraction_id = dplyr::row_number())

  extractedReference_wide <- extraction$extractedReference |>
    dplyr::rename("extracted.pixel.id" = "ID") |>
    dplyr::left_join(chosenCandidates |> sf::st_drop_geometry() |> dplyr::select(reference_full_id, extraction_id),
              by = c("extracted.pixel.id" = 'extraction_id')) |>
    dplyr::select(-extracted.pixel.id) |>
    dplyr::rename("ID" = "reference_full_id")

  extracted_target    <- summarize_data(pivot_data(extraction$extractedTarget))
  extracted_reference <- summarize_data(pivot_data(extractedReference_wide))
  ref_table           <- reshape_top_sims(top.sims)

  future::plan(future::multisession, workers = n_cores)

  target_ids <- 1:dplyr::n_distinct(extracted_target$ID)
  # target_ids comes from extraction$extractedTarget, passed in .GlobalEnv 
  # used in run_model_for_fun_group
  fun_groups <- unique(extracted_target$functional_group)
 
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
