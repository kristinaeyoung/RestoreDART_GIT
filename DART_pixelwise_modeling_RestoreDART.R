## MAIN GOAL
## Bring in the DART outputs (RData files, stored as .zip)
## Summarize mean RAP cover in 100m DART pixels
## Conduct pixel-wise BSTS/CausalImpact modeling for a treatment of interest
## Create summary tables for model fit and treatment effects

source("processing_functions.R")

#n_cores <- 1

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

for (id in ids_to_run) {
  cat("Processing ID:", id, "\n")

  rdata_path <- file.path(dart_rdata_folder, paste0("ID_", id, ".RData"))
  if (!file.exists(rdata_path)) {
    stop(paste("RData file not found for ID:", id, "- expected at:", rdata_path))
  }
  load(rdata_path)
  break
  
  cat("Data loaded for ID:", id, "\n")
  # soils data is in padpixels?

#   trt_year <- padpoly |> sf::st_drop_geometry() |> dplyr::select(trtYear) |> dplyr::mutate(trtYear = as.numeric(trtYear)) |> dplyr::pull(trtYear)
#   polyID   <- padpoly |> sf::st_drop_geometry() |> dplyr::select(polyID)  |> dplyr::mutate(polyID  = as.numeric(polyID))  |> dplyr::pull(polyID)
# 
#   chosenCandidates <- chosenCandidates |>
#     tibble::rownames_to_column(var = "reference_full_id") %>%
#     dplyr::mutate(extraction_id = dplyr::row_number())
# 
#   extractedReference_wide <- extraction$extractedReference |>
#     dplyr::rename("extracted.pixel.id" = "ID") |>
#     dplyr::left_join(chosenCandidates |> sf::st_drop_geometry() |> dplyr::select(reference_full_id, extraction_id),
#               by = c("extracted.pixel.id" = 'extraction_id')) |>
#     dplyr::select(-extracted.pixel.id) |>
#     dplyr::rename("ID" = "reference_full_id")
# 
#   extracted_target    <- summarize_data(pivot_data(extraction$extractedTarget))
#   extracted_reference <- summarize_data(pivot_data(extractedReference_wide))
#   ref_table           <- reshape_top_sims(top.sims)
# 
#   future::plan(future::multisession, workers = n_cores)
# 
#   target_ids <- 1:dplyr::n_distinct(extracted_target$ID)
#   fun_groups <- unique(extracted_target$functional_group)
# 
#   cat("Starting BSTS and CI modeling for ID:", id, "\n")
#   combined_model_series_list <- future.apply::future_lapply(fun_groups, run_model_for_fun_group, future.seed = T)
# 
#   combined_model_series <- dplyr::bind_rows(combined_model_series_list) %>%
#     dplyr::select(-cum.response, -cum.pred, -cum.pred.lower, -cum.pred.upper, -cum.effect, -cum.effect.lower, -cum.effect.upper)
#   write.csv(combined_model_series, file = paste0(dart_save_base_folder, "ID_", polyID, "_trt_effects.csv"), row.names = F)
#   cat("Completed model runs for ID:", id, "\n")
#   
 }


### Combine per-ID trt_effects outputs into single CSV

# csv_files <- list.files(path = dart_save_base_folder, pattern = "trt_effects.*\\.csv$", full.names = TRUE)
# combined_trt_effects <- dplyr::bind_rows(lapply(csv_files, readr::read_csv))
# readr::write_csv(combined_trt_effects, file.path("../results/trt_effects_all_RestoreDART.csv"))
