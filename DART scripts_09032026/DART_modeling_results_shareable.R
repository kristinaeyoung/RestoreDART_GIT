##################################################
## MAIN GOAL 
## bring in the DART outputs (Rdata files)
## summarize mean RAP cover in 100m DART pixels
## conduct pixel wise modeling bsts for a treatment of interest with just that pixel's reference values
## create summary tables and maps for model fit + treatment effects

library(tidyverse)
library(tidyterra)
library(terra)
library(CausalImpact)
library(bsts)
library(sf)
library(future)
library(future.apply)

##################################################################################################################################
#### functions
# Function to pivot the extracted RAP data 
pivot_data <- function(extracted_data) {
  extracted_data |>
    pivot_longer(
      cols = -c(ID, x, y), 
      names_to = c("functional_group", "year"), 
      names_pattern = "(.*?)_(\\d{4})_v\\d+", 
      values_to = "cover_value"
    ) |>
    select(ID, x, y, functional_group, year, cover_value)
}
# Function to summarize the dataset - mean RAP cover in 100m dart pixel
summarize_data <- function(pivoted_data) {
  pivoted_data |>
    group_by(ID, functional_group, year) |>
    summarize(cover = mean(cover_value, na.rm = TRUE), .groups = 'drop')
}

# Function to reshape top.sims into a long data frame
reshape_top_sims <- function(top_sims) {
  ref_table <- top_sims$index %>% 
    t() %>%
    as_tibble() %>%
    mutate(target = row_number()) %>%
    select(target, everything()) %>%
    pivot_longer(starts_with('V'),names_to = 'refnumber',values_to = 'reference') %>%
    select(-refnumber)
  return(ref_table)
}

# Function to prepare time series data for model fitting.
prep_ts <- function(target_id, fun_group, treat_year) {
  # Get start and end years
  years <- extracted_target %>%
    filter(ID == target_id & functional_group == fun_group) %>%
    pull(year) %>%
    sort()
  if (length(years) == 0) {
    warning(paste("No years found for target_id:", target_id, "and functional_group:", fun_group))
    return(NULL)
  }
  t0 <- min(years)
  tL <- max(years)
  y <- seq(t0, tL)  # Sequence of years
  # Vector of reference pixel IDs
  ref <- ref_table %>%
    filter(target == target_id) %>%
    pull(reference)
  # Select and reshape data
  # Xtrain is a time series object with complete cover time series
  # at each of the n = length(ref) reference pixels
  Xtrain <- 
    extracted_reference %>% 
    filter(ID %in% ref & functional_group == fun_group) %>%
    pivot_wider(
      names_from = ID,
      values_from = cover,
      names_prefix = 'X'
    ) %>%
    filter(year %in% y) %>%
    arrange(year) %>%
    ungroup() %>%
    select(contains('X')) 
  
  # Check if Xtrain is empty or has missing data
  if (nrow(Xtrain) == 0 || any(is.na(Xtrain))) {
    warning(paste("Missing or incomplete time series data for target_id:", target_id, "and functional_group:", fun_group))
    return(NULL)
  }
  
  # Convert Xtrain to time series
  Xtrain <- ts(Xtrain, start = t0, end = tL)
  
  # Y is the complete cover time series at the treated pixel
  Y <- 
    extracted_target %>%
    filter(ID == target_id & functional_group == fun_group) %>%
    filter(year %in% y) %>%
    arrange(year) %>%
    ungroup() %>%
    select(cover)
  # Check if Y is empty or has missing data
  if (nrow(Y) == 0 || any(is.na(Y$cover))) {
    warning(paste("Missing or incomplete cover data for target_id:", target_id, "and functional_group:", fun_group))
    return(NULL)
  }
  # Convert Y to time series
  Y <- ts(Y$cover, start = t0, end = tL)
  # Ytrain is a time series object for the full sequence of years at
  # the target pixel, but with post-treatment observations removed (NA)
  Ytrain <- Y
  Ytrain[which(y >= treat_year)] <- NA
  # Ypost is the time series of cover observed after the treatment
  Ypost <- Y[which(y >= treat_year)]
  return(list(Ytrain = Ytrain, Xtrain = Xtrain, Ypost = Ypost))
}

# helper function for fitting the bsts and CI model
fit_model <- function(ts_data, iter){ 
  Ytrain <- ts_data$Ytrain
  Xtrain <- ts_data$Xtrain
  Ypost <- ts_data$Ypost
  impact <- tryCatch({ 
    ss.G <- AddLocalLevel(list(), Ytrain)
    mod.G <- bsts(
      Ytrain ~ Xtrain,
      state.specification = ss.G,
      niter = iter, 
      ping = 0
    )
    CausalImpact(bsts.model = mod.G, post.period.response = Ypost)
  }, error = function(e) e )
  return(impact)
}

# run the model for each pixel and functional group
# this is set up to run in parallel
run_model_for_fun_group <- function(fun_group) {
  message(paste("Starting modeling for functional group:", fun_group))  # Debug message
  start_time <- Sys.time()  # Record start time for this functional group
  # Split the target IDs into chunks for parallel processing within this functional group
  target_id_chunks <- split(target_ids, ceiling(seq_along(target_ids) / ceiling(length(target_ids) / n_cores)))
  # Run model fitting in parallel for each chunk of target IDs within the functional group
  model_series_list <- future_lapply(target_id_chunks, function(target_id_chunk) {
    chunk_results <- list()  # Store results for each target ID in this chunk
    # Move this loop inside the parallel function
    for (target_id in target_id_chunk) {
      #message(paste("Modeling target ID:", target_id, "of", number_of_target_pixels, "for functional group:", fun_group))  # Debug message
      # Prepare the time series data
      ts_data <- prep_ts(target_id = target_id, fun_group = fun_group, treat_year = trt_year)
      # Fit the model and get impact analysis
      test.model <- fit_model(ts_data = ts_data, iter = 2000)
      if (!inherits(test.model, "error")) {
        # Process and store results
        model_series <- as.data.frame(test.model$series)
        model_series$year.index <- (index(test.model$series)) + 1985
        model_series$target_id <- target_id
        model_series$fun_group <- fun_group
        model_series$trtYear <- trt_year
        model_series$PolyID <- polyID
        chunk_results[[length(chunk_results) + 1]] <- model_series
      } else {
        warning(paste("Model fitting failed for target_id:", target_id, "and functional group:", fun_group))
      }
    }
    # Combine all results from this chunk
    bind_rows(chunk_results)
  }, future.seed = TRUE)
  # Combine all chunk results for this functional group
  combined_results <- bind_rows(model_series_list)
  # Record end time and calculate duration
  end_time <- Sys.time()
  duration <- end_time - start_time
  message(paste("Completed modeling for functional group:", fun_group, "in", duration, "seconds"))
  return(combined_results)
}

# calculate model performance metrics based on agreement of pre-treatment cover in the reference pixel vs expected cover
calculate_model_performance <- function(combined_model_series, extracted_target, trt_year) {
  model_performance <- combined_model_series %>%
    left_join(extracted_target |> mutate(year = as.numeric(year)), 
              by = c('target_id' = 'ID', 'year.index' = 'year', 'fun_group' = 'functional_group')) |>
    rename(observed_cover = cover) |>
    filter(year.index <= trt_year) |>
    group_by(target_id, fun_group) |>
    summarise(
      nyrs_pretrt = n_distinct(year.index), 
      RMSE = mean(sqrt((point.pred - observed_cover)^2), na.rm = TRUE), 
      RelRMSE = RMSE / mean(observed_cover, na.rm = TRUE), 
      r = cor(observed_cover, point.pred, use = "complete.obs"), 
      point.effect = mean(point.effect, na.rm = TRUE),
      .groups = 'drop'
    )
  return(model_performance)
}

# maps of treatment effects for the years following treatment
# specify functonal group in the format of full_name_cover ie shrub_cover
create_effects_map <- function(functional_group_of_interest) {
  color_ramp <- c("#40004b", "#c2a5cf", "#e7d4e8", "#d9f0d3", "#a6dba0", "#00441b")
  fx_effects_map <- ggplot() +
    geom_spatvector(data = terra::vect(padpoly), fill = 'black') +
    geom_spatvector(data = terra::vect(trt_effects_sf |> filter(fun_group == functional_group_of_interest & year.index > trt_year)),
                    aes(fill = point.effect), color = 'transparent') +
    scale_fill_gradientn(colors = color_ramp, na.value = "transparent",
                         guide = guide_colorbar(title = "Treatment \neffect \n(% cover)")) +
    facet_wrap(~year.index) +
    labs(title = paste0("Trt effects:" ,  functional_group_of_interest)) + 
    theme_void()
  ggsave(plot = fx_effects_map,
         file = paste0(dart_save_base_folder, "ID_", polyID, "_", functional_group_of_interest, "effects_map.png"),
         height = 6, width = 8, units = 'in')
}





##########################################################################################
### END FUNCTIONS
##########################################################################################

### set up parameters for loop and functions
n_cores = 36


# Define the directory containing the DART output RData files
dart_data_folder <- "/90daydata/aeroldc/GH/DART/DART_testing/DART_data/outdir_4/"
dart_save_base_folder <- "/90daydata/aeroldc/GH/DART/DART_testing/RNM_trts/" # where the results will be saved
rdata_files <- list.files(dart_data_folder, pattern = "\\.RData$", full.names = TRUE)

# Extract unique IDs from the filenames
ids <- gsub(".*ID_(\\d+)\\.RData$", "\\1", rdata_files)
unique_ids <- unique(as.numeric(ids))
print(unique_ids)
specific_ids = c(15732)

# Define the IDs you want to loop through
#ids <- c(10, 11)  # Add or modify IDs as needed

# Loop through each ID
for (id in specific_ids) {
  cat("Processing ID:", id, "\n")  # Print the current ID being processed
  # Load the data for the current ID
  load(paste0(dart_data_folder, "ID_", id, ".RData"))
  cat("Data loaded for ID:", id, "\n")
  
  # Extract treatment year and polygon ID
  trt_year <- padpoly |> st_drop_geometry() |> select(trtYear) |> mutate(trtYear = as.numeric(trtYear)) |> pull(trtYear)
  polyID <- padpoly |> st_drop_geometry() |> select(polyID) |> mutate(polyID = as.numeric(polyID)) |> pull(polyID)
  cat("Extracted treatment year and polygon ID for ID:", id, "\n")
  
  # Create and save treatment pixel map
  trt_pixel_map <- ggplot() +
    geom_spatvector(data = terra::vect(padpoly), fill = 'coral1') + geom_spatvector(data = terra::vect(padpixels), fill ='red4') +
    geom_spatvector(data = terra::vect(candidates), fill ='grey') + geom_spatvector(data = terra::vect(chosenCandidates), fill ='cyan3') +
    theme_void()
  ggsave(plot = trt_pixel_map, 
         file = paste0(dart_save_base_folder, "ID_", polyID, "_treatment_base_map.png"), 
         height = 8, width = 8, units = 'in')
  cat("Treatment pixel map saved for ID:", polyID, "\n")
  
  # Update the index naming in the reference extracted data 
  chosenCandidates <- chosenCandidates |>
    rownames_to_column(var = "reference_full_id") %>%
    mutate(extraction_id = row_number()) 
  # Now assign the reference pixel ID to the extraction 
  extractedReference_wide <- extraction$extractedReference |> 
    rename("extracted.pixel.id" = "ID") |>
    left_join(chosenCandidates |> st_drop_geometry() |>
                select(reference_full_id, extraction_id), 
              by = c("extracted.pixel.id" = 'extraction_id')) |>
    select(-extracted.pixel.id) |>
    rename("ID" = "reference_full_id")
  
  # pivot and summarize extracted rap data in target
  temp_extracted_target <- pivot_data(extraction$extractedTarget)
  extracted_target <- summarize_data(temp_extracted_target)
  cat("Summarized target data for ID:", id, "\n")
  
  # pivot and summarize extracted rap data in reference
  temp_extracted_reference <- pivot_data(extractedReference_wide)
  extracted_reference <- summarize_data(temp_extracted_reference)
  cat("Summarized reference data for ID:", id, "\n")
  
  ref_table <- reshape_top_sims(top.sims)
  cat("Reshaped top simulations for ID:", id, "\n")
  
  # Prepare to fit the model for the target pixel
  plan(multisession, workers = n_cores) 
  
  # Define target pixel IDs and unique functional groups
  number_of_target_pixels <- n_distinct(extracted_target$ID)
  target_ids <- 1:number_of_target_pixels  # Update if more target IDs are included
  fun_groups <- unique(extracted_target$functional_group)
  cat("Identified functional groups for ID:", id, "\n")
  
  # Run models in parallel and combine results
  cat("Starting BSTS and CI modeling for ID:", id, "\n")
  combined_model_series_list <- future_lapply(fun_groups, run_model_for_fun_group, future.seed = TRUE)

    # Combine the results 
  combined_model_series <- bind_rows(combined_model_series_list) %>%
    select(-cum.response, -cum.pred, -cum.pred.lower, -cum.pred.upper, -cum.effect, -cum.effect.lower, -cum.effect.upper)
  write.csv(combined_model_series, file = paste0(dart_save_base_folder, "ID_", polyID, "_trt_effects.csv"), row.names = FALSE)
  cat("Completed models run for functional groups for ID:", id, "per target pixel \n")
  
  # Model performance 
  model_performance <- calculate_model_performance(combined_model_series, extracted_target, trt_year)
  write.csv(model_performance, 
            file = paste0(dart_save_base_folder, "ID_", polyID,"_model_performance.csv"), row.names = FALSE)
  cat("Calculated and saved model performance for ID:", id, "\n")
  
  # Prepare model performance as spatial data
  model_performance_sf <- padpixels |>
    mutate(pixel_ID = row_number()) |>
    select(pixel_ID, geometry) |>
    left_join(model_performance |> select(-point.effect), by = c('pixel_ID' = 'target_id'))
  
  # Create model performance map
  model_performance_map <- ggplot() +
    geom_spatvector(data = terra::vect(padpoly), fill = 'grey80') +
    geom_spatvector(data = terra::vect(model_performance_sf |> filter(!is.na(fun_group))),
                    aes(fill = r), color = 'transparent') +
    labs(title = "Model performance",
         subtitle = "Correlation between predicted + observed cover in target pixel pre-trt", fill = "r") +
    facet_wrap(~fun_group) +    theme_void()
  
  # Save model performance map
  ggsave(plot = model_performance_map,
         file = paste0(dart_save_base_folder, "ID_", polyID, "_model_performance_map.png"),
         height = 8, width = 8, units = 'in')
  cat("Saved model performance map for ID:", id, "\n")  
  # Prepare treatment effects spatial data
  trt_effects_sf <- padpixels |>
    mutate(pixel_ID = row_number()) |>
    select(pixel_ID, geometry) |>
    left_join(combined_model_series |> filter(year.index > trt_year) |>select(response, point.effect, year.index, target_id, fun_group),
              by = c('pixel_ID' = 'target_id'))
  create_effects_map("shrub_cover")
  create_effects_map("bare_ground_cover")
  create_effects_map("perennial_forb_and_grass_cover")
  
  cat("Saved map for functionals group for ID:", id, "\n") 
  cat("Results saved for ID:", polyID, "\n\n")  
  # Clean up: remove unnecessary variables to free memory
  rm(trt_year, polyID, trt_pixel_map, chosenCandidates, extractedReference_wide, temp_extracted_target, extracted_target, temp_extracted_reference, extracted_reference, ref_table, fun_groups, combined_model_series_list, combined_model_series, model_performance)
  gc()
}


