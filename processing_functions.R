# library(dplyr)
# library(tidyr)
# library(tibble)
# library(readr)
# library(bsts)
# library(CausalImpact)
# library(zoo)
# library(sf)
# library(future)
# library(future.apply)
# 
pivot_data <- function(extracted_data) {
  extracted_data |>
    tidyr::pivot_longer(
      cols = -c(ID, x, y),
      names_to = c("functional_group", "year"),
      names_pattern = "(.*?)_(\\d{4})_v\\d+",
      values_to = "cover_value"
    ) |>
    dplyr::select(ID, x, y, functional_group, year, cover_value)
}
# 
summarize_data <- function(pivoted_data) {
  pivoted_data |>
    dplyr::group_by(ID, functional_group, year) |>
    dplyr::summarize(cover = mean(cover_value, na.rm = TRUE), .groups = 'drop')
}
# 
reshape_top_sims <- function(top_sims) {
  ref_table <- top_sims$index %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(target = dplyr::row_number()) %>%
    dplyr::select(target, dplyr::everything()) %>%
    tidyr::pivot_longer(dplyr::starts_with('V'), names_to = 'refnumber', values_to = 'reference') %>%
    dplyr::select(-refnumber)
  return(ref_table)
}
# 
# prep_ts <- function(target_id, fun_group, treat_year) {
#   years <- extracted_target %>%
#     dplyr::filter(ID == target_id & functional_group == fun_group) %>%
#     dplyr::pull(year) %>%
#     sort()
#   if (length(years) == 0) {
#     warning(paste("No years found for target_id:", target_id, "and functional_group:", fun_group))
#     return(NULL)
#   }
#   t0 <- min(years)
#   tL <- max(years)
#   y <- seq(t0, tL)
#   ref <- ref_table %>%
#     dplyr::filter(target == target_id) %>%
#     dplyr::pull(reference)
#   Xtrain <-
#     extracted_reference %>%
#     dplyr::filter(ID %in% ref & functional_group == fun_group) %>%
#     tidyr::pivot_wider(
#       names_from = ID,
#       values_from = cover,
#       names_prefix = 'X'
#     ) %>%
#     dplyr::filter(year %in% y) %>%
#     dplyr::arrange(year) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(dplyr::contains('X'))
#   if (nrow(Xtrain) == 0 || any(is.na(Xtrain))) {
#     warning(paste("Missing or incomplete time series data for target_id:", target_id, "and functional_group:", fun_group))
#     return(NULL)
#   }
#   Xtrain <- ts(Xtrain, start = t0, end = tL)
#   Y <-
#     extracted_target %>%
#     dplyr::filter(ID == target_id & functional_group == fun_group) %>%
#     dplyr::filter(year %in% y) %>%
#     dplyr::arrange(year) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(cover)
#   if (nrow(Y) == 0 || any(is.na(Y$cover))) {
#     warning(paste("Missing or incomplete cover data for target_id:", target_id, "and functional_group:", fun_group))
#     return(NULL)
#   }
#   Y <- ts(Y$cover, start = t0, end = tL)
#   Ytrain <- Y
#   Ytrain[which(y >= treat_year)] <- NA
#   Ypost <- Y[which(y >= treat_year)]
#   return(list(Ytrain = Ytrain, Xtrain = Xtrain, Ypost = Ypost))
# }
# 
# fit_model <- function(ts_data, iter) {
#   Ytrain <- ts_data$Ytrain
#   Xtrain <- ts_data$Xtrain
#   Ypost <- ts_data$Ypost
#   impact <- tryCatch({
#     ss.G <- bsts::AddLocalLevel(list(), Ytrain)
#     mod.G <- bsts::bsts(
#       Ytrain ~ Xtrain,
#       state.specification = ss.G,
#       niter = iter,
#       ping = 0
#     )
#     CausalImpact::CausalImpact(bsts.model = mod.G, post.period.response = Ypost)
#   }, error = function(e) e)
#   return(impact)
# }
# 
# run_model_for_fun_group <- function(fun_group) {
#   message(paste("Starting modeling for functional group:", fun_group))
#   start_time <- Sys.time()
#   target_id_chunks <- split(target_ids, ceiling(seq_along(target_ids) / ceiling(length(target_ids) / n_cores)))
#   model_series_list <- future.apply::future_lapply(target_id_chunks, function(target_id_chunk) {
#     chunk_results <- list()
#     for (target_id in target_id_chunk) {
#       ts_data <- prep_ts(target_id = target_id, fun_group = fun_group, treat_year = trt_year)
#       test.model <- fit_model(ts_data = ts_data, iter = 2000)
#       if (!inherits(test.model, "error")) {
#         model_series <- as.data.frame(test.model$series)
#         # model_series$year.index <- (zoo::index(test.model$series)) + 1985
#         model_series$target_id <- target_id
#         model_series$fun_group <- fun_group
#         model_series$trtYear <- trt_year
#         model_series$PolyID <- polyID
#         chunk_results[[length(chunk_results) + 1]] <- model_series
#       } else {
#         warning(paste("Model fitting failed for target_id:", target_id, "and functional group:", fun_group))
#       }
#     }
#     dplyr::bind_rows(chunk_results)
#   }, future.seed = TRUE)
#   combined_results <- dplyr::bind_rows(model_series_list)
#   end_time <- Sys.time()
#   duration <- end_time - start_time
#   message(paste("Completed modeling for functional group:", fun_group, "in", duration, "seconds"))
#   return(combined_results)
# }
