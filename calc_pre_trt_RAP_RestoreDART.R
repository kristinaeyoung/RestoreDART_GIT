#dart <- read.csv('/project/aeroldc/RestoreDART/DART_running/trt_effects_post_trt_all_RestoreDART.csv', header = T)
# in this case the dart df is the same as trt_effects 
# trt_effects = dart

# Filter to only 5 years before treatment (i.e., year.index between -6 and -1)
trt_effects_filtered <- trt_effects |> 
  mutate(years_since_treatment = year.index - trtYear)|>
  filter(years_since_treatment >= -6 & years_since_treatment <= -1)
head(trt_effects_filtered)

# Summarize mean cover for each target_id and group
rap_5ybt_summary <- trt_effects_filtered |>
  group_by(PolyID, target_id, fun_group ) |>
  summarise(mean_cover_5YBT = mean(response, na.rm = TRUE), .groups = "drop")

head(rap_5ybt_summary)
## now this can be joined with the other datasets for modeling 