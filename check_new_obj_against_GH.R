library(dplyr)

dart <- read.csv('../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_04052026.csv')
dart_test <- read.csv('../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_GH.csv')

objectives_filtered <- readxl::read_excel("../RestoreDART_DATA/objectives/MARCH2025_OBJECTIVES_04052026.xlsx") |>
  filter(!is.na(OBJECTIVE)) |>
  rename(PolyID = polyID_unique) |>
  select(PolyID, OBJECTIVE, combined_TREATMENT_ASSIGNMENT)

# extra test dataset from previous data
dart_test <- dart_test |>
  select(
    point.effect, point.effect.lower, point.effect.upper, year.index, trtYear, target_id,
    fun_group, PolyID, us_l4code, OBJECTIVE, combined_TREATMENT_ASSIGNMENT, r
  ) |>
  rename('objective_test' = OBJECTIVE, 'tx_comb_test' = combined_TREATMENT_ASSIGNMENT) |>
  left_join(Objectives_filtered, by = 'PolyID') |>
  rename('year_RAP' = year.index, 'year_tx' = trtYear, 'polygon' = PolyID, 'pixel' = target_id,
         'effect' = point.effect, 'lower' = point.effect.lower, 'upper' = point.effect.upper,
         'objective' = OBJECTIVE, 'tx_comb' = combined_TREATMENT_ASSIGNMENT)

# objectives mis-matched, check specific objectives
dart_obj_test <- dart_test |>
  filter_out(objective == objective_test) |>
  mutate(objective_PFG = grepl('increase_PFG', objective)) |>
  mutate(objective_PFG_test = grepl('increase_PFG', objective_test)) |>
  filter_out(objective_PFG == objective_PFG_test)

stopifnot(
  identical(dart_test$tx_comb, dart_test$tx_comb_test),
  identical(dart_test$objective, dart_test$objective_test)
)