library(dplyr)

source('plot_functions.R')

fig_dir <- '../results/figures'

dart <- read.csv('../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_04052026.csv')
dart_test_fl <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_GH.csv'
tx_key <- read.csv('../results/tx_key_BEM.csv')

objectives_filtered <- readxl::read_excel("../RestoreDART_DATA/objectives/MARCH2025_OBJECTIVES_04052026.xlsx") |>
  filter(!is.na(OBJECTIVE)) |>
  rename(PolyID = polyID_unique) |>
  select(PolyID, OBJECTIVE, combined_TREATMENT_ASSIGNMENT)

# extra test dataset from previous data
dart_test <- dart_test_fl |>
  read.csv() |>
  select(
    point.effect, point.effect.lower, point.effect.upper, year.index, trtYear, target_id,
    fun_group, PolyID, us_l4name, OBJECTIVE, combined_TREATMENT_ASSIGNMENT, r
  ) |>
  rename('objective_test' = OBJECTIVE, 'tx_comb_test' = combined_TREATMENT_ASSIGNMENT) |>
  left_join(objectives_filtered, by = 'PolyID') |>
  rename('year_RAP' = year.index, 'year_tx' = trtYear, 'polygon' = PolyID, 'pixel' = target_id,
         'effect' = point.effect, 'lower' = point.effect.lower, 'upper' = point.effect.upper,
         'objective' = OBJECTIVE, 'tx_comb' = combined_TREATMENT_ASSIGNMENT) |>
  mutate(year_diff = year_RAP - year_tx) |>
  filter_out(fun_group == 'litter_cover') |>
  mutate(fun_group = gsub('_cover', '', fun_group)) |>
  mutate(fun_group = gsub('annual_forb_and_grass', 'AFG', fun_group)) |>
  mutate(fun_group = gsub('perennial_forb_and_grass', 'PFG', fun_group)) |>
  mutate(fun_group = gsub('bare_ground', 'bare', fun_group)) |>
  mutate(tx_comb = tolower(tx_comb)) |>
  mutate(tx_comb = gsub('^greenstrip$', 'greenstrip_ground seeding', tx_comb)) |>
  mutate(tx_comb = gsub('^vegetation removal$', 'vegetation removal_manual', tx_comb)) |>
  rename('tx_fine' = tx_comb) |>
  left_join(within(tx_key, rm(npoly_coarse, npix_coarse)), by = 'tx_fine') |>
  mutate(sig = lower > 0 | upper < 0)

stopifnot(
  all(dart_test$objective %in% dart$objective),
  identical(dart_test$tx_comb, dart_test$tx_comb_test),
  identical(dart_test$objective, dart_test$objective_test)
)

unique(trimws(unlist(strsplit(unique(dart_test$objective), ','))))

check_objective <- function(df, objective_string) {
  
  cat('\nstring: ', objective_string, '\n')
  print('grepl nrow:')
  df |>
    filter_out(objective == objective_test) |>
    mutate(objective_flag = grepl(objective_string, objective)) |>
    mutate(objective_flag_test = grepl(objective_string, objective_test)) |>
    filter_out(objective_flag == objective_flag_test) |>
    nrow() |>
    print()
  
  print('ifelse nrow:')
  df |>
    filter_out(objective == objective_test) |>
    mutate(objective_flag = ifelse(objective == objective_string, T, F)) |>
    mutate(objective_flag_test = ifelse(objective_test == objective_string, T, F)) |>
    filter_out(objective_flag == objective_flag_test) |>
    nrow() |>
    print()
}

check_objective(dart_test, 'increase_PFG')
check_objective(dart_test, 'decrease_TRE')
check_objective(dart_test, 'increase_SHR')
check_objective(dart_test, 'decrease_AFG')
check_objective(dart_test, 'decrease_SHR')

# big mismatches in decrease_AFG, decrease_SHR, but not others
d_PFG <- dart |>
  filter(grepl('increase_PFG', objective)) |>
  filter(grepl('PFG', fun_group)) |>
  filter(year_diff > 0)

d_PFG_test <- dart_test |>
  filter(grepl('increase_PFG', objective)) |>
  filter(grepl('PFG', fun_group)) |>
  filter(year_diff > 0)

fig1 <- fig_1(d_PFG)
fig2 <- fig_1(d_PFG_test)

png(file.path(fig_dir, 'fig_1_test_prev.png'), 60, 12, 'in', res = 150)
fig1
dev.off()
png(file.path(fig_dir, 'fig_1_test_GH.png'), 20, 5, 'in', res = 150)
fig2
dev.off()

# i think it might have been this line:
#mutate(year_diff = year_RAP - year_tx)
# it was previously year_tx - year_RAP, which gives negatives
