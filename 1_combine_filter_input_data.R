# Kristina Young April 23 2025
# Purpose: This script combines the pixel-wise point effect output from DART with assigned treatment
#          objectives, climate data, ecoregion classifications. 
#
# Updated BEM March 2026, refactored for organization but not re-run
#     * replaced magrittr pipe with native pipe for readability
#     * combind redundant pipes
#     * added in missing AI/SPEI data
#     * added missing DART data

# Assumptions/Considerations:
#     * 'greenstrip' is the same as'greenstrip_ground seeding'
#     * 'vegetation removal' is the same as 'vegetation removal_manual'
#     * 'unknown' treatment dropped, removed 9 polygons and 140 pixels
#     * filtered by r > 0.5, removed 1% of total observations
#     * filtered by missing soil/climate, removed 2.9% of total data

# TODO:
#     * update version information on R, dplyr, tidyr

# R version 
library(dplyr)

out_file <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_04052026.csv'
tx_key_fl <- '../results/tx_key_BEM.csv'

# import
tx_key <- read.csv(tx_key_fl)
new_ai <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/AI_21_24.csv') |>
  select(-ActnDsc, -trtYear, -post_fr, -dat_src, -trtID)
new_spei <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/SPEI_21_24.csv') |>
  select(-ActnDsc, -trtYear, -post_fr, -dat_src, -trtID)
objectives <- readxl::read_excel("../RestoreDART_DATA/objectives/MARCH2025_OBJECTIVES_04052026.xlsx")
aridity_long <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_AI_For_DART_1km.csv', header = T) |>
  left_join(new_ai) |>
  tidyr::pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",         
    values_to = "Aridity"        
  ) |>
  mutate(Year = as.integer(sub("X", "", Year)))  |> 
  select(-system.index, -plyID_g) |>
  rename(PolyID = plyID_n)
spei_long <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_SPEI_For_DART_1km.csv', header = T) |>
  left_join(new_spei) |>
  tidyr::pivot_longer(
    cols = starts_with("X"),  
    names_to = "Year",         
    values_to = "SPEI"         
  ) |>
  mutate(Year = as.integer(sub("X", "", Year)))  |> 
  select(-system.index, -plyID_g) |>
  rename(PolyID = plyID_n)
preformance <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_model_performance_all_RestoreDART.csv', header = T) |>
  select(-nyrs_pretrt, -RMSE, -RelRMSE, -point.effect)
ecoregion <- read.csv("../RestoreDART_DATA/SPATIAL_DATA/RestoreDART_projects_with_ecoregion_info.csv", header = T) |>
  select(polyID, us_l4code, us_l4name) |>
  rename(PolyID = polyID)
dart <- read.csv('../RestoreDART_DATA/MIXED_MODELS/DART_combined_BEM_04032026.csv')
pre_treatment <- read.csv('../RestoreDART_DATA/PRE_TREATMENT/rap_5ybt_summary.csv', header = T) |>
  select(PolyID, target_id, fun_group, mean_cover_5YBT)
soil <- read.csv('../RestoreDART_DATA/MIXED_MODELS/xx_soil_df.csv') |>
  distinct()
coords <- read.csv("../RestoreDART_DATA/MIXED_MODELS/xx_coord_df.csv") |>
  rename('PolyID' = polygon, 'target_id' = SOLIS_pixel) |>
  select(-ID, -RAP_pixel) |>
  group_by(PolyID, target_id) |>
  distinct() |>
  summarise(X = round(mean(X)), Y = round(mean(Y)), .groups = 'drop')

# modify
climate_long <- left_join(aridity_long, spei_long, by = c('ActnDsc', 'trtYear', 'post_fr', 'PolyID', "dat_src", 'trtID', 'Year')) |>
  select(-trtID, -trtYear, -ActnDsc, -post_fr, -dat_src) |>
  rename(year.index = Year) |>
  filter(year.index %in% dart$year.index)

Objectives_filtered <- objectives |>
  filter(!is.na(OBJECTIVE)) |>
  rename(PolyID = polyID_unique) |>
  select(PolyID, OBJECTIVE, combined_TREATMENT_ASSIGNMENT)
unique(Objectives_filtered$combined_TREATMENT_ASSIGNMENT)

dart <- dart |>
  filter(PolyID %in% Objectives_filtered$PolyID) |>
  left_join(Objectives_filtered, by = 'PolyID') |>
  left_join(ecoregion, by = 'PolyID') |>
  left_join(preformance, by = c('PolyID', 'target_id', 'fun_group')) |>
  left_join(pre_treatment, by = c("PolyID", "target_id", "fun_group")) |>
  mutate(PolyID = as.integer(PolyID)) |>
  left_join(climate_long, by = c('PolyID', 'year.index')) |>
  mutate(YearSinceTrt = year.index - trtYear) |>
  left_join(soil, by = c('PolyID', 'target_id')) |>
  left_join(coords, by = c('PolyID', 'target_id'))

# check
dart |>
  count(OBJECTIVE) |>
  arrange(desc(n))
# how many pixels will a filter of 0.5 remove?
dart |>
  summarise(total = length(r), good = sum(r > 0.5)) |>
  mutate(perc = round(good / total, 2))
# 2.3% for preliminary objective groupings
# 1.0% for n PolyID = 941, nrow = 15,983,604
dart <- dart |>
  filter(r > 0.5)
# nrow = 15,663,219

# data needs to be complete
lapply(dart, \(xx) table(is.na(xx)))
# 22776 soil, 424420 AI/SPEI missing
dart <- dart |>
  tidyr::drop_na()
# 15,216,023, 2.9% reduction in total data, but lots missing from 2021-2024

# rename columns
dat <- dart |>
  select(-response, -point.pred, -point.pred.lower, -point.pred.upper, -YearSinceTrt, -r) |>
  select(-PLANC, -PROFC, -SWI, -RELELEV02, -RELELEV16, -RELELEV32, -MODGMRPH, 
         -VALLEDEP, -NTOPOPEN, -PTOPOPEN, -VDCN, -DVMNED, -TPI, -eastness, -southness,
         -fragVol, -resdep, -clay, -CAlog_10) |>
  rename('year_RAP' = year.index, 'year_tx' = trtYear, 'polygon' = PolyID, 'pixel' = target_id,
         'effect' = point.effect, 'lower' = point.effect.lower, 'upper' = point.effect.upper,
         'objective' = OBJECTIVE, 'tx_comb' = combined_TREATMENT_ASSIGNMENT,
         'aridity' = Aridity, 'spei' = SPEI) |>
  filter_out(fun_group == 'litter_cover') |>
  mutate(fun_group = gsub('_cover', '', fun_group)) |>
  mutate(fun_group = gsub('annual_forb_and_grass', 'AFG', fun_group)) |>
  mutate(fun_group = gsub('perennial_forb_and_grass', 'PFG', fun_group)) |>
  mutate(fun_group = gsub('bare_ground', 'bare', fun_group)) |>
  mutate(tx_comb = tolower(tx_comb)) |>
  # tx_comb which are not in tx_key:
  # "unknown"            "vegetation removal" "greenstrip"
  # drop 'unknown', removes 9 polygons and 140 pixels
  filter_out(tx_comb == 'unknown') |>
  # assume 'greenstrip' is the same as'greenstrip_ground seeding'
  mutate(tx_comb = gsub('^greenstrip$', 'greenstrip_ground seeding', tx_comb)) |>
  # assume 'vegetation removal' is the same as 'vegetation removal_manual'
  mutate(tx_comb = gsub('^vegetation removal$', 'vegetation removal_manual', tx_comb)) |>
  # year_diff should be positive when RAP measurement is post-treatment
  mutate(year_diff = year_RAP - year_tx) |>
  mutate(sig = lower > 0 | upper < 0)
  #mutate(subj = paste(polygon, pixel, sep = '_'))

stopifnot(all(dat$tx_comb %in% tx_key$tx_fine))

dat <- dat |>
  rename('tx_fine' = tx_comb) |>
  left_join(within(tx_key, rm(npoly_coarse, npix_coarse)), by = 'tx_fine')

# how many unique methods?
# should just be 4
unique(dat$tx_coarse) |>
  strsplit(';') |>
  unlist() |>
  unique()

# what % of data is significant?
round((table(dat$sig) / length(dat$sig)) * 100, 2)

# re-order columns
dat <- dat |>
  select(
    effect, lower, upper, sig, polygon, us_l4name, us_l4code, pixel, X, Y, fun_group, objective, year_RAP, year_tx, year_diff,
    aridity, spei, ELEVm, SLOPE, MELTON, soilec, sand, silt, mean_cover_5YBT, 
    tx_fine, tx_coarse, prescribed_burn, seeding, soil_disturbance, vegetation_removal
  )

# write output file
write.csv(dat, out_file, row.names = F)
