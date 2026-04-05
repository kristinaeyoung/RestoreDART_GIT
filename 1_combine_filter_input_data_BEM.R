# Kristina Young April 23 2025
# Purpose: This script combines the pixel-wise point effect output from DART with assigned treatment
#          objectives, climate data, ecoregion classifications. 
#
# Updated BEM March 2026, refactored for organization but not re-run
#     * replaced magrittr pipe with native pipe for readability
#     * combind redundant pipes
#     * added in missing AI/SPEI data
#     * added missing DART data

# TODO:
#     * rename this file to 1_make_input_data.R
#     * update version information on R, dplyr, tidyr
#     * AI/SPEI script only getting 2/3 of the 2021-2024 data

# R version 
library(dplyr)

out_file <- '../RestoreDART_DATA/MIXED_MODELS/xx_combined_filter_input_data.csv'
tx_key_fl <- '../results/tx_key_BEM.csv'

# import
tx_key <- read.csv(tx_key_fl)
new_ai <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/AI_21_24.csv') |>
  select(-ActnDsc, -trtYear, -post_fr, -dat_src, -trtID)
new_spei <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/SPEI_21_24.csv') |>
  select(-ActnDsc, -trtYear, -post_fr, -dat_src, -trtID)
objectives <- readxl::read_excel("../RestoreDART_DATA/objectives/MARCH2025_OBJECTIVES_27032026.xlsx")
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
  select(polyID, us_l4code, us_l4name, trtYear) |>
  rename(PolyID = polyID)
dart <- read.csv('../RestoreDART_DATA/DART_combined_BEM_04032026.csv')
pre_treatment <- read.csv('../RestoreDART_DATA/PRE_TREATMENT/rap_5ybt_summary.csv', header = T) |>
  select(PolyID, target_id, fun_group, mean_cover_5YBT)
soil <- read.csv('../RestoreDART_DATA/MIXED_MODELS/xx_soil_df.csv') |>
  distinct()

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
  left_join(preformance, by = c('PolyID', 'target_id', 'fun_group')) |>
  left_join(pre_treatment, by = c("PolyID", "target_id", "fun_group")) |>
  mutate(PolyID = as.integer(PolyID)) |>
  left_join(climate_long, by = c('PolyID', 'year.index')) |>
  mutate(YearSinceTrt = year.index - trtYear) |>
  left_join(soil, by = c('PolyID', 'target_id'))

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


#d0$treatment <- tolower(d0$treatment)
# tx_key is missing a key for 'vegetation removal', which should just be 'vegetation removal'
#tx_key[nrow(tx_key) + 1, 'tx_fine'] <- 'vegetation removal'
#tx_key[nrow(tx_key), 'tx_coarse'] <- 'vegetation removal'
#stopifnot(all(d0$treatment %in% tx_key$tx_fine))
#colnames(d0)[which(colnames(d0) == 'treatment')] <- 'tx_fine'
#d0 <- dplyr::left_join(d0, within(tx_key, rm(npoly_coarse, npix_coarse)), by = 'tx_fine')

# how many unique methods?
# should just be 4
#v1 <- unique(d0$tx_coarse)
#v1 <- strsplit(v1, ';')
#v1 <- unlist(v1)
#v1 <- unique(v1)

# condense objectives into 5 separate models
#d1 <- d0[grepl('increase_PFG', d0$objective), ]
#d1 <- d1[grepl('perennial_forb_and_grass_cover', d1$var), ]
#d1 <- d1[which(d1$years_since_treatment > 0), ]
# also decrease_AFG, increase_SHR, decrease_SHR, decrease_TRE
# for combination objectives, how do we decide which go into their own groups?

# each df has a landscape response to restoration for a set of pixels and years within polygons

# write output file