# Kristina Young April 23 2025
# Purpose: This script combines the pixel-wise point effect output from DART with assigned treatment
#          objectives, climate data, ecoregion classifications. 
#
# Updated BEM March 2026, refactored for organization but not re-run
#     * replaced magrittr pipe with native pipe for readability
#     * combind redundant pipes
#     * added in 2_combine_filter_input_data.R
#     * added modify_data.R
#     * separated plotting

# TODO:
#     * check if objectives can be updated/brought in
#     * is DART data available? do I need it to recombine the data with the new objectives?
#     * need section that pulls/combines new AI/SPEI data
#     * rename this file to 1_make_input_data.R
#     * update version information on R, dplyr, tidyr

# R version 
library(dplyr)

# File Names and data imports
out_file <- ('../RestoreDART_DATA/MIXED_MODELS/2_combined_filter_input_data.csv')
tx_key_fl <- '../results/tx_key_BEM.csv'

objectives <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_reduced_objectives_assigned.csv', header = T)
aridity <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_AI_For_DART_1km.csv', header = T)
spei <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_SPEI_For_DART_1km.csv', header = T)
preformance <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_model_performance_all_RestoreDART.csv', header = T) |>
  select(-nyrs_pretrt, -RMSE, -RelRMSE, -point.effect)
ecoregion <- read.csv("../RestoreDART_DATA/SPATIAL_DATA/RestoreDART_projects_with_ecoregion_info.csv", header = T) |>
  select(polyID, us_l4code, us_l4name, trtYear) |>
  rename(PolyID = polyID)
dart <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_trt_effects_post_trt_all_RestoreDART.csv', header = T) |>
  left_join(ecoregion, by = c('PolyID', 'trtYear'))
pre_treatment <- read.csv('../RestoreDART_DATA/PRE_TREATMENT/rap_5ybt_summary.csv', header = T) |>
  select(PolyID, target_id, fun_group, mean_cover_5YBT)
tx_key <- read.csv(tx_key_fl)

# Remove all NAs in OBJECTIVES
# Rename polyID
Objectives_filtered <- objectives |>
  filter(!is.na(OBJECTIVE)) |>
  rename(PolyID = polyID_unique) |>
  select(PolyID, OBJECTIVE, combined_TREATMENT_ASSIGNMENT)

unique(Objectives_filtered$combined_TREATMENT_ASSIGNMENT)

######## Climate data wrangling
# first, we need to join the spei and aridity data
# reformat both of them from wide to long
spei_long <- spei |>
  tidyr::pivot_longer(
    cols = starts_with("X"),  # Select all columns that start with "X"
    names_to = "Year",         # New column name for years
    values_to = "SPEI"         # New column name for SPEI values
  ) |>
  mutate(Year = as.integer(sub("X", "", Year)))  |> # Remove "X" and convert to integer
  select(-system.index, -plyID_g) |>
  rename(PolyID = plyID_n)

aridity_long <- aridity |>
  pivot_longer(
    cols = starts_with("X"),  # Select all columns that start with "X"
    names_to = "Year",         # New column name for years
    values_to = "Aridity"         # New column name for SPEI values
  ) |>
  mutate(Year = as.integer(sub("X", "", Year)))  |> # Remove "X" and convert to integer
  select(-system.index, -plyID_g) |>
  rename(PolyID = plyID_n)


# join aridity and spei together to form a giant climate dataframe
climate_long <- left_join(aridity_long, spei_long, by = c('ActnDsc', 'trtYear', 'post_fr', 'PolyID', "dat_src", 'trtID', 'Year')) |>
  select(-trtID, -trtYear, -ActnDsc, -post_fr, -dat_src) |>
  rename(year.index = Year)

### filtering other data sets
# Filter to only the assigned Objectives
dart <- dart |>
  filter(PolyID %in% Objectives_filtered$PolyID) |>
  left_join(Objectives_filtered, by = 'PolyID')

# Look at the number of objectives
dart |>
  count(OBJECTIVE) |>
  arrange(desc(n))

## join the climate and model performance data to dart filtered
dart <- left_join(dart, climate_long, by = c('PolyID', 'year.index')) |>
  # add in model performance data
  left_join(preformance, by = c('PolyID', 'target_id', 'fun_group')) |>
  filter(r>0.5)

# how many pixels will a filter of 0.5 remove?
dart |>
  summarise(total = length(r), good = sum(r > 0.5)) |>
  mutate(perc = round(good / total, 2))
# should be 2.3% for previous objective groupings, unsure for new groupings with updated objectives

# apply filter
# this is the pearsons correlation coefficient between observed and predicted cover BEFORE treatment in the TARGET pixel
# In Joe Smith's paper, they filtered to remove pixels which had poor fit with r<0.5\
dart <- dart_fc |>
  filter(r > 0.5)

# combine with pretreatment
dart <- dart |>
  left_join(combined_data, pre_treatment, by = c("PolyID", "target_id", "fun_group")) |> 
  mutate(YearSinceTrt = year.index - trtYear)

# Joining with soil variables 
#  data not currently accessible as of 3/13/2026

# Joining with SPEI/AI
#  data needs to be re-pulled for years 2021-2024

# Save outputs (update file name when additional variables are added)
write.csv(dart, out_file, row.names = F)
rm(dart)

# add BEM script
# below can be tested without DART data available
dart0 <- read.csv(out_file)

# drop redundant data and rename
dart0 <- dart0 |>
  select(-response, -point.pred, -point.pred-lower, -point.pred.upper, -us_l4code, -YearsSinceTrt)

### EDITING HERE, NEED COLUMN NAMES FROM DESKTOP
### USE TIDY DIALECT
colnames(d0) <- c(
  'effect', 'lower', 'upper', 'year', 'pixel', 'var', 'treatment_year', 'polygon', 'years_since_treatment', 
  'eco', 'objective', 'treatment', 'aridity', 'spei', 'r', 'mean_cover_5YBT'
)

# data needs to be complete
lapply(d0, \(xx) table(is.na(xx)))
# ~ 20% of data missing from aridity and SPEI
d0 <- d0[complete.cases(d0), ]
# still 1.5 million rows

# drop litter
d0 <- d0[-which(d0$var == 'litter_cover'), ]

d0$treatment <- tolower(d0$treatment)
# tx_key is missing a key for 'vegetation removal', which should just be 'vegetation removal'
tx_key[nrow(tx_key) + 1, 'tx_fine'] <- 'vegetation removal'
tx_key[nrow(tx_key), 'tx_coarse'] <- 'vegetation removal'
stopifnot(all(d0$treatment %in% tx_key$tx_fine))
colnames(d0)[which(colnames(d0) == 'treatment')] <- 'tx_fine'
d0 <- dplyr::left_join(d0, within(tx_key, rm(npoly_coarse, npix_coarse)), by = 'tx_fine')

# how many unique methods?
# should just be 4
v1 <- unique(d0$tx_coarse)
v1 <- strsplit(v1, ';')
v1 <- unlist(v1)
v1 <- unique(v1)

# condense objectives into 5 separate models
d1 <- d0[grepl('increase_PFG', d0$objective), ]
d1 <- d1[grepl('perennial_forb_and_grass_cover', d1$var), ]
d1 <- d1[which(d1$years_since_treatment > 0), ]
# also decrease_AFG, increase_SHR, decrease_SHR, decrease_TRE
# for combination objectives, how do we decide which go into their own groups?

# each df has a landscape response to restoration for a set of pixels and years within polygons

# write output file