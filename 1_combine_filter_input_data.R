# Kristina Young April 23 2025
# Purpose: This script combines the pixel-wise point effect output from DART with assigned treatment objectives, climate data,
#          ecoregion classifications, soil variables, topographic variables, and pre-treatment cover into one data frame in  
#          order to make mixed effects models in another script.
# TODO: 1. add in soil variables
#       2. add in topographic variables
#       3. add in pre-treatment cover
#       4. ensure we want to use ecoregion rather than MLRA
#       5. finish assigning objectives for each land treatment

# R version 4.2.2
library(dplyr) # dplyr_1.1.0
import::from(magrittr, "%>%") # magrittr_2.0.3
# library(tidyr) 

# File Names
out_file <- ('../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data.csv')

dart <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_trt_effects_post_trt_all_RestoreDART.csv', header = T)
objectives <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_reduced_objectives_assigned.csv', header = T)
aridity <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_AI_For_DART_1km.csv', header = T)
spei <- read.csv('../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_SPEI_For_DART_1km.csv', header = T)
preformance <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_model_performance_all_RestoreDART.csv', header = T)
ecoregion <- read.csv("../RestoreDART_DATA/SPATIAL_DATA/RestoreDART_projects_with_ecoregion_info.csv", header = T)

## add in the ecoregion data - below the dart but we could join these data frames in a bunch of locations
dart <- dplyr::left_join(dart,
                         ecoregion |>
                           select(polyID, us_l4code, us_l4name, trtYear) |>
                           rename(PolyID = polyID),
                         by = c('PolyID', 'trtYear'))

# Remove all NAs in OBJECTIVES
Objectives_filtered <- objectives %>%
  filter(!is.na(OBJECTIVE))

unique(Objectives_filtered$combined_TREATMENT_ASSIGNMENT)

# Rename polyID
Objectives_filtered <- Objectives_filtered %>%
  rename(PolyID = polyID_unique)


######## Climate data wrangling
# first, we need to join the spei and aridity data
# reformat both of them from wide to long
spei_long <- spei %>%
  pivot_longer(
    cols = starts_with("X"),  # Select all columns that start with "X"
    names_to = "Year",         # New column name for years
    values_to = "SPEI"         # New column name for SPEI values
  ) %>%
  mutate(Year = as.integer(sub("X", "", Year)))  %>% # Remove "X" and convert to integer
  select(-system.index, -plyID_g) %>%
  rename(PolyID = plyID_n)

aridity_long <- aridity %>%
  pivot_longer(
    cols = starts_with("X"),  # Select all columns that start with "X"
    names_to = "Year",         # New column name for years
    values_to = "Aridity"         # New column name for SPEI values
  ) %>%
  mutate(Year = as.integer(sub("X", "", Year)))  %>% # Remove "X" and convert to integer
  select(-system.index, -plyID_g) %>%
  rename(PolyID = plyID_n)


# join aridity and spei together to form a giant climate dataframe
climate_long <- left_join(aridity_long, spei_long, by = c('ActnDsc', 'trtYear', 'post_fr',
                                                          'PolyID', "dat_src", 'trtID', 'Year'))

### filtering other data sets
# Filter to only the assigned Objectives
dart_filtered <- dart %>%
  filter(PolyID %in% Objectives_filtered$PolyID)
unique(dart_filtered$PolyID)

# Join the datasets
dart_filtered <- dart_filtered %>%
  left_join(Objectives_filtered %>% select(PolyID, OBJECTIVE, combined_TREATMENT_ASSIGNMENT), by = "PolyID")

unique(dart_filtered$OBJECTIVE)

# Look at the number of objectives
dart_filtered %>%
  count(OBJECTIVE) %>%
  arrange(desc(n))


## join the climate and model performance data to dart filtered
dart_filtered_with_climate <- left_join(dart_filtered,
                                       climate_long|>
                                         select(-trtID, -trtYear, -ActnDsc, -post_fr, -dat_src) |>
                                         rename(year.index = Year),
                                       by = c('PolyID', 'year.index'))

# now add in the model performance data
dart_filtered_with_climate_and_performance <- left_join(dart_filtered_with_climate,
                                                       preformance |> select(-nyrs_pretrt, -RMSE, -RelRMSE, -point.effect),
                                                       by = c('PolyID', 'target_id', 'fun_group'))
# now we have r - which is our measure of model fit
# this is the pearsons correlation coefficient between observed and predicted cover BEFORE treatment in the TARGET pixel
# In Joe Smith's paper, they filtered to remove pixels which had poor fit with r<0.5
dart_filtered_with_climate_and_performance_filtered <- dart_filtered_with_climate_and_performance|>
  filter(r>0.5)

# how many pixels does that remove?
nrow(dart_filtered_with_climate_and_performance)
nrow(dart_filtered_with_climate_and_performance_filtered)
1885313 - 1841245
# 44068 pixels were removed
(44068/1885313)*100
# 2.3% of pixels removed because of poor bsts model fit

# Save outputs
write.csv(dart_filtered_with_climate_and_performance_filtered, out_file, row.name = FALSE)