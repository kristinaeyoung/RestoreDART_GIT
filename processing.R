library(dplyr)
library(tidyr)
# remove.packages("lme4")
# db <- available.packages(repos = "https://cran.r-project.org/")
# tools::package_dependencies("Matrix", db = db, which = "LinkingTo", reverse = TRUE)

# oo <- options(repos = "https://cran.r-project.org/")
# install.packages("Matrix")
# install.packages("lme4")
# options(oo)

dart <- read.csv('/project/aeroldc/RestoreDART/DART_running/trt_effects_post_trt_all_RestoreDART.csv', header = T)
objectives <- read.csv('/project/aeroldc/RestoreDART/DART_running/reduced_objectives_assigned.csv', header = T)
aridity <- read.csv('/project/aeroldc/RestoreDART/climate_data/Annual_Mean_AI_For_DART_1km.csv', header = T)
spei <- read.csv('/project/aeroldc/RestoreDART/climate_data/Annual_Mean_SPEI_For_DART_1km.csv', header = T)
preformance = read.csv('/project/aeroldc/RestoreDART/DART_running/model_performance_all_RestoreDART.csv', header = T)


## read in the ecoregion info
## this has continental US ecoregions levels 1-4 for each polygon. For each polygon, only the dominant (largest area) level 4 ecoregion was selected.
ecoregion = read.csv("/project/aeroldc/RestoreDART/combined_spatial_data/RestoreDART_projects_with_ecoregion_info.csv", header = T)

## add in the ecoregion data - below the dart but we could join these dataframes in a bunch of locations
dart = left_join(dart,
                 ecoregion |>
                   select(polyID, us_l4code, us_l4name, trtYear)|>
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
dart_filtered_with_climate = left_join(dart_filtered,
                                       climate_long|>
                                         select(-trtID, -trtYear, -ActnDsc, -post_fr, -dat_src) |>
                                         rename(year.index = Year),
                                       by = c('PolyID', 'year.index'))

# now add in the model performance data
dart_filtered_with_climate_and_performance = left_join(dart_filtered_with_climate,
                                                       preformance |> select(-nyrs_pretrt, -RMSE, -RelRMSE, -point.effect),
                                                       by = c('PolyID', 'target_id', 'fun_group'))
# now we have r - which is our measure of model fit
# this is the pearsons correlation coefficient between observed and predicted cover BEFORE treatment in the TARGET pixel
# In Joe Smith's paper, they filtered to remove pixels which had poor fit with r<0.5
dart_filtered_with_climate_and_performance_filtered= dart_filtered_with_climate_and_performance|>
  filter(r>0.5)

# how many pixels does that remove?
nrow(dart_filtered_with_climate_and_performance)
nrow(dart_filtered_with_climate_and_performance_filtered)
1885313 - 1841245
# 44068 pixels were removed
(44068/1885313)*100
# 2.3% of pixels removed because of poor bsts model fit


##### OBJECTIVE 1: DECREASE IN TREE COVER
# Use an example of decrease tree cover as the objective and tree cover as
# the functional group of interest
decrease_TRE <- dart_filtered_with_climate_and_performance_filtered %>%
  filter(OBJECTIVE == "decrease_TRE", fun_group == "tree_cover")%>%
  mutate(YearSinceTrt = year.index - trtYear) # Make a new column to look at year since treatment

positive_decrease_TRE <- decrease_TRE %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

# Ensure YearSinceTrt is numeric
positive_decrease_TRE <- positive_decrease_TRE %>%
  mutate(YearSinceTrt = as.numeric(YearSinceTrt))
