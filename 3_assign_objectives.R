# Kristina Young April 23 2025
# Purpose: 
# TODO: Assign the below objectives: increase_PFG, Decrease_AFG, Increase_SHR, Decrease_SHR, Decrease_TRE

# R version 4.2.2
library(dplyr) # dplyr_1.1.0
library(tidyverse) # tidyverse 2.0.0

# File Names
in_file <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data.csv'

combined_data <- read.csv(in_file)

# OBJECTIVE 1: DECREASE IN TREE COVER
# Use an example of decrease tree cover as the objective and tree cover as 
# the functional group of interest
decrease_TRE <- combined_data %>%
  filter(OBJECTIVE == "decrease_TRE", fun_group == "tree_cover")%>%
  mutate(YearSinceTrt = year.index - trtYear) # Make a new column to look at year since treatment

positive_decrease_TRE <- decrease_TRE %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

# Ensure YearSinceTrt is numeric
positive_decrease_TRE <- positive_decrease_TRE %>%
  mutate(YearSinceTrt = as.numeric(YearSinceTrt))


##### OBJECTIVE 2: INCREASE IN PERENNAIL GRASS AND FORBS
# Use an example of increase in PFG as the objective and PFG cover as 
# the functional group of interest
# georgia replaced dart_filtered with dart_filtered_with_climate_and_performance_filtered
increase_PFG <- dart_filtered_with_climate_and_performance_filtered %>%
  filter(OBJECTIVE == "increase_PFG", fun_group == "annual_forb_and_grass_cover")

# Make a new column to look at year since treatment
increase_PFG <- increase_PFG %>%
  mutate(YearSinceTrt = year.index - trtYear)

positive_increase_PFG <- increase_PFG %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

# Ensure YearSinceTrt is numeric
positive_increase_PFG <- positive_increase_PFG %>%
  mutate(YearSinceTrt = as.numeric(YearSinceTrt))


