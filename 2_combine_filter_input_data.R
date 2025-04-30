# Kristina Young April 23 2025
# Purpose: This script builds from 1_combine_filter_input_data.R to add in pre-treatment data, soil variables, and topographical variables to create an output dataframe to use for modeling in another script.
# TODO: 1. add in soil variables
#       2. add in topographic variables
#       3. finish assigning objectives for each land treatment

# R version 4.2.2
library(dplyr) # dplyr_1.1.0
library(tidyverse) # tidyverse 2.0.0
import::from(magrittr, "%>%") # magrittr_2.0.3

# File Names
in_file <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data.csv'

combined_data <- read.csv(in_file)

# File Names
out_file <- ('../RestoreDART_DATA/MIXED_MODELS/2_combined_filter_input_data.csv')

# Reading in data files
pre_treatment <- read.csv('../RestoreDART_DATA/PRE_TREATMENT/rap_5ybt_summary.csv', header = T)

# Joining dataframe with pretreatment data
combined_with_pretreatment <- left_join(
  combined_data,
  pre_treatment |> select(PolyID, target_id, fun_group, mean_cover_5YBT),
  by = c("PolyID", "target_id", "fun_group")
)

# Joining with soil variables 
#  WAITING FOR THIS DATA

# Joining with environmental variables
#  WAITING ON THIS DATA

# Save outputs (update file name when additional variables are added)
write.csv(combined_with_pretreatment, out_file, row.names = FALSE)
