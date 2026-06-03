
### RestoreDART

# The scripts for Restore DART are organized as follows:

## Miscellaneous scripts

# These scripts should be run before the main analysis

# /DART_scripts_09032026
#     This is a folder of DART processing scripts provided by GH. I used them to figure out how to pull the DART soils
#     from the DART data objects and kept them here for reference/backup.

# pull_DART_soils.R
#     This script processes the DART data in order to pull soils data for further modelling. It's a very finicky script
#     and requires the first draft analysis data created by KY: 1_combined_filter_input_data.csv. It creates
#     two data products necessary for the rest of the analysis, xx_soil_df.csv and xx_coord_df.csv. It also does the R
#     data file unpacking for collate_DART.R

# collate_DART.R
#     The DART data was provided in .csv form with a single file for every input polygon ran through DART. There were
#     1903 files total. This script combines them into a DART_combined_BEM_[Date].csv file. It requires an unpacking
#     process from pull_DART_soils.R

# make_tx_key.R
#     This script uses count_by_trt_methods.csv (provided bY GH) to create a 'treatment key' that gives a coarse and fine
#     treatment identification to each polygon. It also splits the treatments into logical groupings, which might be
#     useful for models later.

## Main processing files

# 1_combine_filter_input_data.R
#     This is the main pre-processing script. It is a refactor/update of KY and GH's original script of the same name. The
#     objective is to bring together the DART, objective, treatment, climate, and soil data into one data frame for plotting
#     and modelling. The output, 1_combined_filter_input_data_[Date].csv, should be the input for all plotting/resulting/
#     modelling efforts.

# 2_make_DART_results.R
#     This file creates the results for the main body of the work - the DART results. It should include both total results
#     as well as any figures. The first goal of the DART results section is to give something to GH and KY so that they
#     can talk about overall narrative structure.

## Helper files

# plot_functions.R
#     Plot code can be shoved into functions to make the plotting scripts easier to read. It should go here.

# helper_functions.R
#     Other helpful functions should go here.
