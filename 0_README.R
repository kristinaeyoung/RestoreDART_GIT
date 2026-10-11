
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

# 2_results_setup.R
#     Everything the results documents show is computed here: the constants, the nine-column read of the input file,
#     the objective split, the three sample-size filters, and each objective's copy of the nine compiled tables. The
#     documents display and compute nothing. 3_make_linear_models.R sources it too, so the models are fit to
#     demonstrably the same rows the report describes. Renamed from 0_DART_setup.R on 07 Oct 2026.

# 2_make_DART_results.Rmd
#     This file creates the results for the main body of the work - the DART results. It should include both total results
#     as well as any figures. The first goal of the DART results section is to give something to GH and KY so that they
#     can talk about overall narrative structure.

# 2_sample_size_checks.Rmd
#     Per-objective detail: five static sections carrying the sample-size and filtering material, the significance
#     decompositions, and each objective's own hand-written interpretation.

# 2_raw_result_graphs.Rmd
#     Reference only - the per-ecoregion bar panels. Split out because it is bulky.

# 2_interpret_DART_results.Rmd
#     The interpretation document, added 10 Oct 2026 in response to KY's analysis_modification_suggestions_08102026.
#     Where 2_make_DART_results.Rmd is organized by the quantity computed, this one is organized by the four biological
#     findings those quantities support, and is the document the manuscript Results are written from. It displays only,
#     and reads the same objects from 2_results_setup.R.

# 3_make_linear_models.R / 3_report_lme4_models.Rmd
#     The pooled model stage: an lme4 ladder per objective, fit and written by the script, displayed by the document.
#     Same compute-in-script seam as the 2_* files.

## Where output goes

# All generated output goes to ONE date-stamped directory per run,
# ../analysis_outputs/RestoreDART_run_<run_stamp>/, named from `run_stamp` in 2_results_setup.R. Both pipeline stages
# write there - the compiled .csv files and the model stage's tables, fits and figures - and the rendered .html is
# copied in beside them. See ../analysis_outputs/0_README_analysis_outputs.txt for the layout.

# Render a document from the repository root, so that the ../analysis_inputs and ../analysis_outputs paths resolve,
# and send the .html to the run directory as well as leaving one beside the .Rmd:
#
#   Rscript -e "rmarkdown::render('2_interpret_DART_results.Rmd',
#                output_format = 'html_document',
#                output_dir = file.path('../analysis_outputs', 'RestoreDART_run_10072026'))"
#
# Each 2_* document pays its own ~47 s read of the input file and runs about two and a half minutes at ~3.5 GB peak.
# 3_report_lme4_models.Rmd fits nothing and renders in about 20 s.

## Helper files

# plot_functions.R
#     Plot code can be shoved into functions to make the plotting scripts easier to read. It should go here.

# helper_functions.R
#     Other helpful functions should go here.
