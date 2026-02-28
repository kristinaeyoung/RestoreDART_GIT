# BEM Feb 2026

# clean up the objectives .csv

d1 <- read.csv('../RestoreDART_DATA/MIXED_MODELS/MIXEDMODEL_reduced_objectives_assigned.csv')

# removes intercalary carriage returns and line breaks
d1$combined_Objectives <- gsub('\\r', '', d1$combined_Objectives)
d1$combined_Objectives <- gsub('\\n', '', d1$combined_Objectives)

# the rest of the columns are pretty messsy but they might not need fixing, this just improves readability