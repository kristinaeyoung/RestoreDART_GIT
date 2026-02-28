library(tidyr)
library(dplyr)

in_file <- '../RestoreDART_DATA/MIXED_MODELS/2_combined_filter_input_data.csv'
AI_file <- '../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_AI_For_DART_1km.csv'
SPEI_file <- '../RestoreDART_DATA/CLIMATE_DATA/Annual_Mean_SPEI_For_DART_1km.csv'

d0 <- read.csv(in_file)
#AI <- read.csv(AI_file)
#SPEI <- read.csv(SPEI_file)

d00 <- d0[!is.na(d0$Aridity), ]
d01 <- d0[is.na(d0$Aridity), ]
# are any PolyID totally missing aridity/SPEI?
stopifnot(all(d00$PolyID %in% d01$PolyID))

# is aridity just a polygon-level attribute, that varies by year?
d00 |>
  dplyr::group_by(PolyID, year.index) |>
  dplyr::group_map(~ length(unique(.x$Aridity))) |>
  unlist() |>
  table()
# non-NA aridity only has one value for every year-polygon combination

# are NA's in Aridity dispersed over all years? or some years in their entirety?
d0$Aridity_NA <- is.na(d0$Aridity)
table(d0$Aridity_NA, d0$year.index)
# looks like Aridity is missing in its entirety for 2021-2024

# check SPEI also
d0$SPEI_NA <- is.na(d0$SPEI)
table(d0$SPEI_NA, d0$year.index)
# also missing 2021-2024