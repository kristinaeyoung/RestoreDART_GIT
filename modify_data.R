# BEM
# Purpose: Modify data to prepare for fitting models
# TODO: 

library(dplyr)
library(ggplot2)

source('plot_functions.R')

# File Names/directories
fig_dir <- '../results/figures'
in_file <- '../RestoreDART_DATA/MIXED_MODELS/2_combined_filter_input_data.csv'
tx_key_fl <- '../results/tx_key_BEM.csv'

d0 <- read.csv(in_file)
tx_key <- read.csv(tx_key_fl)

# drop redundant data
drop_cols <- c('response', 'point.pred', 'point.pred.lower', 'point.pred.upper', 'us_l4code', 'YearSinceTrt')
d0 <- d0[, -which(colnames(d0) %in% drop_cols)]
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

# question 1: was there a significant treatment effect?
# for increase_PFG, was 'effect' significantly positive?

# sapply across rows, check if either: lower > 0, upper < 0
# for obj = increase_PFG, only a significantly positive effect is relevant, e.g. lower > 0
d1$sig_effect <- ifelse(d1$lower > 0, 'TRUE', 'FALSE')
#d1$sig_effect <- ifelse(d1$upper < 0, 'TRUE', d1$sig_effect)
d1$sig_effect <- as.logical(d1$sig_effect)

# treatment effectiveness’: percentage of DART pixels that had a significant effect in the direction of the stated objective
# ‘treatment attenuation/appreciation’: neg/pos correlation of between-year treatment effectiveness”
# ‘overall treatment effectiveness’ mean of treatment effectiveness across the entire post-intervention time-series

d1_res <- d1 |>
  dplyr::group_by(tx_coarse, years_since_treatment) |>
  dplyr::summarise(perc_sig = round(mean(sig_effect), 2)) |>
  dplyr::ungroup() |>
  dplyr::filter(tx_coarse == 'seeding')

d1_res |>
  filter(years_since_treatment %in% c(5, 10, 15))

# treatment appreciation, spearman is b/c of strong outliers
stats::cor.test(d1_res$years_since_treatment, d1_res$perc_sig, method = 'pearson')
stats::cor.test(d1_res$years_since_treatment, d1_res$perc_sig, method = 'spearman', exact = F)

round(mean(d1_res$perc_sig) * 100)
se <- function(x) sd(x)/sqrt(length(x))
round(se(d1_res$perc_sig) * 100, 2)
# WHICH PIXEL/YEARS are significantly different from zero?
# is there a threshold of pixel % that makes a polygon "significantly restored"?
# is there a threshold of TIME that makes a polygon "significantly restored"?
# or, years_since_treatment? e.g. years_since_treatment = > 20, what % of sig_effect == T?

# this could all make a good heatmap, and would be a "DART result"
# borrow heatmap code from CARE

# What proportion of DART pixels had significant + effect for PFG?
# group the data
fig_1 <- fig_1(d1)
fig_2 <- fig_2(d1)

# remake with the obj_key
# make this figure as PNG to put in the PPT
#png(file.path(fig_dir, 'fig_1_v1.png'), 20, 12, 'in', res = 150)
#png(file.path(fig_dir, 'fig_1_v2.png'), 20, 8, 'in', res = 150)
png(file.path(fig_dir, 'fig_1_v3.png'), 20, 8, 'in', res = 150)
#HighResTiff(fig_1, file.path(fig_dir, 'fig_1'), 20, 12, 150)
fig_1
dev.off()
#HighResTiff(fig_2, file.path(fig_dir, 'fig_2'), 16, 12, 150)
#png(file.path(fig_dir, 'fig_2_v1.png'), 16, 12, 'in', res = 150)
png(file.path(fig_dir, 'fig_2_v2.png'), 14, 5, 'in', res = 150)
fig_2
dev.off()

# figure: heatmap of We could also do this for the most-coarse categories (seeding, veg removal, burn, soil dist), plot as heat map with 
#         treatment as X and objective as Y, calculate ‘treatment appreciation’ as an LM and estimate appreciation/attenuation as a significant 
#         (Spearman has a p-value, N.S. here) +/- effect

# figure: map/multidimensional figure of sample size? dimensions: pixel/polygon x treatment x ecoregion x time

# figure: MAP of DART pixels, bin across treatments?
