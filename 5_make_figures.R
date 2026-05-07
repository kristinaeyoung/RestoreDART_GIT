# BEM March 2026

library(dplyr)
library(ggplot2)

# TODO:
#    bring in prelim plots from ./archive/modify_data.R
#    add extra raw data plots as discussed with Kristina on 3/12/26 meeting

source('plot_functions.R')

# File Names/directories
fig_dir <- '../results/figures'
in_file <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_04052026.csv'

d0 <- read.csv(in_file)

# how many unique methods?
v1 <- unique(d0$tx_coarse) |>
  strsplit(';') |>
  unlist() |>
  unique()

# condense objectives into 5 separate models
d_PFG <- d0 |>
  filter(objective == 'increase_PFG') |>
  filter(grepl('PFG', fun_group)) |>
  filter(year_diff > 0)

# test figures
fig1 <- fig_1(d_PFG)
fig2 <- fig_2(d_PFG)

png(file.path(fig_dir, 'fig_1.png'), 25, 9, 'in', res = 150)
fig1
dev.off()

png(file.path(fig_dir, 'fig_2.png'), 25, 9, 'in', res = 150)
fig2
dev.off()

  
# also decrease_AFG, increase_SHR, decrease_SHR, decrease_TRE
# for combination objectives, how do we decide which go into their own groups?
# each df has a landscape response to restoration for a set of pixels and years within polygons

# question 1: was there a significant treatment effect?
# for increase_PFG, was 'effect' significantly positive?
round((table(d_PFG$sig) / nrow(d_PFG)) * 100, 1)
# 87.4% not-significant, 12.6% significant
round((nrow(filter(d_PFG, sig == T & effect > 0)) / nrow(d_PFG)) * 100, 1)
# 9.2% significantly positive

d_PFG_res <- d_PFG |>
  group_by(tx_coarse, year_diff) |>
  summarise(perc_sig = mean(sig), .groups = 'drop') |>
  #filter(yyear_diff %in% c(5, 10, 15))
  ungroup()

sub_names <- table(d_PFG$polygon, d_PFG$sig) |>
  as.data.frame() |>
  filter(Var2 == 'TRUE') |>
  filter(Freq > 0) |>
  select(Var1) |>
  unlist() |>
  as.character() |>
  as.integer()

d_PFG_sub <- d_PFG |>
  filter(polygon %in% sub_names)
  

# treatment appreciation, spearman is b/c of strong outliers
stats::cor.test(d_PFG_res$year_diff, d_PFG_res$perc_sig, method = 'pearson')
stats::cor.test(d_PFG_res$year_diff, d_PFG_res$perc_sig, method = 'spearman', exact = F)
# rho = -0.28

# Can we use this to quantify “effectiveness”? e.g. “We define ‘treatment effectiveness’ as the percentage of DART pixels that had a significant effect in the 
# direction of the stated objective, ‘treatment attenuation/appreciation’ as the neg/pos Pearson/Spearman correlation of between-year treatment effectiveness”, 
# and ‘overall treatment effectiveness’ as the mean of treatment effectiveness across the entire post-intervention time-series. For these results (‘increase_PFG’) 
# and tx = ‘seeding’, treatment effectiveness at 5, 10, 15 years post-treatment = 30%, 19%, 23% effective, ‘treatment appreciation’ = +0.37 (Pearson) / +0.27 (Spearman), 
# ‘overall treatment effectiveness’ = 24 +/- 1.7 %. We could also do this for the most-coarse categories (seeding, veg removal, burn, soil dist), 
# plot as heat map with treatment as X and objective as Y, calculate ‘treatment appreciation’ as an LM and estimate appreciation/attenuation as a significant 
# (Spearman has a p-value, N.S. here) +/- effect

# treatment effectiveness’: percentage of DART pixels that had a significant effect in the direction of the stated objective
# ‘treatment attenuation/appreciation’: neg/pos correlation of between-year treatment effectiveness”
# ‘overall treatment effectiveness’ mean of treatment effectiveness across the entire post-intervention time-series

# treatment appreciation, spearman is b/c of strong outliers

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
fig1 <- fig_1(d_PFG)
fig2 <- fig_2(d_PFG)
fig3 <- fig_1(d_PFG_sub)
fig4 <- fig_1(d_PFG_sub)

png(file.path(fig_dir, 'fig_1_v3.png'), 20, 8, 'in', res = 150)
fig1
dev.off()
png(file.path(fig_dir, 'fig_2_v3.png'), 14, 5, 'in', res = 150)
fig2
dev.off()
png(file.path(fig_dir, 'fig_3_v3.png'), 14, 5, 'in', res = 150)
fig3
dev.off()
png(file.path(fig_dir, 'fig_4_v3.png'), 14, 5, 'in', res = 150)
fig4
dev.off()

# figure: heatmap of We could also do this for the most-coarse categories (seeding, veg removal, burn, soil dist), plot as heat map with 
#         treatment as X and objective as Y, calculate ‘treatment appreciation’ as an LM and estimate appreciation/attenuation as a significant 
#         (Spearman has a p-value, N.S. here) +/- effect

# figure: map/multidimensional figure of sample size? dimensions: pixel/polygon x treatment x ecoregion x time

# figure: MAP of DART pixels, bin across treatments?
