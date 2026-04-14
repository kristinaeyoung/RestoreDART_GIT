# BEM March 2026

library(brms)
library(dplyr)

set.seed(1)

# TODO:
#    * write/develop methods for modelling the data structure
#    * currently, 1 model = 1 objective, and only "significant' pixels are used

in_fl <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_14042026.csv'
brms_mod_dir <- '../RestoreDART_DATA/MIXED_MODELS/brms_models'

dat <- read.csv(in_fl)

dat_PFG <- dat |>
  filter(fun_group == 'AFG') |>
  filter(grepl('increase_PFG', objective)) |>
  filter(sig == TRUE) |>
  select(-sig) |>
  filter(year_diff >= 0) |>
  filter(effect > 0) |>
  # only two samples of prescribed burn;seeding;soil disturbance
  filter_out(tx_coarse == 'prescribed burn;seeding;soil disturbance')

# model notes:
# sig TRUE and effect > 0 works well with a lognormal family
# (1 | polygon/pixel) cant be fit with PFG replication
# (1 | subj/year_diff) cant be fit with PFG replication, 600+ divergences and bad pairs
# (1 | polygon/year_diff) is better, 171 divergent, pairs not terrible. but, polygon taking information away from tx?
# effect ~ (1 | tx_coarse) fits well, does not account for time
# effect ~ (1 | tx_coarse/year_diff) doesnt make sense with time as an unordered effect
f_1 <- brmsformula(effect ~ (1 + year_diff | tx_coarse), family = lognormal())

fit_1 <- brms::brm(formula = f_1, data = dat_PFG, chains = 4, iter = 5000, warmup  = 2000, cores = 4)
summary(fit_1)
pp_check(fit_1, ndraws = 100) 
pairs(fit_1)
ranef(fit_1)

# moran's
res <- residuals(fit, method = "posterior_predict")[, "Estimate"]
Create spatial weights: listw <- nb2listw(neighbors, style = "W")
Run test: spdep::moran.test(res, listw).


fit_2 <- brms::brm(
  formula = f_2, data = dat_PFG, chains = 4, iter = 3000, warmup  = 1000, cores = 4
)
#fit_3 <- brms::brm(
#  formula = f_3, data = dat_PFG, chains = 4, iter = 3000, warmup  = 1000, cores = 4
#)
#fit_4 <- brms::brm(
#  formula = f_4, data = dat_PFG, chains = 4, iter = 3000, warmup  = 1000, cores = 4
#)

save(fit_1, file = file.path(brms_mod_dir, 'fit_1_PFG.RData'))
save(fit_2, file = file.path(brms_mod_dir, 'fit_2_PFG.RData'))
#save(fit_3, file = file.path(brms_mod_dir, 'fit_3_PFG.RData'))
#save(fit_4, file = file.path(brms_mod_dir, 'fit_4_PFG.RData'))

# claude generated template below
summary(fit_1)
print(prior_summary(fit_1))

#bayesplot::mcmc_trace(fit_1)
#bayesplot::mcmc_rhat(rhat(fit_1))
#bayesplot::mcmc_neff(neff_ratio(fit_1))

pp_check(fit_1, ndraws = 100)
#pp_check(fit_2, ndraws = 100)
#pp_check(fit_3, ndraws = 100)
#pp_check(fit_4, ndraws = 100)

# y_rep <- posterior_predict(fit_1, ndraws = 500)
# y_obs <- data$y
# 
# ppc_dens_overlay(y_obs, y_rep[1:100, ]) +
#   labs(title    = "PPC: Density Overlay",
#        subtitle = "Light lines = posterior predictive draws; dark line = observed") +
#   theme_minimal(base_size = 14)
# 
# ppc_stat(y_obs, y_rep, stat = "mean") +
#   labs(title = "PPC: Distribution of Replicated Means") +
#   theme_minimal(base_size = 14)
# 
# ppc_stat(y_obs, y_rep, stat = "sd") +
#   labs(title = "PPC: Distribution of Replicated SDs") +
#   theme_minimal(base_size = 14)
# 
# ppc_stat_2d(y_obs, y_rep, stat = c("mean", "sd")) +
#   labs(title = "PPC: Joint Distribution of Mean and SD") +
#   theme_minimal(base_size = 14)
# 
# ppc_intervals(
#   y          = y_obs,
#   yrep       = y_rep,
#   x          = data$x1,
#   prob       = 0.50,
#   prob_outer = 0.95
# ) +
#   labs(title = "PPC: Prediction Intervals vs. x1",
#        x = "x1", y = "y") +
#   theme_minimal(base_size = 14)
# 
# loo_1 <- loo(fit_1, moment_match = FALSE)
# print(loo_1)
# 
# mean_rep <- apply(y_rep, 1, mean)
# bayesian_p_value_mean <- mean(mean_rep >= mean(y_obs))
# 
# conditional_effects(fit_1)
# 
# ppc_plot <- ppc_dens_overlay(y_obs, y_rep[1:100, ]) +
#   labs(title = "PPC: Density Overlay") +
#   theme_minimal(base_size = 14)
