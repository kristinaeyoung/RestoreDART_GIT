# BEM
# created  07 October 2026
# last run NOT RUN

# Pooled mixed-model analysis of DART treatment effects: the stage the three
# `2_*.Rmd` documents stop short of. Those documents are a descriptive synthesis
# of pixel-level DART estimates - counts and shares of significant pixels, per-
# polygon means, year bins and peak years - and nothing in them pools `effect`
# across pixels in a further model. This script is where that pooling happens.
#
# Drafted by Claude on 07 October 2026 and NOT RUN. Every number it would
# produce is unverified, no model here has been fit against the real data, and
# the ladder below is a translation of prior work rather than a result. Treat it
# as a starting point to argue with, not as output.
#
# Results are written to a dated run directory under `out_dir` and are presented
# to collaborators by `3_report_linear_models.Rmd`, which reads that directory
# and refits nothing. The division of labour is the same one the report
# documents use: this script computes, that document displays.
#
# The specification follows KY's model ladder in
# `archive/3_preliminary_analysis.R` and `archive/MIXEDMODEL_modeling_03March2025.R`,
# with the column names remapped to the current input file:
#
#   point.effect                  -> effect
#   YearSinceTrt                  -> year_diff
#   PolyID                        -> polygon
#   target_id                     -> pixel
#   combined_TREATMENT_ASSIGNMENT -> tx_coarse
#   Aridity                       -> aridity
#   SPEI                          -> spei
#
# TODO:
#     * decide between `tx_coarse` as a single multi-level factor (what the
#       archived ladder does, and what `m2` below does) and the four binary
#       indicator columns entered additively. The factor asks which treatment
#       COMBINATION worked; the indicators ask what each component contributes
#       and can separate a component that only ever appears alongside another.
#       They are different questions and the paper should say which one it asks.
#     * `us_l4name` enters `m5` as a fixed effect with ~24 levels while
#       `polygon` is already a random intercept. Every polygon sits in exactly
#       one ecoregion, so the two are nested, not crossed - decide whether
#       ecoregion belongs as a fixed effect, as a third random grouping, or as
#       the level at which treatment effects are allowed to vary.
#     * no spatial or temporal autocorrelation term anywhere. `X`/`Y` are in
#       the input file and KY's script closes with `ape::Moran.I` and
#       `stats::acf` as unfinished business. Residual autocorrelation among
#       pixels inside a polygon is the most likely reason a fixed effect here
#       looks more precise than it is.
#     * decide which of the two `sig_only` runs the paper reports, and say so.
#       Running both and choosing afterwards on the strength of the result would
#       be the wrong way round.

# Caveats/Considerations:
#     * `sig_only` selects between two genuinely different questions, and the
#       answers are not alternative estimates of one quantity - see the comment
#       on the flag itself below. The run is tagged in every filename and
#       recorded in the run manifest so that a table can always be traced back
#       to which of the two it came from.
#     * unweighted. `lower`/`upper` are in the input file and a precision weight
#       (`1 / (upper - lower)`, say) is the obvious extension, but BEM settled
#       on no weighting after the simulation in `weight_testing_summary.Rmd`,
#       and that decision is carried forward here rather than reopened.
#     * fit on the POST-FILTER data, so the sample is the one the report
#       describes - all three sample-size filters, at the thresholds set in
#       `2_results_setup.R`. Those thresholds differ by objective (300 pixels for
#       four of five, 10 for `decrease_tre`), so the five objectives' models are
#       fit to unequally filtered samples and their coefficients are not
#       straightforwardly comparable across objectives. The filtering log
#       written by `2_make_DART_results.Rmd` records what each one cost.
#     * one model per objective, never one model across objectives. A pooled
#       intercept across `decrease_afg` and `increase_pfg` would average
#       opposite intended signs. The intended sign is attached to the
#       coefficient table at the end instead, via `get_intended_sign()`.
#     * the three continuous covariates are centred and scaled; `year_diff` is
#       left in years so its coefficient reads as change per year since
#       treatment, which is the quantity the report's time section is about.
#     * `pixel` nested in `polygon` gives a random-effect level per observation
#       group with hundreds of thousands of levels. This is the expensive part
#       and it may not converge at full size - see `n_poly_sub` below.
#     * the distributional assumptions are DISPLAYED, not tested. The four
#       diagnostic figures per objective - residual, scale-location, residual
#       Q-Q and random-effect Q-Q - are drawn for BEM to look at, and no result
#       below is conditioned on any of them. The one quantity that comes close
#       is the posterior predictive p value, which asks whether simulated data
#       reproduces two summary statistics of the observed data; it is a check on
#       the fitted model as a whole, not a test of normality or homoscedasticity.
#     * the inference here is the likelihood ratio tests in `lrt_tbl` and the
#       confidence intervals in `coef_tbl`. Neither carries a p value on an
#       individual fixed effect, which is lme4's own position rather than a
#       choice made here (Bates et al. 2015, section 5.2.5).

### Libraries

# lme4 is not used anywhere in the report code; it is new to the live sources,
# though both archived modelling scripts already depend on it. nlme, ggeffects,
# broom.mixed, broom and tidyverse appear in the archived scripts and are
# deliberately NOT carried over - everything below is base R, dplyr, ggplot2
# (through plot_functions.R) and lme4.
library(lme4)
library(dplyr)
library(ggplot2)

set.seed(1)

### Setup

# Sourced rather than re-implemented so that the sample-size thresholds, the
# objective split and the three filters have exactly one definition. This costs
# the ~47 s read of the nine report columns and rebuilds the eight compiled
# tables, none of which this script uses, in exchange for models that are fit to
# demonstrably the same rows the report describes.
#
# Leaves behind, among other things: `combined_df`, `summaries`, `summaries_mod`
# (the post-filter copies), `obj_state`, `in_fl`, `out_dir`, `fig_dir`, the
# helper functions and the plotting functions.
source('2_results_setup.R', local = environment())

# ---- constants ----------------------------------------------------------

# Which pixels the models are fit to. This is the one switch in this script that
# changes the QUESTION rather than the estimate, so it is worth being explicit
# about what each setting asks:
#
#   FALSE - every post-filter pixel, significant or not. The coefficients
#     estimate the AVERAGE treatment effect, counting the pixels where DART
#     detected nothing as the near-zero estimates they are. This is the quantity
#     a reader assumes when a paper says "the effect of seeding was x".
#
#   TRUE - only pixels whose DART interval excluded zero. The coefficients then
#     estimate the average effect AMONG DETECTED EFFECTS, which is the quantity
#     the three report documents summarise throughout (mean_effect_by_year() and
#     peak_effect_year() both average over significant pixels only), so this
#     setting is the one whose magnitudes are comparable with the report.
#
# The second is a sample selected on the outcome and its two tails are selected
# in opposite directions, so the residuals are bimodal by construction and a
# normal-errors model is a poor description of them however good the fit
# statistics look. Read the Q-Q figure before quoting anything from a TRUE run.
# These are different estimands, not rival estimates of one: a difference
# between the two runs is not a disagreement to be resolved.
sig_only <- F

# Short tag carried in every output filename, so that the two runs can share a
# dated directory without either overwriting the other and so that a .csv lifted
# out of that directory still says which run produced it.
run_tag <- if (sig_only) 'sigonly' else 'allpix'

# Covariate columns to add to the nine `2_results_setup.R` reads. The first four
# are join keys, not covariates: a row of the input file is one pixel, in one
# year since treatment, for one cover response, so those four identify it.
# `polygon` is in the key as well because `pixel` is not guaranteed unique
# across polygons and a silent many-to-many join is the failure mode here.
mod_keys <- c('polygon', 'pixel', 'year_diff', 'fun_group')
mod_covs <- c('aridity', 'spei', 'mean_cover_5YBT')
mod_cols <- c(mod_keys, mod_covs)

# Covariates to centre and scale before fitting. Not `year_diff` (see caveats),
# and not anything that enters as a factor.
scale_covs <- mod_covs

# Number of polygons to subsample per objective, or NA to fit every polygon.
# `(1 | polygon/pixel)` carries one random-effect level per pixel, and the
# larger objectives bring ~600k post-filter pixels, so a full fit is slow and
# may fail to converge. Set this to a few hundred for an exploratory pass, then
# to NA for the fit that gets reported. Sampling is at the POLYGON level, not
# the pixel level, because polygons are the independent spatial replicates -
# thinning pixels within a polygon would shrink the sample without reducing the
# number of random-effect levels, which is the part that costs.
n_poly_sub <- NA

# Whether to add an eighth rung carrying a random SLOPE for time since
# treatment by polygon, on top of the full fixed-effect model. The scientific
# question is whether the effect develops at different rates in different
# polygons rather than just starting from different levels, which every rung
# below assumes away by fitting intercepts only; KY's archived script carried
# the same idea as a commented-out `model_lmer3`.
#
# The slope is entered UNCORRELATED with the intercept, `(year_diff || polygon)`
# (Bates et al. 2015, Table 2). The paper's caveat on that notation is that an
# uncorrelated specification is not invariant to additive shifts of the
# predictor and should be restricted to predictors on a ratio scale, where zero
# is meaningful rather than conventional. `year_diff` qualifies - zero is the
# treatment - so the uncorrelated form is legitimate here and buys one
# covariance parameter rather than three.
#
# Off by default because it is much the most expensive rung: a slope per polygon
# on top of an intercept per pixel. Turn it on once the ladder below converges.
fit_random_slope <- FALSE

# How confidence intervals on the fixed effects are computed. lme4 offers three
# (Bates et al. 2015, section 5.2.6), and on their sleepstudy example the three
# agreed closely - the largest discrepancy was 26%, on a random-effect
# correlation rather than a fixed effect:
#   'Wald'    - the quadratic approximation, essentially free. Symmetric, and
#      only available for fixed effects. The only one that is affordable at this
#      sample size, and the default here for that reason alone.
#   'profile' - likelihood profiling. Asymmetric and much better behaved when
#      the profile zeta plot is non-linear, but it refits the model many times.
#   'boot'    - parametric bootstrap via bootMer(). Best, and hopeless here: it
#      refits the model `nsim` times on hundreds of thousands of rows.
# Worth running 'profile' once on the smallest objective to see whether the Wald
# intervals it reports elsewhere are trustworthy.
ci_method <- 'Wald'

# Simulations for the posterior predictive check (Bates et al. 2015, section
# 5.2.3): simulate this many datasets from each fitted model, compute a summary
# statistic on each, and see where the observed statistic falls in that
# distribution. An observed value in the far tail says the model is a poor
# description of the data in that respect. Set to 0 to skip - each simulation
# draws one value per row, so the cost scales with the fitted sample.
n_ppd <- 200

# Residuals to draw in each diagnostic figure. The figures are saved as ggplot
# objects as well as .png, and a ggplot object carries its own data, so plotting
# every residual would put ~10^6 rows per objective into the .Rdata file and
# make it slower to load than the models are to refit. A uniform sample of 20k
# shows the same shape.
n_resid_plot <- 20000

# ---- output directory ---------------------------------------------------

# One directory per run date, date-stamped MMDDYYYY to match the convention the
# input file and the manuscript versions already use. Both `sig_only` settings
# can share a date; they are told apart by `run_tag` in the filenames and by the
# manifest written at the end.
#
# Deliberately NOT cleared on re-run: a second run on the same date overwrites
# its own files and leaves the other setting's alone.
run_dir <- file.path(out_dir, paste0('RestoreDART_model_run_',
                                     format(Sys.Date(), '%m%d%Y')))
dir.create(run_dir, recursive = T, showWarnings = F)
stopifnot(dir.exists(run_dir))

# Where the fitted objects are cached. Fitting is the expensive step and every
# table and figure below is cheap to rebuild from the fits, so they are saved
# and reloaded rather than refit on every run. Delete the file to force a refit.
mod_fl <- file.path(run_dir, paste0('3_DART_models_', run_tag, '.rds'))

### Data assembly

# The covariate columns are read in a second pass over the input file and joined
# on, rather than added to `in_cols` in `2_results_setup.R`. Keeping them out of
# the report's read is the point: the report documents are explicit that they
# read only the nine columns they use, and widening that read would add ~1 GB
# and ~1 min to all three of them for columns none of them displays.
in_hdr  <- names(read.csv(in_fl, nrows = 1))
stopifnot(all(mod_cols %in% in_hdr))

cov_df <- read.csv(in_fl, colClasses = ifelse(in_hdr %in% mod_cols, NA, 'NULL'))

# A duplicated key would turn every left_join() below into a row-multiplying
# merge that still returns a plausible-looking data frame, so it is checked once
# here rather than trusted.
stopifnot(!anyDuplicated(cov_df[, mod_keys]))

### Model ladder

# Seven nested specifications, fit in order and compared by AIC. This is KY's
# ladder from `archive/3_preliminary_analysis.R`, written out as formulas so the
# comparison is reproducible rather than a block of commented-out calls.
#
# `m0` carries no fixed effects at all: it is the variance partition, and the
# question it answers - how much of the variation in `effect` is between
# polygons versus between pixels within a polygon - is worth reading before any
# of the fixed effects are interpreted.
#
# REML = FALSE throughout, because these models differ in their FIXED effects
# and REML likelihoods are not comparable across different fixed-effect
# structures. Refit the selected model with REML = TRUE before quoting its
# variance components.
mod_forms <- list(
  m0 = effect ~ (1 | polygon / pixel),
  m1 = effect ~ year_diff + (1 | polygon / pixel),
  m2 = effect ~ year_diff + tx_coarse + (1 | polygon / pixel),
  m3 = effect ~ year_diff + tx_coarse + aridity + (1 | polygon / pixel),
  m4 = effect ~ year_diff + tx_coarse + aridity + spei + (1 | polygon / pixel),
  m5 = effect ~ year_diff + tx_coarse + aridity + spei + us_l4name + (1 | polygon / pixel),
  m6 = effect ~ year_diff + tx_coarse + aridity + spei + us_l4name + mean_cover_5YBT + (1 | polygon / pixel)
)

# The optional eighth rung (see `fit_random_slope`): m6's fixed effects, plus a
# per-polygon slope on time since treatment, uncorrelated with the intercept.
if (fit_random_slope) {
  mod_forms$m7 <- effect ~ year_diff + tx_coarse + aridity + spei + us_l4name +
    mean_cover_5YBT + (year_diff || polygon) + (1 | polygon:pixel)
}

### Fitting

if (!file.exists(mod_fl)) {

  mod_fits <- list()

  for (.i in seq_along(summaries_mod)) {

    obj_nm  <- names(summaries_mod)[.i]
    cur_obj <- sub('_[A-Z]+$', '', obj_nm)

    df0 <- summaries_mod[[.i]]$input |>
      dplyr::left_join(cov_df, by = mod_keys)

    # left_join() cannot drop rows, so an increase means the key was not unique
    # after all and a decrease is impossible - either way the join is wrong.
    stopifnot(nrow(df0) == nrow(summaries_mod[[.i]]$input))

    n_pix_post_filter <- nrow(df0)

    # Dropped before fitting rather than left to lmer's na.action, so that the
    # number of rows each model saw is known and recorded alongside its AIC.
    # A covariate that is largely missing for one objective shows up here as a
    # row count far below that objective's post-filter total.
    df0 <- df0[complete.cases(df0[, c('effect', 'year_diff', 'tx_coarse',
                                      'us_l4name', mod_covs)]), ]

    # The significance subset, if asked for. Asserted logical first: `sig` is
    # read straight from the .csv and a character "TRUE"/"FALSE" column would
    # subset to zero rows here rather than erroring (the pandas-to-R boundary
    # that bit the staged-data check).
    if (sig_only) {
      stopifnot(is.logical(df0$sig))
      df0 <- df0[df0$sig, ]
    }

    if (!is.na(n_poly_sub)) {
      poly_keep <- unique(df0$polygon)
      if (length(poly_keep) > n_poly_sub) {
        poly_keep <- sample(poly_keep, n_poly_sub)
      }
      df0 <- df0[df0$polygon %in% poly_keep, ]
    }

    # as.vector() because scale() returns a one-column matrix, which lmer
    # tolerates but which makes every downstream coefficient name carry a
    # matrix subscript. Scaling happens AFTER the subset, so a `sig_only` run
    # centres on the significant pixels' own covariate means - the alternative
    # would make the intercept refer to a covariate combination the fitted
    # sample does not contain.
    for (.c in scale_covs) {
      df0[[.c]] <- as.vector(scale(df0[[.c]]))
    }

    # Factors are built from the data that survived the filters, so a treatment
    # or ecoregion that this objective lost entirely does not linger as an empty
    # level and produce an NA coefficient.
    df0$tx_coarse <- factor(df0$tx_coarse)
    df0$us_l4name <- factor(df0$us_l4name)
    df0$polygon   <- factor(df0$polygon)
    df0$pixel     <- factor(df0$pixel)

    # A single-level factor cannot enter as a fixed effect. Rather than failing
    # on the first objective that has only one surviving ecoregion, the ladder
    # is trimmed to the specifications that are estimable for this objective and
    # what was skipped is recorded. A `sig_only` run makes this likelier, since
    # the subset can empty a treatment or an ecoregion entirely.
    forms_i <- mod_forms
    if (nlevels(df0$tx_coarse) < 2) forms_i <- forms_i[c('m0', 'm1')]
    if (nlevels(df0$us_l4name) < 2) forms_i <- forms_i[setdiff(names(forms_i), c('m5', 'm6'))]

    fits_i <- lapply(forms_i, \(ff) {
      lme4::lmer(formula = ff, data = df0, REML = F)
    })

    mod_fits[[obj_nm]] <- list(
      objective   = cur_obj,
      cover       = sub('^.*_', '', obj_nm),
      sig_only    = sig_only,
      n_pix_avail = n_pix_post_filter,
      n_row       = nrow(df0),
      n_poly      = nlevels(df0$polygon),
      n_pixel     = nlevels(df0$pixel),
      n_tx        = nlevels(df0$tx_coarse),
      n_eco       = nlevels(df0$us_l4name),
      forms       = forms_i,
      fits        = fits_i
    )

    rm(df0, fits_i, forms_i)
    gc()
  }

  saveRDS(mod_fits, mod_fl)

} else {
  mod_fits <- readRDS(mod_fl)
}

### Model comparison

# One row per objective x specification. `delta_aic` is within an objective, so
# the best specification for each objective is its 0 - AIC is not comparable
# across objectives here, because each objective's models are fit to a different
# number of rows.
aic_tbl <- lapply(names(mod_fits), \(nm) {
  mf <- mod_fits[[nm]]
  aic_i <- sapply(mf$fits, AIC)
  data.frame(
    objective = mf$objective,
    cover     = mf$cover,
    model     = names(mf$fits),
    form      = sapply(mf$forms, \(ff) paste(deparse(ff), collapse = ' ')),
    n_row     = mf$n_row,
    n_poly    = mf$n_poly,
    df        = sapply(mf$fits, \(xx) attr(logLik(xx), 'df')),
    logLik    = round(sapply(mf$fits, \(xx) as.numeric(logLik(xx))), 2),
    AIC       = round(aic_i, 2),
    delta_aic = round(aic_i - min(aic_i), 2),
    row.names = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

# The sample each objective's models were fit to, as its own one-row-per-
# objective table. Separate from `aic_tbl` because it does not vary by
# specification, and because a `sig_only` run's `pct_pix_used` is the headline
# number for how much of the post-filter sample the subset kept.
sample_tbl <- lapply(names(mod_fits), \(nm) {
  mf <- mod_fits[[nm]]
  data.frame(
    objective    = mf$objective,
    cover        = mf$cover,
    sig_only     = mf$sig_only,
    n_pix_avail  = mf$n_pix_avail,
    n_row        = mf$n_row,
    pct_pix_used = round(100 * mf$n_row / mf$n_pix_avail, 2),
    n_poly       = mf$n_poly,
    n_pixel      = mf$n_pixel,
    n_tx         = mf$n_tx,
    n_eco        = mf$n_eco,
    n_models     = length(mf$fits),
    row.names    = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

### The selected model

# "Selected" is lowest AIC within an objective, resolved once here because
# everything below reports on the selected model rather than the ladder. Stated
# as the rule rather than applied silently: the ladder is nested, so AIC rewards
# the larger model whenever the added covariate does anything at all, and a
# one-unit difference between `m5` and `m6` is not a reason to prefer either.
# Read `aic_tbl` and `lrt_tbl` before taking any of it at face value.
best_model <- sapply(mod_fits, \(mf) names(which.min(sapply(mf$fits, AIC))))

### Convergence

# Read off the saved fits rather than captured during fitting, so that a cached
# .rds still reports it. Two different things are checked and they mean
# different things (Bates et al. 2015, section 4):
#
#   isSingular() - the fitted variance-covariance matrix sits on the boundary,
#     i.e. some variance component is estimated as exactly zero. lme4 deliberately
#     parameterises in a constrained space so that singular fits CAN be
#     represented rather than failing, so this is a report about the data, not a
#     numerical failure: it usually means the grouping factor it names explains
#     no variance once the others are in. With `(1 | polygon/pixel)` on data
#     where many pixels are observed in only one year, a singular pixel term is
#     a real possibility and worth seeing.
#
#   optinfo messages - the optimizer's own warnings. The paper notes the authors
#     have only seen the underlying algorithm fail on badly scaled problems,
#     i.e. continuous predictors spanning a very large or very small numerical
#     range, which is the reason the three covariates are centred and scaled
#     above. A convergence warning here is therefore worth taking seriously
#     rather than ignoring: the usual cause has already been ruled out.
conv_tbl <- lapply(names(mod_fits), \(nm) {
  mf <- mod_fits[[nm]]
  data.frame(
    objective   = mf$objective,
    cover       = mf$cover,
    model       = names(mf$fits),
    singular    = sapply(mf$fits, lme4::isSingular),
    n_warning   = sapply(mf$fits, \(xx) length(xx@optinfo$conv$lme4$messages)),
    optimizer   = sapply(mf$fits, \(xx) xx@optinfo$optimizer),
    message     = sapply(mf$fits, \(xx) {
      ms <- xx@optinfo$conv$lme4$messages
      if (is.null(ms) || !length(ms)) '' else paste(ms, collapse = '; ')
    }),
    row.names   = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

### Likelihood ratio tests across the ladder

# The AIC table above ranks the rungs; this tests them. lme4 reports no p values
# on its fixed effects by design, and the paper's own recommendation for when an
# inferential statement is needed is to frame it as a likelihood ratio test by
# handing several fitted models to anova() (Bates et al. 2015, section 5.2.5),
# which is exactly what the nested ladder supports.
#
# No refitting happens: anova() refits REML fits with ML because REML
# likelihoods are not comparable across different fixed-effect structures, and
# every rung here was already fit with REML = FALSE for that reason.
#
# Read alongside `delta_aic`, not instead of it. Each test compares ONE rung
# against the one below it, so the p values are a sequence of nested
# comparisons, not five independent tests, and nothing here corrects for the
# six comparisons made within an objective.
#
# Two details of anova.merMod() that only surface against a real fit, both found
# on the first full run (07 Oct 2026, lme4 2.0.6):
#
#   * `model.names` has to be passed explicitly. unname() is needed so that the
#     fits arrive as positional arguments rather than as named ones that would
#     collide with anova()'s own formals, but it also leaves lme4 with nothing to
#     deparse, so it warns 'failed to find model names' and labels the rows
#     MODEL1..MODEL7. The ladder's own names are what the table should carry.
#
#   * the deviance column is named '-2*log(L)' in current lme4 and 'deviance' in
#     older versions. `av$deviance` is NULL against the former, and round(NULL)
#     is an error rather than an empty column, so this failed hard instead of
#     silently. Resolved by name so the script works either side of the rename.
lrt_tbl <- lapply(names(mod_fits), \(nm) {
  mf <- mod_fits[[nm]]
  av <- as.data.frame(do.call(anova, c(unname(mf$fits),
                                       list(model.names = names(mf$fits)))))
  dev_col <- intersect(c('deviance', '-2*log(L)'), names(av))[1]
  stopifnot(!is.na(dev_col))
  data.frame(
    objective = mf$objective,
    cover     = mf$cover,
    model     = rownames(av),
    npar      = av$npar,
    deviance  = round(av[[dev_col]], 2),
    chisq     = round(av$Chisq, 3),
    chisq_df  = av$Df,
    p_value   = signif(av$`Pr(>Chisq)`, 4),
    row.names = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

### Sequential decomposition of the selected model

# anova() on a SINGLE merMod returns the sequential (Type I) sums of squares and
# F statistics for the fixed-effects terms, in the order they enter the formula.
# The paper is explicit that this is an informal assessment only - there are no
# p values attached, because there is no agreed way to get denominator degrees
# of freedom for these F statistics (Bates et al. 2015, section 5.2.4). It is
# read as a relative magnitude: which terms account for much of the variation
# the model explains and which account for almost none.
#
# Sequential means order-dependent. Every term is adjusted for the terms BEFORE
# it and not for the terms after, so `year_diff` - first in every formula -
# carries any variation it shares with the covariates that follow.
seq_tbl <- lapply(names(mod_fits), \(nm) {
  mf <- mod_fits[[nm]]
  av <- as.data.frame(anova(mf$fits[[best_model[[nm]]]]))
  data.frame(
    objective = mf$objective,
    cover     = mf$cover,
    model     = best_model[[nm]],
    term      = rownames(av),
    npar      = av$npar,
    sum_sq    = round(av$`Sum Sq`, 3),
    mean_sq   = round(av$`Mean Sq`, 3),
    f_value   = round(av$`F value`, 3),
    row.names = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

### Fixed effects of the selected model

# `intended_sign` is attached so that a coefficient can be read against the
# objective's goal without the reader working out the sign convention - it is
# -1 for `decrease_*` and +1 for `increase_*`, from get_intended_sign(), which
# is the single source of that mapping (see the terminology section of
# `2_make_DART_results.Rmd`).
#
# `lo` / `hi` come from confint() at `ci_method` rather than from a multiple of
# the standard error. The two coincide exactly when `ci_method` is 'Wald', which
# is the default, but writing the interval the model reports means switching to
# 'profile' on one objective changes this table instead of silently producing a
# symmetric interval that the profile method did not give.
#
# No p value is reported, and that is lme4's own position rather than a choice
# made here: the package omits them because the null distributions of these
# statistics are not t or F distributed at finite sample size, and the usual
# corrections are "at best ad hoc solutions" (Bates et al. 2015, section 5.2.5).
# The paper's recommended alternatives are the likelihood ratio tests in
# `lrt_tbl` and the confidence intervals in this table, which is what the two
# together provide. `t_value` is carried as a magnitude, not a test statistic.
coef_tbl <- lapply(names(mod_fits), \(nm) {
  mf  <- mod_fits[[nm]]
  fit <- mf$fits[[best_model[[nm]]]]
  cf  <- summary(fit)$coefficients

  # parm = 'beta_' restricts the interval to the fixed effects; the Wald method
  # cannot produce one for a variance component anyway.
  ci <- confint(fit, method = ci_method, parm = 'beta_')
  ci <- ci[match(rownames(cf), rownames(ci)), , drop = FALSE]
  stopifnot(nrow(ci) == nrow(cf))

  data.frame(
    objective     = mf$objective,
    cover         = mf$cover,
    model         = best_model[[nm]],
    term          = rownames(cf),
    estimate      = round(cf[, 'Estimate'], 4),
    se            = round(cf[, 'Std. Error'], 4),
    lo            = round(ci[, 1], 4),
    hi            = round(ci[, 2], 4),
    ci_method     = ci_method,
    t_value       = round(cf[, 't value'], 3),
    intended_sign = get_intended_sign(mf$objective),
    row.names     = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

### Variance components of the selected model

# The m0 question, carried through to whichever model was selected: how much of
# the residual variation sits between polygons, between pixels within a polygon,
# and within a pixel across years. A polygon variance that dwarfs the others is
# the quantitative version of the report's standing caveat that pixels inside a
# polygon are not independent replicates.
vc_tbl <- lapply(names(mod_fits), \(nm) {
  mf <- mod_fits[[nm]]
  vc <- as.data.frame(lme4::VarCorr(mf$fits[[best_model[[nm]]]]))
  data.frame(
    objective = mf$objective,
    cover     = mf$cover,
    model     = best_model[[nm]],
    grp       = vc$grp,
    variance  = round(vc$vcov, 4),
    sd        = round(vc$sdcor, 4),
    pct_var   = round(100 * vc$vcov / sum(vc$vcov), 2),
    row.names = NULL,
    stringsAsFactors = F
  )
}) |>
  (\(xx) do.call(rbind, xx))()

### Posterior predictive check

# Simulate `n_ppd` datasets from each selected model, compute a summary
# statistic on each, and ask where the OBSERVED statistic falls in that
# distribution (Bates et al. 2015, section 5.2.3, which gives this recipe with
# the interquartile range as the statistic). A value in the far tail says the
# model fails to reproduce that feature of the data.
#
# Two statistics rather than one, because they fail differently. The IQR is the
# spread of the bulk of the distribution; the SD is dominated by the tails. A
# model that matches the IQR but badly under-predicts the SD is reproducing the
# typical pixel and missing the extreme ones - which is the expected failure
# mode here, since the response is itself an estimate whose own error grows
# where cover is low.
#
# The p value is one-tailed and computed the paper's way, counting the observed
# value into its own reference distribution so it can never be exactly 0 or 1.
# Values near 0.5 are unremarkable; values near 0 or 1 are the signal. Note the
# paper's own caveat: unlike a full Bayesian posterior predictive check, this
# conditions on the fitted parameters and ignores their uncertainty, which is a
# reasonable approximation when the residual variation is large.
if (n_ppd > 0) {

  ppd_tbl <- lapply(names(mod_fits), \(nm) {
    mf  <- mod_fits[[nm]]
    fit <- mf$fits[[best_model[[nm]]]]
    sims <- simulate(fit, nsim = n_ppd)
    y    <- lme4::getME(fit, 'y')

    do.call(rbind, lapply(c(IQR = 'IQR', SD = 'sd'), \(fn) {
      f_i  <- match.fun(fn)
      sim_v <- vapply(sims, f_i, numeric(1))
      obs_v <- f_i(y)
      data.frame(
        objective  = mf$objective,
        cover      = mf$cover,
        model      = best_model[[nm]],
        statistic  = fn,
        observed   = round(obs_v, 4),
        sim_mean   = round(mean(sim_v), 4),
        sim_lo     = round(quantile(sim_v, 0.025, names = FALSE), 4),
        sim_hi     = round(quantile(sim_v, 0.975, names = FALSE), 4),
        n_sim      = n_ppd,
        ppd_p      = round(mean(obs_v >= c(obs_v, sim_v)), 4),
        row.names  = NULL,
        stringsAsFactors = F
      )
    }))
  }) |>
    (\(xx) do.call(rbind, xx))()

  rm(list = intersect(c('sims', 'y'), ls()))
  gc()

} else {
  ppd_tbl <- NULL
}

### Figures

# Every figure is built through a function in `plot_functions.R`, like every
# other figure in this project, and collected into one named list. The list is
# written out as a single .Rdata so that `3_report_linear_models.Rmd` can load
# the plot objects and print them, and each element is also written as a .png
# for pasting into slides and mail.
#
# Residuals are subsampled (see `n_resid_plot`): a ggplot object carries its own
# data, so the saved objects would otherwise be the largest files in the run.
mod_plots <- list()

for (.nm in names(mod_fits)) {

  mf  <- mod_fits[[.nm]]
  fit <- mf$fits[[best_model[[.nm]]]]

  res_df <- data.frame(fitted = fitted(fit), resid = resid(fit))
  if (nrow(res_df) > n_resid_plot) {
    res_df <- res_df[sample(nrow(res_df), n_resid_plot), ]
  }

  sub_i <- sprintf('%s, n = %s of %s residuals shown',
                   best_model[[.nm]],
                   format(nrow(res_df), big.mark = ','),
                   format(mf$n_row, big.mark = ','))

  mod_plots[[paste0('resid_', .nm)]] <- plot_model_resid(
    res_df, title = .nm, subtitle = sub_i
  )
  mod_plots[[paste0('scaleloc_', .nm)]] <- plot_model_scale_location(
    res_df, title = .nm, subtitle = sub_i
  )
  mod_plots[[paste0('qq_', .nm)]] <- plot_model_qq(
    res_df, title = .nm, subtitle = sub_i
  )

  # Conditional modes of the random effects, which the three residual figures
  # above say nothing about: they test the within-pixel errors, this tests
  # whether the polygon and pixel intercepts are themselves normal. Thinned on
  # the same budget - the pixel grouping has one level per pixel, so the
  # full set is as large as the residual vector.
  ran_df <- as.data.frame(lme4::ranef(fit))
  if (nrow(ran_df) > n_resid_plot) {
    ran_df <- ran_df[sample(nrow(ran_df), n_resid_plot), ]
  }
  mod_plots[[paste0('ranefqq_', .nm)]] <- plot_model_ranef_qq(
    ran_df, title = .nm,
    subtitle = sprintf('%s, conditional modes by grouping factor', best_model[[.nm]])
  )

  rm(res_df, ran_df)
}

mod_plots[['coefs_all']] <- plot_model_coefs(
  coef_tbl,
  title    = 'Fixed effects of the selected model, by objective',
  subtitle = sprintf('Lowest-AIC specification per objective; bars are +/- 1.96 SE; %s',
                     if (sig_only) 'significant pixels only' else 'all post-filter pixels')
)

mod_plots[['varcomp_all']] <- plot_model_varcomp(
  vc_tbl,
  title    = 'Variance partition of the selected model, by objective',
  subtitle = 'Percent of total variance in the pixel-level DART effect'
)

# Figure dimensions in inches, by name prefix. The two cross-objective figures
# need more room than a single-panel diagnostic; the coefficient figure needs
# the most, because a panel can carry twenty-odd ecoregion contrasts.
fig_dim <- list(resid    = c(5, 4.5),  qq      = c(5, 4.5),
                scaleloc = c(5, 4.5),  ranefqq = c(8, 4.5),
                coefs    = c(11, 9),   varcomp = c(8, 5.5))

for (.nm in names(mod_plots)) {
  dim_i <- fig_dim[[sub('_.*$', '', .nm)]]
  ggplot2::ggsave(
    filename = file.path(run_dir, paste0('3_model_', .nm, '_', run_tag, '.png')),
    plot     = mod_plots[[.nm]],
    width    = dim_i[1], height = dim_i[2], dpi = 300
  )
}

# One .Rdata holding the whole named list rather than one file per figure: the
# report document loads it in a single line, and a plot object is only useful
# alongside the others it is compared with.
save(mod_plots, file = file.path(run_dir, paste0('3_model_plots_', run_tag, '.Rdata')))

### Write

# The run manifest. Written so that a run directory carries its own settings -
# anyone reading a .csv out of it, or rendering the report document against it,
# can see which pixels were used, what the thresholds were and what fit the
# models, without reading this script or trusting a filename.
manifest <- data.frame(
  run_date         = format(Sys.Date(), '%Y-%m-%d'),
  run_tag          = run_tag,
  sig_only         = sig_only,
  pixels_used      = if (sig_only) 'significant pixels only' else 'all post-filter pixels',
  n_poly_sub       = ifelse(is.na(n_poly_sub), 'all polygons', as.character(n_poly_sub)),
  n_objectives     = length(mod_fits),
  n_models         = length(mod_forms),
  random_slope     = fit_random_slope,
  ci_method        = ci_method,
  n_ppd            = n_ppd,
  n_singular       = sum(conv_tbl$singular),
  n_conv_warning   = sum(conv_tbl$n_warning > 0),
  scaled_covs      = paste(scale_covs, collapse = ', '),
  min_txeco_n      = paste(names(min_txeco_n), min_txeco_n, sep = '=', collapse = '; '),
  drop_depopulated = drop_depopulated,
  min_year_n       = paste(names(min_year_n), min_year_n, sep = '=', collapse = '; '),
  year_trunc_metric = year_trunc_metric,
  input_file       = basename(in_fl),
  r_version        = paste(R.version$major, R.version$minor, sep = '.'),
  lme4_version     = as.character(utils::packageVersion('lme4')),
  row.names        = NULL,
  stringsAsFactors = F
)

# Named to sort after the nine `2_DART_results_*.csv` files the report writes to
# the parent directory, and tagged so the two `sig_only` runs never collide.
out_tbls <- list(manifest = manifest, sample = sample_tbl, convergence = conv_tbl,
                 aic = aic_tbl, lrt = lrt_tbl, sequential = seq_tbl,
                 coefs = coef_tbl, variance = vc_tbl, ppd = ppd_tbl)

# NULL entries are dropped rather than written as an empty file that would read
# as a real result: `ppd` is NULL whenever `n_ppd` is 0.
out_tbls <- out_tbls[!vapply(out_tbls, is.null, logical(1))]

for (.nm in names(out_tbls)) {
  write.csv(out_tbls[[.nm]],
            file.path(run_dir, paste0('3_DART_model_', .nm, '_', run_tag, '.csv')),
            row.names = F)
}
