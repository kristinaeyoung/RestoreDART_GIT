# Shared setup for the RestoreDART results documents.
#
# Sourced by BOTH `2_make_DART_results.Rmd` (the collaborator-facing results) and
# `2_sample_size_checks.Rmd` (the per-objective sample-size and filtering
# detail). The division of labour is: this script computes, the two documents
# display. Nothing here prints, plots or knits.
#
# It exists because the two documents need the same things. The sample-size
# thresholds must be set in one place or a figure can be filtered differently
# from its own sample-size check, and the nine per-objective tables that
# `collect_obj_tables()` stacks across objectives are displayed in the main
# report but described in the checks file - so whichever document displays them,
# they have to be built somewhere both can reach.
#
# Source it from a knitr chunk as:
#
#   source('2_results_setup.R', local = knitr::knit_global())
#
# The `local =` argument matters: `collect_obj_tables()` looks for objects in
# `ls(envir = parent.frame())`, which from a chunk is the knit environment, so
# the nine `<prefix><objective>_<COVER>` objects this script assigns have to
# land there rather than in whatever environment `source()` would pick by
# default.
#
# Objects this script leaves behind, in the order they are built:
#
#   combined_df    the 9-column post-read input, all rows including pre-treatment
#   summaries      split_by_objective() output, subset to the five in-scope
#                  elements, UNFILTERED
#   summaries_mod  the same five elements after all three sample-size filters
#   obj_state      one list per element, keyed by element name, carrying that
#                  objective's labels, its filtering bookkeeping and its tables
#   poly_by_obj, total_poly_all, poly_by_elem, obj_of_elem  polygon inventory
#   <prefix><objective>_<COVER>   the nine compiled-table copies, flat, for
#                  collect_obj_tables()

# dplyr, tidyr and ggplot2 are attached here rather than left to the require()
# calls inside the helper and plotting functions. require() emits a "Loading
# required package: ..." message the first time it attaches a package, and that
# message was surfacing in the middle of the rendered document (from the first
# chunk to call a function that needed a package not yet attached). Attaching
# them here - from a chunk that suppresses messages - means every later
# require() is a silent no-op.
library(ggplot2)
library(dplyr)
library(tidyr)

# Sourced into THIS script's evaluation environment rather than wherever
# source()'s default would put them, so that the helpers end up in the same
# place as everything else this script defines.
source('plot_functions.R', local = environment())
source('helper_functions.R', local = environment())

# ---- constants ----------------------------------------------------------

# All paths are relative to the repository root, which is where the three
# `.Rmd` files live and therefore the working directory knitr renders them in.
# The input file sits outside the repository on purpose - it is 3.2 GB and is
# the DART stage's output, not source - and the results directory sits beside
# it so that nothing generated lands under version control.
#
# Earlier locations, kept because they are the record of where the inputs have
# lived rather than because any still resolves:
#   '../../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_06192026.csv'
#   '../../RestoreDART_DATA/claude_visible/1_combined_filter_input_data_06192026.csv'
#   '../claude_visible/1_combined_filter_input_data_06192026.csv'
# and '../../results' / '../../results/figures', then
# '../claude_visible/results' / '.../figures', for the two output directories.
# The '../claude_visible' round was retired on 07 Oct 2026 when that directory
# was removed and the stage-1 inputs and output were consolidated under
# ../analysis_inputs.

# Version stamp of the stage-1 output this report describes, MMDDYYYY. Must be
# bumped to match `out_stamp` in 1_combine_filter_input_data.R whenever that
# script is re-run; neither is derived from Sys.Date(), deliberately, so that a
# stamp cannot move on its own and silently decouple the report from the file it
# describes. Grep both files for the stamp before changing either.
in_stamp <- '10072026'

# Outputs moved out of ../analysis_inputs/results on 08 Oct 2026, when BEM added
# ../analysis_outputs. That directory had been both an input and an output - it
# held tx_key_BEM.csv, which 1_combine_filter_input_data.R reads, alongside
# everything the renders write - and separating the two is the point of the new
# directory. tx_key_BEM.csv stayed behind; everything generated moved.
#
# On 10 Oct 2026 the generated outputs were consolidated one step further, into
# ONE date-stamped directory per run. Before that, ../analysis_outputs was flat:
# the nine compiled .csv files, the render logs and an empty figures/ sat at the
# top level, while the model stage wrote its own RestoreDART_model_run_<stamp>/
# beside them - so the two halves of a single pass over the pipeline were in
# different places and nothing recorded that they belonged together. The
# retired values:
#   fig_dir <- '../analysis_outputs/figures'
#   out_dir <- '../analysis_outputs'
#
# `run_stamp` names the run directory, MMDDYYYY, and is a literal for the same
# reason `in_stamp` is: a stamp derived from Sys.Date() moves on its own, so a
# re-render meant to refresh an existing run would quietly start a new directory
# and leave half that run - the model fits, the other two documents - behind in
# the old one. Bump it deliberately when a run is to be kept separate from the
# last; leave it alone to add to, or overwrite within, the current one.
#
# It currently equals `in_stamp` because the 07 Oct 2026 pass is the live run:
# the model fits in that directory were made from this same input file, so a
# document rendered now belongs beside them. The two stamps are separate
# constants, though, and are not required to agree - a second run against an
# unchanged input is exactly the case the run stamp exists to keep apart.
run_stamp <- '10072026'

out_root <- '../analysis_outputs'
run_dir  <- file.path(out_root, paste0('RestoreDART_run_', run_stamp))
out_dir  <- run_dir
fig_dir  <- file.path(run_dir, 'figures')
log_dir  <- file.path(run_dir, 'logs')
in_fl    <- paste0('../analysis_inputs/RestoreDART_DATA/MIXED_MODELS/',
                   '1_combined_filter_input_data_', in_stamp, '.csv')

# Created rather than asserted, since a new `run_stamp` names a directory that
# does not exist yet. `out_root` IS asserted - it is a grant point, not
# something this script should invent, and a typo there would otherwise be
# silently papered over by dir.create(recursive = T).
stopifnot(dir.exists(out_root))
for (d0 in c(run_dir, fig_dir, log_dir)) dir.create(d0, recursive = T, showWarnings = F)
rm(d0)

# Asserted rather than left to fail at the read: a wrong `in_fl` otherwise
# surfaces as a read.csv() error several lines down, and a missing `out_dir`
# not until the write_csv chunk at the very end of a three-minute render.
stopifnot(file.exists(in_fl), dir.exists(out_dir), dir.exists(fig_dir), dir.exists(log_dir))

# Sample-size thresholds applied to each treatment x ecoregion combination, in
# pixels. This is the ONLY place the thresholds are set. `default` applies to
# every objective x cover response that isn't named explicitly; to use a
# different cutoff for one combination, add an entry keyed either by the full
# `summaries` element name ("<objective>_<COVER>", e.g. "decrease_shr_SHR") or by
# the bare objective ("decrease_shr") - the two are interchangeable, since
# split_by_objective() pairs each objective with exactly one cover response.
# A named entry always wins over `default`; see txeco_threshold().
#
# Example - a laxer cutoff for a sparsely sampled objective, stricter for a
# well-sampled one:
#   min_txeco_n <- c(default = 300, decrease_shr = 150, decrease_afg_AFG = 500)
#
# Each objective's section in 2_sample_size_checks.Rmd quotes its own resolved
# threshold, and the filtering log compiled in 2_make_DART_results.Rmd records
# which threshold each objective was given and what it cost - worth reading
# before comparing objectives to each other, since a compiled table built on
# unequal thresholds is comparing unequally filtered samples.
min_txeco_n <- c(default = 10, decrease_afg_AFG = 300, increase_pfg_PFG = 300, increase_shr_SHR = 300, decrease_shr_SHR = 300)

# Whether to drop a whole treatment or ecoregion LEVEL that the cell filter
# above has left below the same threshold. The cell filter works on (treatment x
# ecoregion) combinations, so it cannot see a level that has been thinned across
# many surviving cells rather than emptied out of any one of them - a treatment
# can lose most of its cells and be left with a scattering of pixels without any
# single dropped cell looking unusual.
#
# The threshold is deliberately the same `min_txeco_n` number rather than a
# second knob, so each objective has one sample-size floor rather than two to
# reconcile. Applied by drop_depopulated_levels(), one pass: treatment levels
# first, then ecoregion totals recomputed on the reduced frame, not iterated to
# a fixed point. What it removed per objective is in the filtering log's
# `# Tx Depopulated` / `# Eco Depopulated` columns, which are 0 when this is
# FALSE.
drop_depopulated <- TRUE

# Minimum sample size per YEAR SINCE TREATMENT. Keyed and resolved exactly like
# `min_txeco_n` (see keyed_threshold()), with a `default` entry and optional
# per-objective overrides. Applied by truncate_late_years(): the first year
# below the threshold is found and that year together with EVERY YEAR AFTER IT
# is dropped, whether or not a later year happens to clear the threshold again.
#
# Truncation rather than per-year exclusion, because the per-year sample size
# falls monotonically as polygons drop out of the post-treatment record - a year
# that fails marks where the record stops supporting a comparison, and a later
# year that clears it is a blip inside an already-unreliable tail. Dropping the
# whole tail keeps the x axis of every per-year figure meaning one thing.
#
# `year_trunc_metric` decides what is counted, and the choice matters more than
# the threshold value does:
#   'significant' - significant pixels in that year. This is the quantity the
#      problem is about: mean_effect_by_year() and peak_effect_year() average
#      over significant pixels only, so a year with a thousand pixels and four
#      significant ones still produces a published peak backed by four.
#   'pixels' - every pixel observed in that year, the direct analogue of
#      `min_txeco_n`. On this data the per-year totals stay in the hundreds to
#      the end of every objective's record (the smallest is 44), so a small
#      total-pixel threshold truncates nothing at all.
min_year_n        <- c(default = 10)
year_trunc_metric <- 'significant'

# ---- read ---------------------------------------------------------------

# Read in the combined filter input data and split by objective. Each element of [split_by_objective()]
# is named as `[direction]_[cover]_[RESPONSE]`, where the combination of direction and cover
# describes a treatment **objective**, and `RESPONSE` describes the (differenced) RAP cover response variable.

# Keep the raw combined data around (not just the per-objective `summaries` list).
#
# Only these nine of the input file's thirty columns are referenced anywhere in
# the two report documents, helper_functions.R or plot_functions.R; the
# remaining twenty-one (the covariates, the CI bounds, the coordinates and the
# binary treatment indicators) belong to the modelling stage. Reading only what
# is used keeps the 3.2 GB file to about half a GB in memory. Anything added to
# either document that needs a further column must add it here first, or it will
# be silently absent rather than erroring on read.
in_cols <- c('effect', 'sig', 'polygon', 'us_l4name', 'pixel',
             'fun_group', 'objective', 'year_diff', 'tx_coarse')
in_hdr  <- names(read.csv(in_fl, nrows = 1))
stopifnot(all(in_cols %in% in_hdr))
combined_df <- read.csv(in_fl,
                        colClasses = ifelse(in_hdr %in% in_cols, NA, 'NULL'))

# ---- split --------------------------------------------------------------

# Objectives dropped from `summaries`:
#   - "null_afg": not a treatment objective (control/no-op)
#   - "increase_afg", "increase_tre": out of scope for this round of results
summaries <- combined_df |>
  split_by_objective() |>
  lapply(get_summary_DART_results) |>
  (\(xx) subset(xx, !grepl('null_afg|^increase_afg_|^increase_tre_', names(xx))))()

# Polygon inventory across every element of `summaries`, used by each objective's
# summary table for the "# / % of time this was the objective" columns. Built
# here, on the unfiltered split, so that the denominator is the same for every
# objective regardless of which document is being rendered or in what order its
# sections appear.
#
# Each list element's name is "<objective>_<FUN_GROUP>", where FUN_GROUP is just
# the uppercased tail of the objective itself (see split_by_objective()), so the
# suffix is stripped back off to get the objective on its own before tallying.
poly_by_elem   <- sapply(summaries, function(x) length(unique(x$input$polygon)))
obj_of_elem    <- sub('_[A-Z]+$', '', names(summaries))
poly_by_obj    <- tapply(poly_by_elem, obj_of_elem, sum)
total_poly_all <- sum(poly_by_elem)

# ---- derive -------------------------------------------------------------

# One pass per element of `summaries`: resolve that objective's two thresholds,
# apply the three sample-size filters in order, and build the nine tables that
# are compiled across objectives.
#
# This used to live in the per-objective template, which ran inside the report
# and wrote back to the parent's `summaries_mod`. It is here now because the
# document that DISPLAYS the compiled tables is no longer the document that
# describes the per-objective filtering, and one implementation feeding both
# beats two implementations that have to be kept in step.
#
# `summaries` is left untouched; `summaries_mod` carries the post-filter copies.
# Everything either document compiles across objectives reads `summaries_mod` or
# the flat per-objective tables assigned at the end of the loop.
summaries_mod <- summaries
obj_state     <- list()

for (.i in seq_along(summaries)) {

  obj_nm    <- names(summaries)[.i]
  cur_obj   <- sub('_[A-Z]+$', '', obj_nm)
  cur_cover <- sub('^.*_', '', obj_nm)

  # Which raw sign of `effect` this objective was aiming for, in words - used by
  # the checks file's text, its figure subtitles, and the stacking order of its
  # direction figure. See get_intended_sign() and the terminology section of
  # 2_make_DART_results.Rmd.
  int_word <- intended_dir_word(cur_obj)
  uni_word <- unintended_dir_word(cur_obj)

  # The two thresholds for THIS objective x cover response, looked up in the
  # keyed vectors set above (see txeco_threshold() / year_threshold()). The
  # resolved scalars get their own names rather than overwriting the lookup
  # vectors, which would leave every later objective inheriting one objective's
  # number.
  min_txeco_n_i <- txeco_threshold(obj_nm, min_txeco_n)
  min_year_n_i  <- year_threshold(obj_nm, min_year_n)

  s_0 <- summaries[[.i]]

  # First filter: sparse (treatment x ecoregion) cells. `excl_combo` is also
  # what the checks file lists as the combinations it is about to drop, and it
  # recomputes it from the same table and threshold rather than reading it back
  # from here - the two agree by construction.
  tbl_combo <- s_0$tbl_tx_eco
  low_idx   <- which(tbl_combo > 0 & tbl_combo < min_txeco_n_i, arr.ind = TRUE)

  excl_combo <- data.frame(
    tx_coarse = rownames(tbl_combo)[low_idx[, 'row']],
    us_l4name = colnames(tbl_combo)[low_idx[, 'col']],
    stringsAsFactors = FALSE
  )

  n_pix_lost_combo <- sum(tbl_combo[low_idx])
  pct_lost_combo   <- round(100 * n_pix_lost_combo / sum(tbl_combo), 2)
  n_excl_combo     <- nrow(excl_combo)

  tx_bef  <- unique(s_0$input$tx_coarse)
  eco_bef <- unique(s_0$input$us_l4name)

  s0_summary_bef <- reduce_s0(s_0$input)
  n_pix_bef      <- sum(s0_summary_bef$grp_n_pix)

  trimmed_input <- dplyr::anti_join(s_0$input, excl_combo, by = c('tx_coarse', 'us_l4name'))

  # A threshold set high enough to exclude every (treatment x ecoregion)
  # combination leaves nothing to summarize. Caught here because the failure is
  # otherwise both late and silent: get_summary_DART_results() errors on a
  # zero-row frame several calls down with an opaque names() message. The fix is
  # a one-line edit to `min_txeco_n` above, so the message names the objective
  # and the threshold responsible.
  if (nrow(trimmed_input) == 0) {
    stop(sprintf(
      'min_txeco_n = %s excludes all %d treatment x ecoregion combinations for "%s"; lower the threshold for this objective in 2_results_setup.R.',
      min_txeco_n_i, nrow(excl_combo), obj_nm
    ))
  }

  # Second filter: whole treatment or ecoregion levels left below the same
  # threshold once their sparse cells are gone. The cell-level filter above works
  # on (treatment x ecoregion) combinations and so cannot see a level that has
  # been thinned across many surviving cells rather than emptied out of any one
  # of them. Controlled by `drop_depopulated`, because whether a thinned level
  # should be analysed at reduced power or removed is a judgement rather than a
  # fact about the data.
  #
  # The attribute is read off immediately: get_summary_DART_results() below
  # rebuilds from the frame and the attribute does not survive that.
  if (drop_depopulated) {
    trimmed_input <- drop_depopulated_levels(trimmed_input, min_txeco_n_i)
    depop <- attr(trimmed_input, 'levels_dropped')
  } else {
    depop <- list(tx_coarse = character(0), us_l4name = character(0))
  }

  # Third filter: the late-year tail. Everything above is cross-sectional - it
  # says nothing about the per-year sample size, which falls monotonically as
  # polygons drop out of the post-treatment record. This truncates the series at
  # the first year that cannot support a comparison and drops everything after
  # it. It happens LAST so that the per-year counts it tests are the ones the
  # figures will actually be drawn from, after both spatial filters.
  trimmed_input <- truncate_late_years(
    trimmed_input, min_year_n_i, metric = year_trunc_metric
  )
  year_trunc <- attr(trimmed_input, 'year_trunc')

  s_0f <- get_summary_DART_results(trimmed_input)

  s0_summary_aft <- reduce_s0(s_0f$input)
  n_pix_aft      <- sum(s0_summary_aft$grp_n_pix)
  pct_lost_trim  <- round(((n_pix_bef - n_pix_aft) / n_pix_bef) * 100, 2)

  summaries_mod[[.i]] <- s_0f

  # A treatment or ecoregion can vanish entirely if it only ever appeared in
  # sparse combinations - checked here, separately from the overall percent lost.
  dropped_tx  <- setdiff(tx_bef,  unique(s_0f$input$tx_coarse))
  dropped_eco <- setdiff(eco_bef, unique(s_0f$input$us_l4name))

  # --- the nine tables compiled across objectives ---
  #
  # Each is assigned below under `<prefix><objective>_<COVER>`, the name
  # collect_obj_tables() matches on. Adding an objective to the input data adds
  # its row to every compiled table without any compilation chunk changing.

  # Filtering log: which thresholds this objective was given and what they cost.
  # `pct_pix_lost` is the TOTAL loss across all three filters, not the cell
  # filter alone: it compares reduce_s0() before any filtering against
  # reduce_s0() after all of it. The per-filter detail is in the columns either
  # side of it - n_combo_excluded for the cell filter, n_tx_depop / n_eco_depop
  # for the depopulated-level drop, and n_year_dropped for the truncation.
  #
  # n_tx_dropped / n_eco_dropped are a different quantity from n_tx_depop /
  # n_eco_depop: the first pair counts levels ABSENT from the post-filter data
  # for any reason (including having only ever appeared in sparse cells), the
  # second counts levels removed deliberately by drop_depopulated_levels(). With
  # `drop_depopulated = FALSE` the second pair is 0 and the first can still be
  # non-zero.
  filter_log <- data.frame(
    objective        = cur_obj,
    cover            = cur_cover,
    min_txeco_n      = min_txeco_n_i,
    n_combo_excluded = n_excl_combo,
    n_tx_depop       = length(depop$tx_coarse),
    n_eco_depop      = length(depop$us_l4name),
    min_year_n       = min_year_n_i,
    year_trunc_at    = year_trunc$trunc_at,
    n_year_dropped   = year_trunc$n_dropped,
    pct_pix_lost     = pct_lost_trim,
    n_tx_dropped     = length(dropped_tx),
    n_eco_dropped    = length(dropped_eco),
    stringsAsFactors = FALSE
  )

  # Post-filter treatment x ecoregion pixel counts in long form, tagged with the
  # coarse objective and the cover response. Displayed nowhere; it exists to be
  # concatenated across objectives and written to .csv.
  txeco_post <- as.data.frame(
    table(s_0f$input$tx_coarse, s_0f$input$us_l4name),
    stringsAsFactors = FALSE
  )
  colnames(txeco_post) <- c('tx_coarse', 'us_l4name', 'n_pix')
  txeco_post <- txeco_post[txeco_post$n_pix > 0, ]
  txeco_post <- data.frame(
    objective = cur_obj,
    cover     = cur_cover,
    txeco_post,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # The three overall percentages as a one-row table, carrying the objective and
  # cover response in their own columns.
  sig_overall <- summarize_sig_overall(s_0f$input, cur_obj, cur_cover)

  # The same total expressed as intended versus unintended rather than as raw
  # sign.
  int_effect <- summarize_intended_effect(s_0f$input, cur_obj, cur_cover)

  # Per-polygon significance: the mean, SD and SE across POLYGONS of each
  # polygon's own percent-significant, rather than a pixel-level share. Built
  # here with the other eight rather than in the report document, where it used
  # to be computed in a loop over `summaries_mod` - the one place a document
  # derived a quantity it displayed, against this script's whole reason for
  # existing. Moving it also puts its rows in the same order as every other
  # compiled table, since they now all come back through collect_obj_tables().
  poly_sig <- summarize_poly_sig(s_0f$input, cur_obj, cur_cover)

  # Mean effect over significant pixels per year, and the peak year per
  # direction. `effect_by_year` is kept alongside the peak table because the
  # checks file draws the figure from it and hands it the peak table, so the
  # dashed peak lines cannot disagree with the printed peaks.
  effect_by_year <- mean_effect_by_year(s_0f$input)
  peak_effect    <- peak_effect_year(effect_by_year)

  # 5/10/15+ year bins for % significant pixels. The binned column is added to a
  # local copy rather than to `s_0f$input`, so the data frame written into
  # `summaries_mod` above stays as the analysis produced it and doesn't pick up a
  # reporting-only column.
  df_bin <- s_0f$input
  df_bin$year_bin <- cut(
    df_bin$year_diff,
    breaks = c(0, 5, 10, 15, Inf),
    labels = c('1-5', '6-10', '11-15', '16+'),
    right = TRUE
  )

  sig_by_bin <- df_bin |>
    dplyr::group_by(year_bin) |>
    dplyr::summarise(
      n_pix            = dplyr::n(),
      mean_pct_sig     = round(100 * mean(sig), 2),
      mean_pct_sig_pos = round(100 * mean(sig & effect > 0), 2),
      mean_pct_sig_neg = round(100 * mean(sig & effect < 0), 2),
      .groups = 'drop'
    ) |>
    dplyr::mutate(objective = cur_obj, cover = cur_cover, .before = 1) |>
    as.data.frame()

  # How often each OTHER objective was also assigned to the same pixel. The solo
  # pixels are carried as an explicit "none" row rather than in a caption, so the
  # share reads off the same column as the co-occurring shares.
  overlap_tbl <- objective_overlap_table(s_0f$input, cur_obj, cur_cover)

  # "% or # of time this was the objective": out of all polygons across every
  # element of `summaries`, what share belonged to this objective?
  n_time_obj   <- unname(poly_by_obj[cur_obj])
  pct_time_obj <- round(100 * n_time_obj / total_poly_all, 2)

  # "# / % of time this was the treatment": within THIS objective's own
  # post-filter data, how many of its polygons received each treatment, across
  # all ecoregions? Deliberately a percentage of `n_time_obj` (the objective's
  # own polygon count), not of `total_poly_all` - a polygon that received more
  # than one treatment across its pixels is counted under more than one
  # treatment, so these percentages are not expected to sum to 100.
  poly_by_tx <- s_0f$input |>
    dplyr::group_by(tx_coarse) |>
    dplyr::summarise(n_time_treatment = length(unique(polygon)), .groups = 'drop') |>
    dplyr::mutate(pct_time_treatment = round(100 * n_time_treatment / n_time_obj, 2))

  # `fun_group` is renamed to `cover` below purely so that this table carries
  # both an `objective` and a `cover` column: collect_obj_tables() only prepends
  # its own `objective_cover` source column to tables that lack that pair, and an
  # extra leading column here would put the compiled table one column out of step
  # with the col.names given in the report's summary_table chunk.
  summary_tbl <- s0_summary_aft |>
    dplyr::left_join(poly_by_tx, by = 'tx_coarse') |>
    dplyr::mutate(
      n_time_objective   = n_time_obj,
      pct_time_objective = pct_time_obj
    ) |>
    dplyr::select(
      n_time_objective, pct_time_objective,
      objective, cover = fun_group, tx_coarse, n_time_treatment, pct_time_treatment, us_l4name,
      grp_n_pix, grp_n_poly, grp_n_sig, grp_n_sig_int, grp_n_sig_uni, grp_perc_int, grp_perc_uni
    ) |>
    as.data.frame()

  # Everything the checks file's section for this objective needs in order to
  # describe the filtering without redoing it. The section unpacks these into
  # bare local names, so its prose can go on quoting `min_txeco_n_i`,
  # `year_trunc`, `int_word` and the rest by name.
  obj_state[[obj_nm]] <- list(
    r_ind            = .i,
    obj_nm           = obj_nm,
    cur_obj          = cur_obj,
    cur_cover        = cur_cover,
    int_word         = int_word,
    uni_word         = uni_word,
    min_txeco_n_i    = min_txeco_n_i,
    min_year_n_i     = min_year_n_i,
    excl_combo       = excl_combo,
    n_excl_combo     = n_excl_combo,
    n_pix_lost_combo = n_pix_lost_combo,
    pct_lost_combo   = pct_lost_combo,
    depop            = depop,
    year_trunc       = year_trunc,
    pct_lost_trim    = pct_lost_trim,
    dropped_tx       = dropped_tx,
    dropped_eco      = dropped_eco,
    s0_summary_bef   = s0_summary_bef,
    s0_summary_aft   = s0_summary_aft,
    filter_log       = filter_log,
    txeco_post       = txeco_post,
    sig_overall      = sig_overall,
    poly_sig         = poly_sig,
    int_effect       = int_effect,
    effect_by_year   = effect_by_year,
    peak_effect      = peak_effect,
    sig_by_bin       = sig_by_bin,
    overlap_tbl      = overlap_tbl,
    summary_tbl      = summary_tbl
  )

  # The flat copies collect_obj_tables() matches on. Assigned at the top level of
  # this script's evaluation environment, which is the knit environment when the
  # script is sourced with `local = knitr::knit_global()`.
  assign(paste0('filter_log_',  obj_nm), filter_log)
  assign(paste0('txeco_post_',  obj_nm), txeco_post)
  assign(paste0('sig_overall_', obj_nm), sig_overall)
  assign(paste0('poly_sig_',    obj_nm), poly_sig)
  assign(paste0('int_effect_',  obj_nm), int_effect)
  assign(paste0('peak_effect_', obj_nm), peak_effect)
  assign(paste0('sig_by_bin_',  obj_nm), sig_by_bin)
  assign(paste0('overlap_tbl_', obj_nm), overlap_tbl)
  assign(paste0('summary_tbl_', obj_nm), summary_tbl)
}

# Clear the loop's working variables. This is load-bearing, not tidiness: each
# section of 2_sample_size_checks.Rmd unpacks `obj_state[[obj_nm]]` into exactly
# these bare names, and if one were left behind here, a section that forgot to
# unpack it would silently read the LAST objective's value instead of failing.
# An object-not-found error inside a knit names the variable; a wrong number in a
# rendered table does not. The big frames (`trimmed_input`, `s_0`, `s_0f`) go
# with them, which also hands back their memory.
rm(list = intersect(
  c('.i', 'obj_nm', 'cur_obj', 'cur_cover', 'int_word', 'uni_word',
    'min_txeco_n_i', 'min_year_n_i', 's_0', 's_0f', 'tbl_combo', 'low_idx',
    'excl_combo', 'n_pix_lost_combo', 'pct_lost_combo', 'n_excl_combo',
    'tx_bef', 'eco_bef', 's0_summary_bef', 'n_pix_bef', 'trimmed_input',
    'depop', 'year_trunc', 's0_summary_aft', 'n_pix_aft', 'pct_lost_trim',
    'dropped_tx', 'dropped_eco', 'filter_log', 'txeco_post', 'sig_overall', 'poly_sig',
    'int_effect', 'effect_by_year', 'peak_effect', 'df_bin', 'sig_by_bin',
    'overlap_tbl', 'n_time_obj', 'pct_time_obj', 'poly_by_tx', 'summary_tbl'),
  ls()
))

# ---- non-target cover responses -----------------------------------------

# The three cross-objective direction checks, stacked into one table with an
# `objective` column. This asks what the OTHER cover responses did in pixels
# where a given objective was being pursued - e.g. what AFG, SHR and PFG cover
# did where the goal was to decrease tree cover - which split_by_objective()
# cannot answer, because it only ever pairs an objective with its own matching
# cover response.
#
# Computed here rather than in a document, like everything else. The three
# calls have existed since the report was first written, but as chunks inside
# 2_make_DART_results.Rmd, which made that document the one place a .Rmd
# computed what it displayed - the same defect that moved `poly_sig_` into this
# script on 07 Oct 2026. The interpretation document needs them stacked across
# objectives for its community-response figure, so they are built once here and
# both documents read the result.
#
# The cost falls on the two documents that display none of this: three grepl()
# passes over `combined_df` and a grouped summarise, a few seconds each. That is
# the price of the seam, and it is worth paying - the alternative is two
# definitions of the same table drifting apart, which is what the `decrease_shb`
# misspelling cost the first time these calls were written out by hand.
#
# Which non-target responses go with which objective is a judgement about what
# is ecologically interesting, not something derivable from the data, so it is
# written out as a list rather than generated: every cover response except the
# objective's own target would include pairings nobody has a question about.
nontarget_spec <- list(
  decrease_tre = c('AFG', 'SHR', 'PFG'),
  increase_pfg = c('AFG'),
  decrease_shr = c('AFG', 'PFG', 'TRE')
)

nontarget_all <- do.call(rbind, lapply(names(nontarget_spec), function(i_obj) {
  r_0 <- compare_related_directions(combined_df, i_obj, nontarget_spec[[i_obj]])
  data.frame(objective = i_obj, r_0$table, stringsAsFactors = F)
}))
row.names(nontarget_all) <- NULL
