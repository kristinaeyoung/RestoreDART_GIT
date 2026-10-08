fig_conventions <- function() {
  # The report's figure conventions in one place: the direction colours, the
  # base themes, the text colours and sizes, and the handful of label strings
  # that are shared between figures. Takes no arguments and returns a named
  # list. Every plotting function in this file calls it and reads its values off
  # the list, so the list is the operative definition of the conventions rather
  # than a description of them - changing a value here changes the figures, and a
  # figure cannot drift from it without the drift showing up as a literal sitting
  # in the plotting function.
  #
  # Two kinds of size live under `$size` and are easy to confuse. Theme text
  # sizes (`strip_text`, `axis_text_small`) are in POINTS; geom sizes (`label_*`,
  # `point*`, `pointrange`) are ggplot2's own units, i.e. millimetres. Axis text
  # keeps ggplot's default size everywhere except the dense heatmap, so only that
  # exception is named. The three label sizes are deliberately not collapsed into
  # one: they differ by what the label has to fit into - a group total above a
  # bar, a segment label inside one, a multi-line peak annotation beside a point.
  #
  # `$theme` carries three ready-made theme objects rather than one, matching the
  # three groups the figures fall into: unfacetted, facetted, and facetted with a
  # legend title that has to read as a heading. A figure needing one further
  # tweak adds its own `theme()` call after one of these, rather than a fourth
  # entry being added here for a single caller.
  #
  # Deliberately NOT here: the per-figure scale expansions, dodge and error-bar
  # widths, and the minimum segment height below which an inside-bar label is
  # dropped. Each is tuned to one figure's own label geometry and has exactly one
  # caller, so promoting it to a report-wide convention would misrepresent it.

  require(ggplot2)

  col <- list(
    pos          = '#b2182b',  # significant positive, in every figure
    neg          = '#2166ac',  # significant negative, in every figure
    overall      = 'grey20',   # the undifferentiated 'overall' series
    neutral      = 'grey35',   # a single-series bar or histogram with no direction
    mid          = 'grey90',   # the zero point of the diverging heatmap fill
    text         = 'black',    # axis text and facet strip text
    label_inside = 'white',    # a label drawn inside a filled bar segment
    separator    = 'white',    # tile borders and bar outlines
    open_fill    = 'white',    # interior of a hollow (censored) point
    seq          = c('grey20', 'grey40', 'grey60')
    # a sequential ramp for an UNORDERED multi-level fill carrying no direction
    # (the variance components). Added rather than left as a literal in the one
    # figure that needs it, because the existing entries do not cover the case:
    # `mid` (grey90) is the zero point of the diverging heatmap fill, and a
    # grey90 segment cannot carry a `label_inside` label. Every value here is
    # dark enough that white text on it stays readable, which is the property
    # that makes it a ramp rather than three arbitrary greys.
  )

  size <- list(
    axis_text_small = 7,       # pt; the dense heatmap only
    strip_text      = 8,       # pt
    label_n         = 2.2,     # mm; a sample size beside or inside a geom
    label_n_total   = 2.5,     # mm; a group total above a bar
    label_peak      = 2.3,     # mm; the multi-line peak annotation
    label_tile      = 2,       # mm; a count printed on a heatmap tile
    point           = 1.6,     # mm
    point_open      = 2.4,     # mm
    pointrange      = 0.6      # mm
  )

  dir_lab <- c(pos = 'Significant positive', neg = 'Significant negative')

  th_base  <- theme_bw() + theme(axis.text = element_text(color = col$text))
  th_facet <- th_base +
    theme(strip.text = element_text(color = col$text, size = size$strip_text))

  list(
    col  = col,
    size = size,
    dir  = list(
      lab    = dir_lab,
      levels = unname(dir_lab),
      pal    = setNames(c(col$pos, col$neg), unname(dir_lab))
    ),
    line = list(
      zero      = 0.3,  # the y = 0 reference line
      annot     = 0.4,  # an error bar or a dashed peak line
      series    = 0.6,  # a data series line
      separator = 0.2   # a tile border or bar outline
    ),
    alpha = list(
      ribbon = 0.2,     # an SE ribbon around a series
      faded  = 0.3      # a cell below the sample-size floor
    ),
    lab = list(
      n_prefix   = 'n = ',  # sample sizes print as 'n = 1234', without parentheses
      dir_legend = 'Direction',
      sig_legend = 'DART result\nsignificant?',
      pct_sig_y  = '% of pixels with a significant effect'
    ),
    theme = list(
      base         = th_base,
      facet        = th_facet,
      facet_legend = th_facet + theme(legend.title = element_text(face = 'bold'))
    )
  )
}
plot_sig_direction_stacked <- function(
    df, group_col, sig_col = 'sig', effect_col = 'effect',
    reverse_stack = FALSE, x_lab = group_col, title = NULL, subtitle = NULL,
    show_n = TRUE, show_n_direction = TRUE, x_break_by = NULL
) {
  # Stacked bar chart breaking the overall "% significant" apart into its
  # significant-positive and significant-negative components (rather than one
  # undifferentiated bar), for whatever grouping variable is passed as `group_col`
  # (e.g. `year_diff`).
  #
  # Stacking order / color: the fill color is always tied to direction
  # ("Significant positive" vs "Significant negative"), regardless of stacking
  # order, so the color scheme never changes between calls. By default
  # (reverse_stack = FALSE), "Significant negative" is drawn adjacent to the
  # x-axis and "Significant positive" on top - this is the natural default for
  # e.g. `decrease_afg`, where a negative effect is the intended one. Pass
  # reverse_stack = TRUE to flip which segment sits at the axis (e.g. for an
  # `increase_*` objective) without touching the color mapping.
  #
  # `x_break_by` sets the spacing of the x-axis breaks when the grouping variable
  # is numeric (e.g. `x_break_by = 5` labels every fifth year since treatment,
  # rather than leaving ggplot's default spacing to label every tenth). Breaks
  # start at the first multiple of `x_break_by` at or above the smallest group
  # value, so no label is drawn outside the range of the data. It is ignored for
  # a non-numeric grouping variable, which gets a discrete axis instead.
  #
  # Sample size: labels are drawn rotated 90 degrees (reading bottom-to-top), so
  # that a long "n = XXXXX" string fits between neighbouring bars instead of
  # colliding with them. Two levels of sample size are shown. `show_n` puts the
  # group total above the top of the bar - XX is every pixel in that group, not
  # just the significant ones, so the reader can see how much data backs the
  # bar. `show_n_direction` additionally labels each stacked segment with its
  # own count: the number of pixels that were significant *and* positive, and
  # the number that were significant *and* negative. Those two are the sample
  # sizes behind the two segment heights themselves (the group total is not,
  # since most of it is usually non-significant), and they sum to the
  # significant pixels in the group rather than to the group total.

  require(dplyr)
  require(ggplot2)
  require(tidyr)

  fc <- fig_conventions()

  grp_summary <- df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_pos   = sum(.data[[sig_col]] & .data[[effect_col]] > 0),
      n_neg   = sum(.data[[sig_col]] & .data[[effect_col]] < 0),
      pct_pos = 100 * mean(.data[[sig_col]] & .data[[effect_col]] > 0),
      pct_neg = 100 * mean(.data[[sig_col]] & .data[[effect_col]] < 0),
      .groups = 'drop'
    )

  plot_df <- grp_summary |>
    tidyr::pivot_longer(
      cols = c(pct_pos, pct_neg),
      names_to = 'direction', values_to = 'pct'
    ) |>
    dplyr::mutate(
      direction = factor(
        ifelse(direction == 'pct_pos', fc$dir$lab[['pos']], fc$dir$lab[['neg']]),
        levels = fc$dir$levels
      )
    )

  label_df <- grp_summary |>
    dplyr::mutate(
      y_lab = pct_pos + pct_neg,
      lab   = paste0(fc$lab$n_prefix, n_total)
    )

  # Per-segment sample sizes, one row per group x direction. The y position is
  # the midpoint of that segment, computed here rather than delegated to
  # position_stack() so the labels follow `reverse_stack`: whichever direction
  # sits at the axis is centred at half its own height, and the other is centred
  # half its own height above the first. Segments with no significant pixels are
  # dropped so an "n = 0" label isn't stacked on top of the segment above it.
  y_pos <- if (reverse_stack) grp_summary$pct_pos / 2 else grp_summary$pct_neg + grp_summary$pct_pos / 2
  y_neg <- if (reverse_stack) grp_summary$pct_pos + grp_summary$pct_neg / 2 else grp_summary$pct_neg / 2

  label_dir_df <- rbind(
    data.frame(
      .grp = grp_summary[[group_col]], direction = fc$dir$lab[['pos']],
      n_dir = grp_summary$n_pos, y_lab = y_pos, stringsAsFactors = FALSE
    ),
    data.frame(
      .grp = grp_summary[[group_col]], direction = fc$dir$lab[['neg']],
      n_dir = grp_summary$n_neg, y_lab = y_neg, stringsAsFactors = FALSE
    )
  )
  label_dir_df$lab <- paste0(fc$lab$n_prefix, label_dir_df$n_dir)
  # Segment labels are drawn in white, inside the segment, so a segment too short
  # to contain its label would put white text on the white panel background (i.e.
  # invisibly) and, where both segments are short, overlap the other segment's
  # label. Those are dropped: the group total above the bar still reports how much
  # data is behind it, and the table accompanying this figure carries the exact
  # per-direction counts for every group.
  label_dir_df$.h <- c(grp_summary$pct_pos, grp_summary$pct_neg)
  min_h <- 0.06 * max(grp_summary$pct_pos + grp_summary$pct_neg)
  label_dir_df <- label_dir_df[label_dir_df$n_dir > 0 & label_dir_df$.h >= min_h, ]
  names(label_dir_df)[names(label_dir_df) == '.grp'] <- group_col

  p0 <- plot_df |>
    ggplot(aes(x = .data[[group_col]], y = pct, fill = direction)) +
    geom_col(position = position_stack(reverse = reverse_stack)) +
    scale_fill_manual(values = fc$dir$pal) +
    labs(
      x = x_lab, y = fc$lab$pct_sig_y,
      fill = fc$lab$dir_legend,
      title = if (is.null(title)) 'Significant DART effects, by direction' else title,
      subtitle = subtitle
    ) +
    fc$theme$base

  if (show_n) {
    p0 <- p0 + geom_text(
      data = label_df, aes(x = .data[[group_col]], y = y_lab, label = lab),
      inherit.aes = FALSE, angle = 90, hjust = -0.1, size = fc$size$label_n_total
    )
  }

  if (show_n_direction) {
    p0 <- p0 + geom_text(
      data = label_dir_df, aes(x = .data[[group_col]], y = y_lab, label = lab),
      inherit.aes = FALSE, angle = 90, hjust = 0.5, size = fc$size$label_n, colour = fc$col$label_inside
    )
  }

  # With the group-total labels rotated upright they need vertical headroom that
  # the default scale expansion doesn't leave, or the tallest bar's label gets
  # clipped at the top of the panel.
  if (show_n) {
    p0 <- p0 + scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))
  }

  # Label every `x_break_by`-th group rather than accepting ggplot's default
  # break spacing (which, over a ~30-year record, labels only every tenth year).
  if (!is.null(x_break_by) && is.numeric(grp_summary[[group_col]])) {
    x_vals <- grp_summary[[group_col]]
    p0 <- p0 + scale_x_continuous(
      breaks = seq(ceiling(min(x_vals) / x_break_by) * x_break_by, max(x_vals), by = x_break_by)
    )
  }

  return(p0)

}
plot_poly_sig_summary <- function(
    tbl, metric = c('all', 'overall', 'positive', 'negative'),
    title = NULL, subtitle = NULL, dodge_width = 0.25
) {
  # Takes the expandable per-objective/cover summary table built by
  # summarize_poly_sig() (helper_functions.R) - one row per objective/cover
  # combination, with mean/SD/SE of % significant pixels taken across polygons -
  # and plots it as a point-range chart (mean +/- SE) so multiple objectives can
  # be compared on one figure as they're added to the report.
  #
  # `metric` selects which of the three per-polygon rates to draw. The default,
  # 'all', puts the overall rate and both direction-specific rates on the SAME
  # figure, dodged side by side within each objective, which is what makes the
  # decomposition readable: the overall point is (by construction) the sum of
  # the positive and negative points, so seeing all three together shows at a
  # glance which direction is carrying a given objective's significance rate.
  # The single-metric options are kept for a one-series version of the same
  # figure. Direction colours match plot_sig_direction_stacked() so a positive
  # or negative series means the same thing in both figures.
  #
  # `title` and `subtitle` override the default title, which names only the
  # `metric` drawn. Once the table carries more than one objective the caller
  # usually wants to say which objective(s) the figure covers, which this
  # function has no way to know; pass it in rather than editing the default.
  #
  # `dodge_width` is the horizontal offset between the three metrics within one
  # objective. It is deliberately smaller than the ggplot default: with every
  # objective x cover response on one figure, each objective gets a narrower slot
  # on the x-axis, and a wide dodge pushes a metric far enough off its own
  # objective's tick to be read against the neighbouring one. Keeping the three
  # points visibly clustered is what makes the decomposition (overall = positive
  # + negative) readable across many objectives.

  require(ggplot2)

  fc <- fig_conventions()

  metric <- match.arg(metric)

  metric_cols <- list(
    overall  = c(mean = 'mean_pct_sig',     se = 'se_pct_sig'),
    positive = c(mean = 'mean_pct_sig_pos', se = 'se_pct_sig_pos'),
    negative = c(mean = 'mean_pct_sig_neg', se = 'se_pct_sig_neg')
  )
  metric_pal <- c(overall = fc$col$overall, positive = fc$col$pos, negative = fc$col$neg)
  metric_lab <- c(overall = 'Overall', positive = fc$dir$lab[['pos']], negative = fc$dir$lab[['neg']])

  keep <- if (metric == 'all') names(metric_cols) else metric

  plot_df <- do.call(rbind, lapply(keep, function(m) {
    data.frame(
      .label  = paste0(tbl$objective, '\n(', tbl$cover, ')'),
      .metric = factor(metric_lab[[m]], levels = unname(metric_lab)),
      .mean   = tbl[[metric_cols[[m]][['mean']]]],
      .se     = tbl[[metric_cols[[m]][['se']]]],
      stringsAsFactors = FALSE
    )
  }))

  y_lab <- if (metric == 'all') {
    'Mean % significant, ± SE across polygons'
  } else {
    paste0('Mean % significant (', metric, '), ± SE across polygons')
  }

  p0 <- plot_df |>
    ggplot(aes(x = .label, y = .mean, colour = .metric)) +
    geom_pointrange(
      aes(ymin = .mean - .se, ymax = .mean + .se),
      size = fc$size$pointrange, position = position_dodge(width = dodge_width)
    ) +
    scale_colour_manual(values = setNames(unname(metric_pal), unname(metric_lab)), name = 'Metric') +
    labs(
      x = 'Objective (cover response)', y = y_lab,
      title = if (is.null(title)) paste0('Per-polygon significance summary: ', metric) else title,
      subtitle = subtitle
    ) +
    fc$theme$base

  # A single-metric call has nothing to distinguish, so the legend is dropped and
  # the figure looks as it did before 'all' existed.
  if (metric != 'all') p0 <- p0 + guides(colour = 'none')

  # Two-line "objective\n(cover)" labels run into each other once there are more
  # than a handful of objectives on the axis, so they are tilted at that point
  # only - a single-objective figure keeps its horizontal label.
  if (nrow(tbl) > 4) {
    p0 <- p0 + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = fc$col$text))
  }

  return(p0)

}
plot_model_resid <- function(
    resid_df, title = NULL, subtitle = NULL,
    x_lab = 'Fitted value', y_lab = 'Residual') {
  # Residuals against fitted values for one fitted mixed model, as a ggplot so
  # that the object can be saved alongside the .png and re-printed by
  # 3_report_linear_models.Rmd without the model being refit.
  #
  # Takes a data frame with `fitted` and `resid` columns rather than the fitted
  # model, for two reasons: the caller is the one that knows how many points to
  # subsample (a full objective brings 10^5-10^6 residuals, and embedding them
  # in a saved ggplot object makes the .Rdata unusable), and keeping lme4 out of
  # this file leaves plot_functions.R dependent on ggplot2 alone.

  require(ggplot2)
  stopifnot(all(c('fitted', 'resid') %in% colnames(resid_df)))

  fc <- fig_conventions()

  ggplot(resid_df, aes(x = fitted, y = resid)) +
    geom_hline(yintercept = 0, linetype = 'dashed',
               linewidth = fc$line$zero, color = fc$col$text) +
    geom_point(size = fc$size$point, alpha = fc$alpha$faded,
               color = fc$col$neutral) +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    fc$theme$base
}

plot_model_qq <- function(
    resid_df, title = NULL, subtitle = NULL,
    x_lab = 'Theoretical quantile', y_lab = 'Sample quantile') {
  # Normal Q-Q plot of one model's residuals. Same argument convention as
  # plot_model_resid() - a subsampled data frame, not a fitted model - so the
  # two are drawn from one object and cannot disagree about which residuals they
  # are showing.
  #
  # The reference line is stat_qq_line()'s, drawn through the first and third
  # quartiles rather than through the origin at slope 1, which is the usual
  # convention and the one qqline() uses.

  require(ggplot2)
  stopifnot('resid' %in% colnames(resid_df))

  fc <- fig_conventions()

  ggplot(resid_df, aes(sample = resid)) +
    stat_qq(size = fc$size$point, alpha = fc$alpha$faded,
            color = fc$col$neutral) +
    stat_qq_line(linetype = 'dashed', linewidth = fc$line$annot,
                 color = fc$col$text) +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    fc$theme$base
}

plot_model_scale_location <- function(
    resid_df, title = NULL, subtitle = NULL,
    x_lab = 'Fitted value', y_lab = expression(sqrt(abs(Residual))), span = 0.5) {
  # Scale-location plot: the square root of the absolute residual against the
  # fitted value, with a loess trend. Bates, Machler, Bolker and Walker (2015,
  # JSS 67(1), section 5.2.3) name this as one of the three standard lme4
  # diagnostics alongside fitted-vs-residual and Q-Q, and note that lme4's
  # version is built on RAW rather than standardized residuals - which is what
  # this does, since `resid_df` carries resid(fit) unstandardized.
  #
  # It is the plot that shows heteroscedasticity most directly, and that is the
  # assumption most at risk here: a pixel-level DART effect is an estimate with
  # its own standard error, and that error is not constant across the range of
  # cover the pixels span. A trend line that rises with the fitted value says
  # the model's single residual variance is describing two different things.

  require(ggplot2)
  stopifnot(all(c('fitted', 'resid') %in% colnames(resid_df)))

  fc  <- fig_conventions()
  tbl <- resid_df
  tbl$sqrt_abs <- sqrt(abs(tbl$resid))

  ggplot(tbl, aes(x = fitted, y = sqrt_abs)) +
    geom_point(size = fc$size$point, alpha = fc$alpha$faded, color = fc$col$neutral) +
    geom_smooth(method = 'loess', span = span, se = FALSE,
                color = fc$col$pos, linewidth = fc$line$series) +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    fc$theme$base
}

plot_model_ranef_qq <- function(
    ranef_df, title = NULL, subtitle = NULL,
    x_lab = 'Theoretical quantile', y_lab = 'Conditional mode') {
  # Normal Q-Q of the CONDITIONAL MODES of the random effects, one panel per
  # grouping factor. Bates et al. (2015, section 5.2.3) recommend the qqmath
  # method on `ranef(fit)` output for exactly this, and it tests an assumption
  # none of the residual plots touch: the residual Q-Q asks whether the
  # within-pixel errors are normal, while this asks whether the POLYGON and
  # PIXEL intercepts are - a separate distributional assumption that the model
  # makes and that nothing else in this project checks.
  #
  # Takes `as.data.frame(ranef(fit))`, which carries `grpvar` (the grouping
  # factor), `term`, `grp` (the level), `condval` and `condsd`. Passing the
  # data frame rather than the fitted model keeps lme4 out of this file, the
  # same argument convention as plot_model_resid().

  require(ggplot2)
  stopifnot(all(c('grpvar', 'condval') %in% colnames(ranef_df)))

  fc <- fig_conventions()

  ggplot(ranef_df, aes(sample = condval)) +
    stat_qq(size = fc$size$point, alpha = fc$alpha$faded, color = fc$col$neutral) +
    stat_qq_line(linetype = 'dashed', linewidth = fc$line$annot, color = fc$col$text) +
    facet_wrap(vars(grpvar), scales = 'free_y') +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    fc$theme$facet
}

plot_model_coefs <- function(
    coef_tbl, title = NULL, subtitle = NULL,
    x_lab = 'Estimate (response units)', y_lab = NULL,
    ci_mult = 1.96, drop_intercept = T) {
  # Fixed-effect estimates with confidence intervals for every objective's
  # selected model, one panel per objective x cover response.
  #
  # `ci_mult` is the multiplier on the standard error, 1.96 by default for an
  # approximate 95% interval. It is an argument rather than a literal because
  # the interval is approximate either way - lme4 reports no denominator degrees
  # of freedom - and a reader who wants the 1-SE version should not have to edit
  # this function to get it.
  #
  # Colour marks whether that interval excludes zero, using the same two
  # direction colours as every other figure in this report (see the terminology
  # section of 2_make_DART_results.Rmd): they mean the RAW sign here, not the
  # intended one, because a panel's terms are treatment contrasts and ecoregion
  # contrasts whose relation to the objective's goal is not a property of the
  # sign alone. `coef_tbl$intended_sign` carries the goal for a reader who wants
  # to apply it.
  #
  # The y scale is free and the x scale is shared. That is the opposite of the
  # usual rule for a compiled figure, and deliberate: the objectives do not have
  # the same terms (each has its own surviving ecoregions and treatments), so a
  # shared term axis would be mostly blank, while the estimate axis - the
  # quantity being compared - stays common across panels.

  require(ggplot2)
  stopifnot(all(c('objective', 'cover', 'term', 'estimate') %in% colnames(coef_tbl)),
            all(c('lo', 'hi') %in% colnames(coef_tbl)) || 'se' %in% colnames(coef_tbl))

  fc <- fig_conventions()

  tbl <- coef_tbl
  if (drop_intercept) tbl <- tbl[tbl$term != '(Intercept)', ]
  stopifnot(nrow(tbl) > 0)

  # Use the interval the caller supplied, if it supplied one. confint() on a
  # merMod returns a real interval by Wald approximation, likelihood profiling
  # or parametric bootstrap (Bates et al. 2015, section 5.2.6), and only the
  # Wald version is the symmetric estimate +/- a multiple of the standard error
  # that the fallback below reconstructs. A profile or bootstrap interval is
  # asymmetric, and rebuilding it from the SE would silently discard exactly
  # the asymmetry that made it worth computing. `ci_mult` therefore applies
  # only to the fallback.
  if (!all(c('lo', 'hi') %in% colnames(tbl))) {
    tbl$lo <- tbl$estimate - ci_mult * tbl$se
    tbl$hi <- tbl$estimate + ci_mult * tbl$se
  }

  # drop = FALSE on the scale below, because a run in which every interval
  # overlaps zero would otherwise silently lose two legend keys.
  excl_zero  <- tbl$lo > 0 | tbl$hi < 0
  flag_lev   <- c(unname(fc$dir$lab), 'Overlaps zero')
  tbl$flag   <- factor(
    ifelse(!excl_zero, 'Overlaps zero',
           ifelse(tbl$estimate > 0, fc$dir$lab[['pos']], fc$dir$lab[['neg']])),
    levels = flag_lev
  )
  flag_pal <- setNames(c(fc$col$pos, fc$col$neg, fc$col$neutral), flag_lev)

  tbl$obj_lab <- obj_cover_label(tbl)

  # Terms are ordered by their MEAN position within each model's own coefficient
  # vector, not by first appearance across the whole table. The panels do not
  # share a term set - each objective has its own surviving ecoregions - so a
  # first-appearance ordering interleaves one objective's ecoregion contrasts
  # ahead of another's `mean_cover_5YBT`, and with a free y scale each panel
  # then shows the shared terms in a different order from its neighbour.
  # Averaging the within-model position keeps every panel close to the order
  # lmer reported and keeps the shared terms aligned between panels.
  term_pos <- ave(seq_len(nrow(tbl)), paste(tbl$objective, tbl$cover),
                  FUN = seq_along)
  term_ord <- tapply(term_pos, tbl$term, mean)
  tbl$term <- factor(tbl$term, levels = rev(names(sort(term_ord))))

  ggplot(tbl, aes(x = estimate, y = term, color = flag)) +
    geom_vline(xintercept = 0, linetype = 'dashed',
               linewidth = fc$line$zero, color = fc$col$text) +
    geom_pointrange(aes(xmin = lo, xmax = hi),
                    linewidth = fc$line$annot, size = fc$size$pointrange) +
    scale_color_manual(values = flag_pal, drop = FALSE,
                       name = fc$lab$dir_legend) +
    facet_wrap(vars(obj_lab), scales = 'free_y') +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    fc$theme$facet_legend
}

plot_model_varcomp <- function(
    vc_tbl, title = NULL, subtitle = NULL,
    x_lab = NULL, y_lab = '% of total variance', show_pct = T) {
  # How each objective's selected model partitions its variance between
  # polygons, between pixels within a polygon, and within a pixel across years.
  # One stacked bar per objective x cover response.
  #
  # This is the quantitative form of the report's standing caveat that pixels
  # inside a polygon are not independent replicates: a polygon term that takes
  # most of the variance says the effective sample size is closer to the polygon
  # count than to the pixel count.
  #
  # Fill comes from `fig_conventions()$col$seq`, the sequential grey ramp, which
  # was added for this figure - see the note on that entry for why none of the
  # existing colours covered the case.

  require(ggplot2)
  stopifnot(all(c('objective', 'cover', 'grp', 'pct_var') %in% colnames(vc_tbl)))

  fc <- fig_conventions()

  tbl         <- vc_tbl
  tbl$obj_lab <- obj_cover_label(tbl)
  tbl$grp     <- factor(tbl$grp, levels = rev(unique(tbl$grp)))

  # Label positions are computed here, on the FULL table, rather than left to
  # position_stack() in the label layer. position_stack() stacks whatever data
  # the layer was given, so a layer filtered to the labellable segments would
  # re-stack those segments against each other and put every label in the wrong
  # place - a wrong figure that still renders. geom_col() puts the first factor
  # level at the top of the bar, so the cumulative sum runs from the top down;
  # the group total is used rather than a literal 100 so that rounding in
  # `pct_var` cannot shift the labels off their segments.
  tbl       <- tbl[order(tbl$obj_lab, as.integer(tbl$grp)), ]
  tbl$lab_y <- ave(tbl$pct_var, tbl$obj_lab,
                   FUN = \(pp) sum(pp) - (cumsum(pp) - pp / 2))

  grey_pal <- setNames(
    rep(fc$col$seq, length.out = nlevels(tbl$grp)),
    levels(tbl$grp)
  )

  gg <- ggplot(tbl, aes(x = obj_lab, y = pct_var, fill = grp)) +
    geom_col(color = fc$col$separator, linewidth = fc$line$separator) +
    scale_fill_manual(values = grey_pal, name = 'Variance component') +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    fc$theme$facet_legend +
    theme(axis.text.x = element_text(color = fc$col$text, angle = 45, hjust = 1))

  # Segments below ~6% of the bar cannot hold a readable label; labelling them
  # anyway produces overlapping text at the segment boundaries.
  if (show_pct) {
    lab_tbl <- tbl[tbl$pct_var >= 6, ]
    gg <- gg + geom_text(
      data = lab_tbl,
      aes(y = lab_y, label = sprintf('%.1f', pct_var)),
      size = fc$size$label_n, color = fc$col$label_inside
    )
  }

  gg
}

obj_cover_label <- function(tbl) {
  # Two-line "objective\n(COVER)" axis/facet label, used by every figure drawn
  # from a table that was compiled ACROSS objectives. Kept as one function
  # because the compiled tables do not all carry the objective and the cover
  # response the same way: collect_obj_tables() prepends a single
  # `objective_cover` column ("decrease_afg_AFG") to tables that lack an
  # objective/cover pair of their own (e.g. peak_effect_all), and leaves the
  # pair alone where it exists (e.g. sig_by_bin_all). Both are resolved here so
  # the facet labels read identically whichever compiled table a figure is
  # built on, and so the label format lives in one place rather than in each
  # plotting function.

  if (all(c('objective', 'cover') %in% colnames(tbl))) {
    return(paste0(tbl$objective, '\n(', tbl$cover, ')'))
  }
  if ('objective_cover' %in% colnames(tbl)) {
    return(sub('_([A-Z]+)$', '\n(\\1)', tbl$objective_cover))
  }
  stop('obj_cover_label(): need either `objective` and `cover`, or `objective_cover`')
}
plot_peak_effect_summary <- function(
    peak_effect_all, title = NULL, subtitle = NULL,
    x_lab = 'Years since treatment',
    y_lab = 'Mean DART effect at peak (\u0394 RAP cover)',
    show_n = TRUE
) {
  # Cross-objective view of the compiled peak-effect table - i.e. of
  # collect_obj_tables('peak_effect_'), which stacks each objective's
  # peak_effect_year() row per direction. It puts the two quantities that table
  # reports on the two axes at once: WHEN the mean effect over significant
  # pixels peaked (x) and HOW LARGE that mean was (y), so timing and magnitude
  # are read together rather than off separate columns.
  #
  # One panel per objective x cover response, with a shared y axis, because the
  # point of the figure is to compare objectives - free scales would make two
  # panels with very different effect magnitudes look alike. Both directions are
  # drawn in every panel and are never combined, for the same reason
  # mean_effect_by_year() keeps them apart: a mean over both would cancel.
  # Direction colours are the ones used everywhere else in the report.
  #
  # The censoring flag from the table is mapped to point shape (hollow = the
  # peak landed on the last year observed for that direction, so the effect may
  # still have been growing past the end of the record) rather than being left
  # to the caption, since a censored peak is the one case where the x position
  # should not be read as a real peak. `show_n` labels each point with the
  # number of significant pixels behind it, which is often in the tens for late
  # peaks and is the main reason to distrust one.

  require(ggplot2)

  fc      <- fig_conventions()
  dir_pal <- fc$dir$pal
  cen_lab <- c('Peak within record', 'Peak on last year observed')

  pk <- as.data.frame(peak_effect_all)
  pk$obj_lab   <- obj_cover_label(pk)
  pk$direction <- factor(
    ifelse(pk$direction == 'positive', fc$dir$lab[['pos']], fc$dir$lab[['neg']]),
    levels = names(dir_pal)
  )
  pk$censoring <- factor(ifelse(pk$is_censored, cen_lab[2], cen_lab[1]), levels = cen_lab)

  # A peak backed by a single significant pixel has no SE (the SD of one value
  # is NA). It is kept and drawn with a zero-width error bar rather than
  # dropped, the same way plot_mean_effect_year() handles a one-pixel year x
  # direction cell: an NA here would silently remove both the error bar and the
  # sample-size label, so the one peak the reader should trust least would be
  # the one with no n printed next to it.
  pk$peak_se[is.na(pk$peak_se)] <- 0

  # Sample-size labels are pushed away from the zero line (above a positive
  # peak, below a negative one) so they cannot land on top of the point or its
  # error bar: the label is anchored to the far end of the bar, not to the mean.
  # They are drawn as two layers with a fixed `vjust` each rather than one layer
  # with `vjust` mapped per row - a per-row justification aesthetic was being
  # applied inconsistently enough to leave labels sitting on their own points.
  pk$.lab   <- paste0(fc$lab$n_prefix, pk$peak_n_pix)
  pk_up     <- pk[pk$peak_effect > 0, ]
  pk_dn     <- pk[pk$peak_effect <= 0, ]
  pk_up$.y  <- pk_up$peak_effect + pk_up$peak_se
  pk_dn$.y  <- pk_dn$peak_effect - pk_dn$peak_se

  p0 <- pk |>
    ggplot(aes(x = peak_year, y = peak_effect, colour = direction, shape = censoring)) +
    geom_hline(yintercept = 0, linewidth = fc$line$zero) +
    geom_errorbar(
      aes(ymin = peak_effect - peak_se, ymax = peak_effect + peak_se),
      width = 1.2, linewidth = fc$line$annot, show.legend = FALSE
    ) +
    geom_point(size = fc$size$point_open, fill = fc$col$open_fill, stroke = 0.8) +
    scale_colour_manual(values = dir_pal, name = fc$lab$dir_legend) +
    # drop = FALSE keeps both keys in the legend even when no peak in the
    # compiled table happens to be censored, so the hollow symbol is always
    # explained rather than only appearing in the runs that have one.
    scale_shape_manual(
      values = setNames(c(16, 21), cen_lab), drop = FALSE, name = 'Censoring'
    ) +
    facet_wrap(~ obj_lab) +
    # Horizontal room for the sample-size labels, which are centred on their
    # point: a peak in year 1 or in the last year of the record sits hard against
    # the panel edge, and without this its label is clipped to a fragment.
    scale_x_continuous(expand = expansion(mult = c(0.12, 0.12))) +
    labs(
      x = x_lab, y = y_lab,
      title = if (is.null(title)) 'Peak mean DART effect: when it happened and how large it was' else title,
      subtitle = subtitle
    ) +
    fc$theme$facet

  if (show_n) {
    if (nrow(pk_up)) {
      p0 <- p0 + geom_text(
        data = pk_up, aes(x = peak_year, y = .y, label = .lab, colour = direction),
        inherit.aes = FALSE, vjust = -0.6, size = fc$size$label_n, show.legend = FALSE
      )
    }
    if (nrow(pk_dn)) {
      p0 <- p0 + geom_text(
        data = pk_dn, aes(x = peak_year, y = .y, label = .lab, colour = direction),
        inherit.aes = FALSE, vjust = 1.6, size = fc$size$label_n, show.legend = FALSE
      )
    }
    # The n labels sit outside the error bars, so the panels need more vertical
    # room than the data alone would ask for or they get clipped.
    p0 <- p0 + scale_y_continuous(expand = expansion(mult = c(0.20, 0.20)))
  }

  return(p0)

}
plot_sig_by_bin_summary <- function(
    sig_by_bin_all, title = NULL, subtitle = NULL,
    x_lab = 'Years since treatment (binned)',
    y_lab = '% of pixels with a significant effect',
    show_n = TRUE
) {
  # Cross-objective view of the compiled year-bin table - i.e. of
  # collect_obj_tables('sig_by_bin_'). This is the binned, all-objectives
  # analogue of plot_sig_direction_stacked(): the same quantity (how OFTEN a
  # significant effect occurred, split into its positive and negative
  # components) at the coarser 1-5 / 6-10 / 11-15 / 16+ resolution, with one
  # panel per objective x cover response instead of one figure per objective.
  #
  # It answers the companion question to plot_peak_effect_summary() above, which
  # measures how LARGE the significant effects were. Both are drawn from the two
  # tables in the same section of the report, and they can disagree: a bin can
  # carry a high significance rate with small effects, or the reverse.
  #
  # Stacking order is fixed (significant negative against the axis, significant
  # positive above it) rather than following the intended direction the way
  # plot_sig_direction_stacked()'s `reverse_stack` does. Intent differs between
  # panels here - it is negative for a `decrease_*` objective and positive for
  # an `increase_*` one - so no single stacking order could carry it, and a
  # per-panel order would make the panels harder to read against each other.
  # The fill colours still mean the raw direction, as everywhere else.

  require(ggplot2)
  require(dplyr)
  require(tidyr)

  fc      <- fig_conventions()
  dir_pal <- fc$dir$pal

  tb <- as.data.frame(sig_by_bin_all)
  tb$obj_lab <- obj_cover_label(tb)

  plot_df <- tb |>
    tidyr::pivot_longer(
      cols = c(mean_pct_sig_pos, mean_pct_sig_neg),
      names_to = 'direction', values_to = 'pct'
    ) |>
    dplyr::mutate(
      direction = factor(
        ifelse(direction == 'mean_pct_sig_pos', fc$dir$lab[['pos']], fc$dir$lab[['neg']]),
        levels = names(dir_pal)
      )
    )

  # Bin total (every pixel in that bin, significant or not) sitting above the
  # bar, rotated upright so a long "n = XXXXXX" string fits between bars - the
  # same two-level sample-size convention as plot_sig_direction_stacked(), minus
  # the per-segment labels, which the panels are too narrow to hold.
  label_df <- tb
  label_df$y_lab <- tb$mean_pct_sig_pos + tb$mean_pct_sig_neg
  label_df$lab   <- paste0(fc$lab$n_prefix, tb$n_pix)

  p0 <- plot_df |>
    ggplot(aes(x = year_bin, y = pct, fill = direction)) +
    geom_col() +
    scale_fill_manual(values = dir_pal, name = fc$lab$dir_legend) +
    facet_wrap(~ obj_lab) +
    labs(
      x = x_lab, y = y_lab,
      title = if (is.null(title)) 'Significant DART effects by years since treatment, binned' else title,
      subtitle = subtitle
    ) +
    fc$theme$facet

  if (show_n) {
    p0 <- p0 +
      geom_text(
        data = label_df, aes(x = year_bin, y = y_lab, label = lab),
        inherit.aes = FALSE, angle = 90, hjust = -0.1, size = fc$size$label_n
      ) +
      # Upper expansion is generous because the bin totals run to six digits on
      # the real data and the labels are rotated upright, so the label above the
      # tallest bar needs roughly half the bar's own height in clear space.
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.50)))
  }

  return(p0)

}
plot_mean_effect_year <- function(
    effect_by_year, col_year = 'year_diff',
    x_lab = 'Years since treatment', y_lab = 'Mean DART effect (\u0394 RAP cover)',
    title = NULL, subtitle = NULL, peak_effect = NULL, show_peak_labels = TRUE
) {
  # Mean DART effect per year since treatment, over significant pixels only and
  # split by the raw direction of the effect - i.e. the table returned by
  # mean_effect_by_year() (helper_functions.R). This is the *magnitude* companion
  # to plot_sig_direction_stacked(), which shows how *often* a significant effect
  # occurred: a year can have few significant pixels that each moved a long way,
  # or many that each moved a little, and those are different findings.
  #
  # No weighting is applied anywhere here - each point is a plain arithmetic mean
  # of `effect` within that year x direction cell, with its own SE, and the two
  # directions are never combined into a single number (averaging them together
  # would cancel them out against each other). The zero line is drawn because
  # the two series sit either side of it by definition. Direction colours match
  # plot_sig_direction_stacked().
  #
  # `peak_effect` optionally takes the one-row-per-direction table from
  # peak_effect_year() (helper_functions.R) and marks those peaks on the figure:
  # a dashed vertical line at each direction's peak year, coloured to match its
  # series, plus (when `show_peak_labels`) a small label at the peak point
  # carrying the mean effect, its SE and the number of significant pixels behind
  # it. Labels are placed outward from the zero line (above the positive series,
  # below the negative one) and flip to the left of the line for peaks in the
  # right-hand half of the record, so they stay inside the panel. The peak table
  # is passed in rather than recomputed here so that the figure and the
  # accompanying peak-year table can never disagree.

  require(ggplot2)

  fc      <- fig_conventions()
  dir_pal <- fc$dir$pal

  plot_df <- effect_by_year
  # A year x direction cell backed by a single pixel has no SE (sd of one value
  # is NA); it is drawn as a point with no band rather than dropped from the
  # figure, since dropping it would also break the line through it.
  plot_df$se_effect[is.na(plot_df$se_effect)] <- 0
  plot_df$direction <- factor(
    ifelse(plot_df$direction == 'positive', fc$dir$lab[['pos']], fc$dir$lab[['neg']]),
    levels = names(dir_pal)
  )

  p0 <- plot_df |>
    ggplot(aes(x = .data[[col_year]], y = mean_effect, colour = direction, fill = direction)) +
    geom_hline(yintercept = 0, linewidth = fc$line$zero) +
    geom_ribbon(
      aes(ymin = mean_effect - se_effect, ymax = mean_effect + se_effect),
      alpha = fc$alpha$ribbon, colour = NA
    ) +
    geom_line(linewidth = fc$line$series) +
    geom_point(size = fc$size$point) +
    scale_colour_manual(values = dir_pal, name = fc$lab$dir_legend) +
    scale_fill_manual(values = dir_pal, guide = 'none') +
    labs(
      x = x_lab, y = y_lab,
      title = if (is.null(title)) 'Mean DART effect through time, by direction (significant pixels only)' else title,
      subtitle = subtitle
    ) +
    fc$theme$base

  if (!is.null(peak_effect)) {

    pk <- as.data.frame(peak_effect)
    pk$direction <- factor(
      ifelse(pk$direction == 'positive', fc$dir$lab[['pos']], fc$dir$lab[['neg']]),
      levels = names(dir_pal)
    )

    p0 <- p0 + geom_vline(
      data = pk, aes(xintercept = peak_year, colour = direction),
      inherit.aes = FALSE, linetype = 'dashed', linewidth = fc$line$annot, show.legend = FALSE
    )

    if (show_peak_labels) {
      x_rng <- range(plot_df[[col_year]])
      pk$.hjust <- ifelse(pk$peak_year > mean(x_rng), 1.05, -0.05)
      pk$.vjust <- ifelse(pk$peak_effect > 0, -0.2, 1.2)
      pk$.lab   <- sprintf(
        'peak year %s\nmean %.3f, SE %.3f\n%s%d',
        pk$peak_year, pk$peak_effect, pk$peak_se, fc$lab$n_prefix, pk$peak_n_pix
      )
      p0 <- p0 +
        geom_text(
          data = pk,
          aes(x = peak_year, y = peak_effect, label = .lab, colour = direction, hjust = .hjust, vjust = .vjust),
          inherit.aes = FALSE, size = fc$size$label_peak, lineheight = 0.95, show.legend = FALSE
        ) +
        # The labels sit outside the ribbons, so the panel needs a little more
        # vertical room than the data alone would ask for or they get clipped.
        scale_y_continuous(expand = expansion(mult = c(0.16, 0.16)))
    }

  }

  return(p0)

}
plot_all_DART_time <- function(
    df_in, res, obj, eco,
    res_col = 'fun_group', obj_col = 'objective', tx_col = 'tx_coarse', eco_col = 'us_l4name'
) {
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  fc <- fig_conventions()
  
  df_plot <- df_in |>
    mutate(year_diff = factor(year_diff), sig = as.character(sig)) |>
    group_by(.data[[tx_col]]) |>
    tidyr::complete(year_diff, sig = as.character(unique(df_in$sig)), fill = list(effect = 0)) |>
    ungroup() |>
    mutate(year_diff = as.integer(year_diff))
  
  ylab <- paste0('DART effect (\u0394 RAP)')
  flab <- fc$lab$sig_legend
  tlab <- paste0("Cover:        ", res, '\nObjective:  ', obj, '\nEcoregion: ', eco)
  
  p0 <- df_plot |>
    ggplot(aes(x = year_diff, y = effect, fill = sig)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.85) +
    geom_hline(yintercept = 0) +
    # `vars(.data[[tx_col]])`, not `~ get(tx_col)`. The get() form happens to
    # resolve - ggplot2 evaluates facet variables in a data mask that get() can
    # reach - but it is the exact pattern that killed plot_eco() and plot_tx(),
    # which both failed at draw time with "object 'us_l4name' not found" once
    # the column was no longer also in the calling scope. .data[[ ]] looks the
    # column up in the data and nowhere else, so it cannot be shadowed by, or
    # accidentally satisfied by, an object of the same name.
    facet_wrap(vars(.data[[tx_col]])) +
    labs(x = "Years since treatment", y = ylab, fill = flab, title = tlab) +
    fc$theme$facet_legend
  
  return(p0)
  
}
