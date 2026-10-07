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
    open_fill    = 'white'     # interior of a hollow (censored) point
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
    facet_wrap(~ get(tx_col)) +
    labs(x = "Years since treatment", y = ylab, fill = flab, title = tlab) +
    fc$theme$facet_legend
  
  return(p0)
  
}
plot_all_DART_sig <- function(
    input_df, obj, ptype = 1, ftype = "",
    min_n = 30, metric = c("net", "prop_sig"), show_n = FALSE
) {
  
  require(dplyr)
  require(ggplot2)
  
  fc <- fig_conventions()
  
  metric <- match.arg(metric)
  
  # ptype 1 facets by ecoregion, so us_l4name stays in the grouping.
  # ptype 2 is meant to collapse across ecoregion - it must be dropped from the
  # grouping *before* summarising, not just left out of facet_wrap() afterward,
  # or every ecoregion's value gets drawn on top of the same tile.
  group_vars <- if (ptype == 1) {
    c("year_diff", "us_l4name", "tx_coarse")
  } else if (ptype == 2) {
    c("year_diff", "tx_coarse")
  } else {
    stop('bad ptype')
  }
  
  fdf <- input_df |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n_pix    = n(),
      prop_sig = mean(sig, na.rm = TRUE),
      prop_pos = mean(sig & effect > 0, na.rm = TRUE),
      prop_neg = mean(sig & effect < 0, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(
      # net_sig: -1 = every pixel is significant & negative, +1 = every pixel is
      # significant & positive, 0 = no signal either way. This is what actually lets
      # you see whether a hot cell is "successful" or "anti-successful."
      net_sig     = prop_pos - prop_neg,
      enough_data = n_pix >= min_n
    )
  
  fill_var  <- if (metric == "net") "net_sig" else "prop_sig"
  fill_lims <- if (metric == "net") c(-1, 1) else c(0, 1)
  fill_lab  <- if (metric == "net") {
    "Net direction of\nsignificant pixels\n(+ = positive, \u2212 = negative)"
  } else {
    "Proportion\nsignificant\nDART pixels"
  }
  
  p0 <- fdf |>
    ggplot(aes(x = year_diff, y = tx_coarse, fill = .data[[fill_var]], alpha = enough_data)) +
    geom_tile(color = fc$col$separator, linewidth = fc$line$separator)
  
  if (metric == "net") {
    p0 <- p0 + scale_fill_gradient2(
      low = fc$col$neg, mid = fc$col$mid, high = fc$col$pos, midpoint = 0,
      limits = fill_lims, name = fill_lab
    )
  } else {
    p0 <- p0 + scale_fill_viridis_c(option = "magma", limits = fill_lims, name = fill_lab)
  }
  
  if (show_n) {
    p0 <- p0 + geom_text(aes(label = n_pix), size = fc$size$label_tile, color = fc$col$overall, alpha = 1)
  }
  
  p0 <- p0 +
    # tiles built on fewer than `min_n` pixels are faded, so a striking color isn't
    # mistaken for a reliable signal when it's actually driven by a handful of pixels
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = fc$alpha$faded), guide = "none") +
    # break long multi-treatment names (e.g. "seeding;soil disturbance") onto separate
    # lines instead of letting them run together or get truncated
    scale_y_discrete(labels = function(x) gsub(';', ';\n', x)) +
    labs(
      x = "Years since treatment", y = "Treatment",
      title = paste0(
        "Where are treated areas significantly ",
        if (metric == "net") "shifting" else "increasing", " ", ftype, "?"
      ),
      subtitle = paste0(
        "(when objective was ", obj, ") \u2014 faded tiles have fewer than ", min_n, " pixels"
      )
    ) +
    fc$theme$base +
    theme(
      axis.text = element_text(color = fc$col$text, size = fc$size$axis_text_small),
      strip.text = element_text(color = fc$col$text)
    )
  
  if (ptype == 1) {
    p0 <- p0 + facet_wrap(~ us_l4name)
  } else {
    p0 <- p0 + coord_fixed()
  }
  
  return(p0)
  
}
