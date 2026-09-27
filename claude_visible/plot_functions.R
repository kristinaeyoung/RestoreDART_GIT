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
        ifelse(direction == 'pct_pos', 'Significant positive', 'Significant negative'),
        levels = c('Significant positive', 'Significant negative')
      )
    )

  label_df <- grp_summary |>
    dplyr::mutate(
      y_lab = pct_pos + pct_neg,
      lab   = paste0('n = ', n_total)
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
      .grp = grp_summary[[group_col]], direction = 'Significant positive',
      n_dir = grp_summary$n_pos, y_lab = y_pos, stringsAsFactors = FALSE
    ),
    data.frame(
      .grp = grp_summary[[group_col]], direction = 'Significant negative',
      n_dir = grp_summary$n_neg, y_lab = y_neg, stringsAsFactors = FALSE
    )
  )
  label_dir_df$lab <- paste0('n = ', label_dir_df$n_dir)
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
    scale_fill_manual(values = c('Significant positive' = '#b2182b', 'Significant negative' = '#2166ac')) +
    labs(
      x = x_lab, y = '% of pixels with a significant effect',
      fill = 'Direction',
      title = if (is.null(title)) 'Significant DART effects, by direction' else title,
      subtitle = subtitle
    ) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))

  if (show_n) {
    p0 <- p0 + geom_text(
      data = label_df, aes(x = .data[[group_col]], y = y_lab, label = lab),
      inherit.aes = FALSE, angle = 90, hjust = -0.1, size = 2.5
    )
  }

  if (show_n_direction) {
    p0 <- p0 + geom_text(
      data = label_dir_df, aes(x = .data[[group_col]], y = y_lab, label = lab),
      inherit.aes = FALSE, angle = 90, hjust = 0.5, size = 2.2, colour = 'white'
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
plot_poly_sig_summary <- function(tbl, metric = c('all', 'overall', 'positive', 'negative'), title = NULL, subtitle = NULL) {
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

  require(ggplot2)

  metric <- match.arg(metric)

  metric_cols <- list(
    overall  = c(mean = 'mean_pct_sig',     se = 'se_pct_sig'),
    positive = c(mean = 'mean_pct_sig_pos', se = 'se_pct_sig_pos'),
    negative = c(mean = 'mean_pct_sig_neg', se = 'se_pct_sig_neg')
  )
  metric_pal <- c(overall = 'grey20', positive = '#b2182b', negative = '#2166ac')
  metric_lab <- c(overall = 'Overall', positive = 'Significant positive', negative = 'Significant negative')

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
      size = 0.6, position = position_dodge(width = 0.4)
    ) +
    scale_colour_manual(values = setNames(unname(metric_pal), unname(metric_lab)), name = 'Metric') +
    labs(
      x = 'Objective (cover response)', y = y_lab,
      title = if (is.null(title)) paste0('Per-polygon significance summary: ', metric) else title,
      subtitle = subtitle
    ) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))

  # A single-metric call has nothing to distinguish, so the legend is dropped and
  # the figure looks as it did before 'all' existed.
  if (metric != 'all') p0 <- p0 + guides(colour = 'none')

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

  dir_pal <- c('Significant positive' = '#b2182b', 'Significant negative' = '#2166ac')

  plot_df <- effect_by_year
  # A year x direction cell backed by a single pixel has no SE (sd of one value
  # is NA); it is drawn as a point with no band rather than dropped from the
  # figure, since dropping it would also break the line through it.
  plot_df$se_effect[is.na(plot_df$se_effect)] <- 0
  plot_df$direction <- factor(
    ifelse(plot_df$direction == 'positive', 'Significant positive', 'Significant negative'),
    levels = names(dir_pal)
  )

  p0 <- plot_df |>
    ggplot(aes(x = .data[[col_year]], y = mean_effect, colour = direction, fill = direction)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_ribbon(
      aes(ymin = mean_effect - se_effect, ymax = mean_effect + se_effect),
      alpha = 0.2, colour = NA
    ) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.6) +
    scale_colour_manual(values = dir_pal, name = 'Direction') +
    scale_fill_manual(values = dir_pal, guide = 'none') +
    labs(
      x = x_lab, y = y_lab,
      title = if (is.null(title)) 'Mean DART effect through time, by direction (significant pixels only)' else title,
      subtitle = subtitle
    ) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))

  if (!is.null(peak_effect)) {

    pk <- as.data.frame(peak_effect)
    pk$direction <- factor(
      ifelse(pk$direction == 'positive', 'Significant positive', 'Significant negative'),
      levels = names(dir_pal)
    )

    p0 <- p0 + geom_vline(
      data = pk, aes(xintercept = peak_year, colour = direction),
      inherit.aes = FALSE, linetype = 'dashed', linewidth = 0.4, show.legend = FALSE
    )

    if (show_peak_labels) {
      x_rng <- range(plot_df[[col_year]])
      pk$.hjust <- ifelse(pk$peak_year > mean(x_rng), 1.05, -0.05)
      pk$.vjust <- ifelse(pk$peak_effect > 0, -0.2, 1.2)
      pk$.lab   <- sprintf(
        'peak year %s\nmean %.3f, SE %.3f\nn = %d',
        pk$peak_year, pk$peak_effect, pk$peak_se, pk$peak_n_pix
      )
      p0 <- p0 +
        geom_text(
          data = pk,
          aes(x = peak_year, y = peak_effect, label = .lab, colour = direction, hjust = .hjust, vjust = .vjust),
          inherit.aes = FALSE, size = 2.3, lineheight = 0.95, show.legend = FALSE
        ) +
        # The labels sit outside the ribbons, so the panel needs a little more
        # vertical room than the data alone would ask for or they get clipped.
        scale_y_continuous(expand = expansion(mult = c(0.16, 0.16)))
    }

  }

  return(p0)

}
plot_eco <- function(df, res, obj, eco = NULL, res_col = 'fun_group', obj_col = 'objective', eco_col = 'us_l4name') {
  
  require(ggplot2)
  
  obj <- tolower(obj)
  df <- if (!is.null(eco)) df[df[[eco_col]] %in% eco, ]
  df <- df[df[[res_col]] == res, ]
  df <- df[grepl(obj, df[[obj_col]]), ]
  stopifnot(nrow(df) > 0)
  
  p_out <- df |>
    ggplot(aes(x = year_diff, y = effect, col = as.character(sig))) +
    geom_point() +
    facet_wrap(~ get(eco_col)) +
    labs(
      x = "Years Since Treatment", y = paste0("Effect (", res, ')'),
      title = 'Overall DART effect through time, ~ ecoregion',
      subtitle = paste0("(when objective was ", obj, ")"),
      color = 'DART result\nsignificant?'
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(color = 'black'), 
      strip.text = element_text(color = 'black', size = 8),
      legend.title = element_text(face = 'bold')
    )
  
  return(p_out)
}
plot_tx <- function(df, res, obj, tx = NULL, res_col = 'fun_group', obj_col = 'objective', tx_col = 'tx_coarse') {
  
  require(ggplot2)
  
  obj <- tolower(obj)
  df <- if (!is.null(tx)) df[df[[tx_col]] %in% tx, ]
  df <- df[df[[res_col]] == res, ]
  df <- df[grepl(obj, df[[obj_col]]), ]
  stopifnot(nrow(df) > 0)
  
  p_out <- df |>
    ggplot(aes(x = year_diff, y = effect, col = as.character(sig))) +
    geom_point() +
    facet_wrap(~ get(tx_col)) +
    labs(
      x = "Years Since Treatment", y = paste0("Effect (", res, ')'),
      title = 'Overall DART effect through time, ~ treatment',
      subtitle = paste0("(when objective was ", obj, ")"),
      color = 'DART result\nsignificant?'
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(color = 'black'), 
      strip.text = element_text(color = 'black', size = 8),
      legend.title = element_text(face = 'bold')
    )
  
  return(p_out)
}
plot_all_DART_time <- function(
    df_in, res, obj, eco,
    res_col = 'fun_group', obj_col = 'objective', tx_col = 'tx_coarse', eco_col = 'us_l4name'
) {
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  df_plot <- df_in |>
    mutate(year_diff = factor(year_diff), sig = as.character(sig)) |>
    group_by(.data[[tx_col]]) |>
    tidyr::complete(year_diff, sig = as.character(unique(df_in$sig)), fill = list(effect = 0)) |>
    ungroup() |>
    mutate(year_diff = as.integer(year_diff))
  
  ylab <- paste0('DART effect (\u0394 RAP)')
  flab <- 'DART result\nsignificant?'
  tlab <- paste0("Cover:        ", res, '\nObjective:  ', obj, '\nEcoregion: ', eco)
  
  p0 <- df_plot |>
    ggplot(aes(x = year_diff, y = effect, fill = sig)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.85) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ get(tx_col)) +
    labs(x = "Years since treatment", y = ylab, fill = flab, title = tlab) +
    theme_bw() +
    theme(
      axis.text = element_text(color = 'black'), 
      strip.text = element_text(color = 'black', size = 8),
      legend.title = element_text(face = 'bold')
    )
  
  return(p0)
  
}
plot_all_DART_sig <- function(
    input_df, obj, ptype = 1, ftype = "",
    min_n = 30, metric = c("net", "prop_sig"), show_n = FALSE
) {
  
  require(dplyr)
  require(ggplot2)
  
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
    geom_tile(color = "white", linewidth = 0.2)
  
  if (metric == "net") {
    p0 <- p0 + scale_fill_gradient2(
      low = "#2166ac", mid = "grey90", high = "#b2182b", midpoint = 0,
      limits = fill_lims, name = fill_lab
    )
  } else {
    p0 <- p0 + scale_fill_viridis_c(option = "magma", limits = fill_lims, name = fill_lab)
  }
  
  if (show_n) {
    p0 <- p0 + geom_text(aes(label = n_pix), size = 2, color = "grey20", alpha = 1)
  }
  
  p0 <- p0 +
    # tiles built on fewer than `min_n` pixels are faded, so a striking color isn't
    # mistaken for a reliable signal when it's actually driven by a handful of pixels
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
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
    theme_bw() +
    theme(
      axis.text = element_text(color = 'black', size = 7),
      strip.text = element_text(color = 'black')
    )
  
  if (ptype == 1) {
    p0 <- p0 + facet_wrap(~ us_l4name)
  } else {
    p0 <- p0 + coord_fixed()
  }
  
  return(p0)
  
}
