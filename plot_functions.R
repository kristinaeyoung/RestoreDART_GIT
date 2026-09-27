plot_sig_direction_stacked <- function(
    df, group_col, sig_col = 'sig', effect_col = 'effect',
    reverse_stack = FALSE, x_lab = group_col, title = NULL, subtitle = NULL,
    show_n = TRUE
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
  # Sample size: each bar is labeled "(n = XX)" above its top, where XX is the
  # total pixel count for that group (not just the significant pixels), so the
  # reader can see how much data backs each bar.

  require(dplyr)
  require(ggplot2)
  require(tidyr)

  grp_summary <- df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      n_total = dplyr::n(),
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
      lab   = paste0('(n = ', n_total, ')')
    )

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
      inherit.aes = FALSE, vjust = -0.4, size = 2.5
    )
  }

  return(p0)

}
plot_poly_sig_summary <- function(tbl, metric = c('overall', 'positive', 'negative')) {
  # Takes the expandable per-objective/cover summary table built by
  # summarize_poly_sig() (helper_functions.R) - one row per objective/cover
  # combination, with mean/SD/SE of % significant pixels taken across polygons -
  # and plots it as a point-range chart (mean +/- SE) so multiple objectives can
  # be compared on one figure as they're added to the report.

  require(ggplot2)

  metric <- match.arg(metric)
  mean_col <- switch(metric, overall = 'mean_pct_sig', positive = 'mean_pct_sig_pos', negative = 'mean_pct_sig_neg')
  se_col   <- switch(metric, overall = 'se_pct_sig',   positive = 'se_pct_sig_pos',   negative = 'se_pct_sig_neg')

  tbl$.label   <- paste0(tbl$objective, '\n(', tbl$cover, ')')
  tbl$.mean    <- tbl[[mean_col]]
  tbl$.se      <- tbl[[se_col]]

  p0 <- tbl |>
    ggplot(aes(x = .label, y = .mean)) +
    geom_pointrange(aes(ymin = .mean - .se, ymax = .mean + .se), size = 0.6) +
    labs(
      x = 'Objective (cover response)', y = paste0('Mean % significant (', metric, '), ± SE across polygons'),
      title = paste0('Per-polygon significance summary: ', metric)
    ) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))

  return(p0)

}
plot_weighted_peak_heatmap <- function(
    poly_year_sig, direction = c('overall', 'positive', 'negative'),
    obj_label = NULL, cover_label = NULL
) {
  # Treatment x ecoregion heatmap of the mean, significance-weighted "peak year"
  # (see weighted_peak_year() in helper_functions.R for the weighting scheme
  # itself). `poly_year_sig` is expected to have one row per
  # polygon/tx_coarse/us_l4name/year_diff, with pre-computed pct_sig, pct_sig_pos,
  # and pct_sig_neg columns (as built in the .Rmd's time-since-treatment section).
  # `direction` selects which of those three weight columns to use, so the same
  # function can produce the "overall", "significant positive only", or
  # "significant negative only" version of the figure - the three are meant to be
  # looked at side by side, since a polygon's overall peak significance year can
  # be driven by either direction.

  require(dplyr)
  require(ggplot2)

  direction <- match.arg(direction)
  weight_col <- switch(direction, overall = 'pct_sig', positive = 'pct_sig_pos', negative = 'pct_sig_neg')

  poly_weighted <- poly_year_sig |>
    dplyr::group_by(polygon, tx_coarse, us_l4name) |>
    dplyr::summarise(
      weighted_year = weighted_peak_year(year_diff, .data[[weight_col]]),
      .groups = 'drop'
    ) |>
    dplyr::filter(!is.na(weighted_year))

  heatmap_df <- poly_weighted |>
    dplyr::group_by(tx_coarse, us_l4name) |>
    dplyr::summarise(mean_weighted_year = mean(weighted_year), .groups = 'drop')

  subtitle_txt <- if (!is.null(obj_label) || !is.null(cover_label)) {
    paste0('Cover: ', cover_label, '  |  Objective: ', obj_label)
  } else NULL

  title_txt <- paste0(
    'Mean (significance-weighted) year of peak DART significance\n',
    '(', switch(direction, overall = 'all significant pixels', positive = 'significant positive pixels only', negative = 'significant negative pixels only'), ')'
  )

  p0 <- heatmap_df |>
    ggplot(aes(x = tx_coarse, y = us_l4name, fill = mean_weighted_year)) +
    geom_tile(color = 'white') +
    scale_fill_viridis_c(option = 'magma', name = 'Mean\nsignificance-\nweighted year') +
    labs(x = 'Treatment', y = 'Ecoregion', title = title_txt, subtitle = subtitle_txt) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'),
      axis.text.y = element_text(color = 'black')
    )

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
