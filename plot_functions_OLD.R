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
