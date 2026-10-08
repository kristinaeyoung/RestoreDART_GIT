# BEM
# created  07 October 2026
# last run NOT RUN

# Functions retired from the live RestoreDART sources, kept here rather than
# deleted so the record of what the report once did survives (the same reason
# the commented-out parameter rounds stay in archive/make_models.R). Nothing in
# the pipeline sources this file; it is a record, not a dependency.
#
# Moved 07 October 2026 after a reachability check over the live documents
# (0_DART_setup.R, the three 2_*.Rmd, 3_make_linear_models.R and
# 3_report_linear_models.Rmd, following calls between functions as well as
# calls from the documents) found nothing reaching either of them.
#
#   make_excl_table()    was helper_functions.R. Built the cumulative
#     "excluding these ecoregions removes x% of pixels" table for a section
#     that no longer exists; the filtering log in 2_make_DART_results.Rmd
#     reports the same quantity per filter and per objective.
#
#   plot_all_DART_sig()  was plot_functions.R. The year x treatment x
#     ecoregion heatmap ("Where are treated areas significantly shifting?"),
#     removed from every per-objective section in September 2026: with five
#     objectives and up to 28 ecoregions each it was one dense panel grid per
#     objective, and the bar panels that replaced it carry the same
#     information on directly readable axes. It takes `metric = 'prop_sig'`
#     as well as the default net-direction fill. To bring the figure back,
#     source this file and restore the one call at the top of the `raw-N_1`
#     chunk of 2_raw_result_graphs.Rmd.
#
# Both read fig_conventions() / the helper API as they stood on 07 October
# 2026. Check their arguments against the current sources before reviving one.

make_excl_table <- function(summary_table, tbl, ...) {
  
  tbl <- match.arg(tbl, choices = c('tbl_eco', 'tbl_tx'))
  tbl_data <- summary_table[[tbl]]
  
  excl_ecos <- list(...)
  
  excl_cols <- lapply(excl_ecos, paste, collapse = ', ')
  
  excl_per <- sapply(seq_along(excl_ecos), function(i) {
    combined <- unlist(excl_ecos[1:i])
    round((sum(tbl_data[names(tbl_data) %in% combined]) / sum(tbl_data)) * 100, 2)
  })
  
  excl_lab <- sapply(seq_along(excl_cols), function(i) {
    paste(unlist(excl_cols[1:i]), collapse = ', ')
  })
  
  return(data.frame(
    lab = excl_lab,
    per = paste(excl_per, '%')
  ))
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
