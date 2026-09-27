get_intended_sign <- function(objective) {
  # Single source of truth for "which raw sign of `effect` counts as the INTENDED
  # management outcome for a given objective": +1 if the objective is an
  # increase_* goal (a positive/increasing effect is intended), -1 if it's a
  # decrease_* goal (a negative/decreasing effect is intended). Centralizing this
  # here means every place that needs to translate a raw positive/negative DART
  # effect into intended/unintended language (reduce_s0(), label_intended() below,
  # the write-up text) agrees with every other place, rather than each re-deriving
  # it slightly differently. See the terminology note near the top of the .Rmd:
  # "positive"/"negative" always refers to the raw sign of the (differenced) RAP
  # cover effect; "intended"/"unintended" always refers to that sign relative to
  # the objective's management goal.
  dir_word <- sub('_.*$', '', objective)
  sign_out <- ifelse(dir_word == 'increase', 1, ifelse(dir_word == 'decrease', -1, NA_real_))
  if (any(is.na(sign_out))) {
    stop('get_intended_sign(): objective must start with "increase_" or "decrease_"')
  }
  return(sign_out)
}
label_intended <- function(sig, effect, objective) {
  # Per-pixel label combining significance with intent (see get_intended_sign()):
  # non-significant pixels get "not significant" (direction isn't meaningful for
  # them); among significant pixels, "intended" means the effect moved the
  # response the way the objective wanted, "unintended" means the opposite.
  intended_sign <- get_intended_sign(objective)
  ifelse(!sig, 'not significant', ifelse(sign(effect) == intended_sign, 'intended', 'unintended'))
}
se <- function(x) {
  # Standard error of the mean. Pulled out as a named helper (rather than left
  # inline in a single .Rmd chunk) so any chunk that summarizes a sample-level
  # statistic across polygons can reuse the same definition.
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}
summarize_poly_sig <- function(df_in, objective_label, cover_label, col_polygon = 'polygon', col_sig = 'sig', col_effect = 'effect') {
  # Per-polygon significance summary (mean/SD/SE of % significant, % significant
  # positive, and % significant negative, taken across polygons rather than
  # pixels - see the note in the .Rmd on why polygon is the right unit here).
  # Returns ONE ROW per objective/cover combination, tagged with `objective`,
  # `cover`, and the number of polygons that row is based on, so that calling
  # this once per objective and `rbind()`-ing the results builds up a single
  # expandable table as more objectives are added to the report.

  require(dplyr)

  poly_sig <- df_in |>
    dplyr::group_by(.data[[col_polygon]]) |>
    dplyr::summarise(
      pct_sig     = 100 * mean(.data[[col_sig]]),
      pct_sig_pos = 100 * mean(.data[[col_sig]] & .data[[col_effect]] > 0),
      pct_sig_neg = 100 * mean(.data[[col_sig]] & .data[[col_effect]] < 0),
      .groups = 'drop'
    )

  data.frame(
    objective       = objective_label,
    cover           = cover_label,
    n_polygons      = nrow(poly_sig),
    mean_pct_sig    = mean(poly_sig$pct_sig),
    sd_pct_sig      = sd(poly_sig$pct_sig),
    se_pct_sig      = se(poly_sig$pct_sig),
    mean_pct_sig_pos = mean(poly_sig$pct_sig_pos),
    sd_pct_sig_pos   = sd(poly_sig$pct_sig_pos),
    se_pct_sig_pos   = se(poly_sig$pct_sig_pos),
    mean_pct_sig_neg = mean(poly_sig$pct_sig_neg),
    sd_pct_sig_neg   = sd(poly_sig$pct_sig_neg),
    se_pct_sig_neg   = se(poly_sig$pct_sig_neg),
    stringsAsFactors = FALSE
  )
}
weighted_peak_year <- function(year, weight) {
  # A significance-weighted "peak year": conceptually the weighted mean (some
  # call this a weighted centroid, or center of mass) of the time series, using
  # each year's significance value as its weight, so years with a stronger
  # signal pull the estimate toward them more than years with little or none.
  # This is exactly what base R's stats::weighted.mean() computes, so that's
  # used directly here rather than hand-rolling sum(year * weight) / sum(weight)
  # inline - see the .Rmd for a longer explanation of the weighting scheme and
  # why a hard single-year peak/trough (as computed elsewhere in the report) can
  # be too sensitive to noise in any one year.
  if (sum(weight, na.rm = TRUE) <= 0) return(NA_real_)
  stats::weighted.mean(year, weight, na.rm = TRUE)
}
split_by_objective <- function(df_in, obj_col = "objective", fun_col = "fun_group") {
  
  stopifnot(
    is.data.frame(df_in),
    nrow(df_in) > 0,
    is.character(obj_col),
    length(obj_col) == 1,
    is.character(fun_col),
    length(fun_col) == 1
  )
  
  all_objectives <- unique(unlist(strsplit(df_in[[obj_col]], ', ')))
  all_fun_groups <- unique(df_in[[fun_col]])
  
  combos <- expand.grid(objective = all_objectives, fun_group = all_fun_groups, stringsAsFactors = F)
  combos$match_fun_group <- toupper(gsub('.*_', '', combos$objective))
  #combos$match_fun_group[which(combos$fun_group == 'BAR')] <- 'BAR'
  combos <- combos[combos$match_fun_group == combos$fun_group, ]

  result <- vector("list", nrow(combos))
  
  for (i in seq_len(nrow(combos))) {
    
    i_obj <- combos$objective[i]
    i_fg  <- combos$fun_group[i]
    
    # selects all rows where ANY objectives matches the target objective
    i_x <- df_in[[obj_col]]
    i_out <- df_in[grepl(i_obj, i_x), ]
    # seelcts rows for target functional group
    i_out <- i_out[i_out[[fun_col]] == i_fg, ]
    # only use post-intervention data
    i_out <- i_out[i_out$year_diff > 0, ]
    
    result[[i]] <- i_out
    
  }
  
  result <- setNames(result, paste(combos$objective, combos$fun_group, sep = '_'))
  
  return(result)
}
get_summary_DART_results <- function(
    df_in,
    col_polygon  = "polygon",
    col_tx       = "tx_coarse",
    col_eco      = "us_l4name",
    col_sig      = "sig",
    col_effect   = "effect",
    bins         = 60
) {
  
  require(dplyr)
  
  # treatment
  tbl_tx <- table(df_in[[col_tx]])
  # ecoregion
  tbl_eco <- table(df_in[[col_eco]])
  # treatment x ecoregion level, for pixel
  tbl_tx_eco <- table(df_in[[col_tx]], df_in[[col_eco]])
  
  # plot the pixel frequency histogram
  pix_table <- as.data.frame(table(df_in[[col_tx]], df_in[[col_eco]]))
  colnames(pix_table) <- c(col_tx, col_eco, "Freq")
  
  pixel_plot_0 <- ggplot(pix_table, aes(x = Freq)) +
    geom_histogram(bins = bins, fill = "grey35", linewidth = 0.3) +
    labs(title = "Histogram of pixel counts (all cells)", x = "Count", y = "Frequency") +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))
  
  pixel_plot_1 <- ggplot(pix_table[which(pix_table$Freq > 0), ], aes(x = Freq)) +
    geom_histogram(bins = bins, fill = "grey35", linewidth = 0.3) +
    labs(title = "Histogram of pixel counts (n > 0)", x = "Count", y = "Frequency") +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))
  
  # treatment x ecoregion level, for polygon
  df_poly <- df_in[, c(col_polygon, col_tx, col_eco)]
  df_poly <- df_poly[!duplicated(df_poly), ]
  tbl_poly <- table(df_poly[[col_tx]], df_poly[[col_eco]])
  
  # plot the polygon frequency histogram
  poly_df <- data.frame(count = as.vector(table(df_poly[[col_tx]], df_poly[[col_eco]])))
  poly_df <- data.frame(count = poly_df$count[poly_df$count > 0])
  poly_n_breaks <- max(poly_df) + 1
  
  poly_plot <- ggplot(poly_df, aes(x = count)) +
    geom_histogram(bins = poly_n_breaks, fill = "grey35", colour = "white", linewidth = 0.3) +
    labs(x = "Count", y = "Frequency", title = paste0("Polygon counts\n(for each level of: ", col_tx, " x ", col_eco, ")")) +
    scale_x_continuous(breaks = seq(max(poly_df))) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))

  # Polygon count BY YEAR SINCE TREATMENT (distinct from tbl_poly/poly_plot above,
  # which count polygons per tx_coarse x us_l4name cell). This counts, for each
  # year_diff, how many distinct polygons contribute at least one pixel that year -
  # i.e. how much the pool of independent spatial replicates shrinks over the
  # course of the time series, as polygons drop out of the post-treatment record
  # (a different question from raw pixel count by year, since a polygon can have
  # many or few pixels without changing how many *independent* polygons back that
  # year's estimate).
  df_poly_year <- df_in[, c(col_polygon, 'year_diff')]
  df_poly_year <- df_poly_year[!duplicated(df_poly_year), ]
  tbl_poly_year <- table(df_poly_year$year_diff)

  poly_plot_year <- ggplot(as.data.frame(tbl_poly_year), aes(x = as.integer(as.character(Var1)), y = Freq)) +
    geom_col(fill = "grey35", colour = "white", linewidth = 0.3) +
    labs(x = "Years since treatment", y = "Number of polygons", title = "Polygon count by year since treatment") +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))

  # overall DART significance
  pix_sig_TOT <- c(
    # percent of significantly different DART pixels that show a positive effect:
    round((table(df_in[[col_sig]]) / nrow(df_in)) * 100, 1)[['TRUE']],
    pix_sig_pos <- round((nrow(df_in[df_in[[col_sig]] == T & df_in[[col_effect]] > 0, ]) / nrow(df_in)) * 100, 1),
    # percent of significantly different DART pixels that show a negative effect:
    pix_sig_neg <- round((nrow(df_in[df_in[[col_sig]] == T & df_in[[col_effect]] < 0, ]) / nrow(df_in)) * 100, 1)
  )
  # by-treatment DART significance
  pix_sig_TX <- df_in |>
    group_by(.data[[col_tx]]) |>
    summarise(pix_sig = sum(.data[[col_sig]]), pix_n = n(), .groups = 'drop') |>
    mutate(pix_sig_perc = round(100 * (pix_sig / pix_n), 2))
  # by-ecoregion DART significance
  pix_sig_ECO <- df_in |>
    group_by(.data[[col_eco]]) |>
    summarise(pix_sig = sum(.data[[col_sig]]), pix_n = n(), .groups = 'drop') |>
    mutate(pix_sig_perc = round(100 * (pix_sig / pix_n), 2))
  
  return(list(
    input          = df_in,
    tbl_tx         = tbl_tx,
    tbl_eco        = tbl_eco,
    tbl_tx_eco     = tbl_tx_eco,
    tbl_poly       = tbl_poly,
    tbl_poly_year  = tbl_poly_year,
    pixel_plot_0   = pixel_plot_0,
    pixel_plot_1   = pixel_plot_1,
    poly_plot      = poly_plot,
    poly_plot_year = poly_plot_year,
    pix_sig_TOT    = pix_sig_TOT,
    pix_sig_TX     = pix_sig_TX,
    pix_sig_ECO    = pix_sig_ECO
  ))
  
}
print_summary_name <- function(xx, ind, type) {
  
  yy <- xx |>
    names() |>
    _[[ind]] |>
    strsplit('_') |>
    _[[1]]
  
  z0 <- paste(yy[1], yy[2], collapse = '_')
  z1 <- paste('cover: ', yy[3], ', objective:', z0, collapse = '')
    
  zz <- ifelse(type == 4, z0, yy[type])
  zz <- ifelse(type == 5, z1, zz)
    
  return(zz)
    
}
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
reduce_s0 <- function(input, coarse_tx = T) {
  # turns s_0$input into a summary table required by table_restoredart_template.xlsx
  
  require(dplyr)
  
  # need total pixels per objective/cover type/ecoregion/treatment/year
  if (coarse_tx) {
    obj_list <- input$objective |>
      unique() |>
      strsplit(',') |>
      lapply(trimws)
    single_obj <- Reduce(intersect, obj_list)
    stopifnot(length(single_obj) == 1)
    input$objective <- rep(single_obj, nrow(input))
    
    if (!('intended_direction' %in% colnames(input))) {
      int_dir <- input$objective |>
        unique() |>
        strsplit('_') |>
        unlist() |>
        subset(c(T, F))
      
      if (int_dir == 'decrease') {
        input$intended_direction <- ifelse(input$effect < 0, T, F)
      } else if (int_dir == 'increase') {
        input$intended_direction <- ifelse(input$effect > 0, T, F)
      } else {
        stop('intended direction assignment failed')
      }
      
      #input$desired <- rowSums(data.frame(input$sig, input$intended_direction))
      input$sig_des <- input$sig == T & input$intended_direction == T
      input$sig_und <- input$sig == T & input$intended_direction == F
      
    } else {
      stop('need to code intended direction for the fine tx categories')
    }
  }
  
  output <- input |>
    #dplyr::group_by(objective, fun_group, us_l4name, tx_coarse, year_RAP) |>
    # dont group by year for now
    dplyr::group_by(objective, fun_group, tx_coarse, us_l4name) |>
    dplyr::summarize(
      grp_n_pix = dplyr::n(),
      grp_n_poly = length(unique(polygon)),
      
      grp_n_sig = sum(sig),
      grp_n_sig_int = sum(sig_des),
      grp_n_sig_uni = sum(sig_und),
      
      grp_perc_sig = round((grp_n_sig / grp_n_pix) * 100, 2),
      grp_perc_int = round((grp_n_sig_int / grp_n_sig) * 100, 2),
      grp_perc_uni = round((grp_n_sig_uni / grp_n_sig) * 100, 2),
      .groups = 'drop_last'
    )
  
  return(output)
  
}
print_tx_eco_trunc_table <- function(rm_tx, rm_eco, tbl, thr) {
  
  full <- tbl
  if (length(rm_tx))  full <- full[-which(rownames(full) %in% rm_tx), , drop = FALSE]
  if (length(rm_eco)) full <- full[, -which(colnames(full) %in% rm_eco), drop = FALSE]
  
  sm <- full
  sm[sm >= thr] <- NA
  sm[sm == 0]   <- NA
  k_r <- rowSums(!is.na(sm)) > 0
  k_c <- colSums(!is.na(sm)) > 0
  sm  <- sm[k_r, k_c, drop = FALSE]
  
  old <- options(knitr.kable.NA = "")
  on.exit(options(old))
  
  # Return the kable objects rather than print()-ing them here: calling print()
  # directly on a knitr_kable inside a helper function bypasses knitr's own
  # print-interception (which dispatches to knit_print.knitr_kable and renders a
  # proper HTML table), so it was falling back to base print.default and dumping
  # raw pipe-table text as plain, unrendered text. Returning them instead lets the
  # calling chunk auto-print each one (with the chunk option results='asis' set),
  # which renders them correctly.
  list(
    full   = knitr::kable(as.data.frame.matrix(full), na = ""),
    sparse = knitr::kable(as.data.frame.matrix(sm), na = "")
  )
  
}
compare_related_directions <- function(
    df_in, target_objective, related_fun_groups,
    obj_col = 'objective', fun_col = 'fun_group', sig_col = 'sig', effect_col = 'effect'
) {
  # Compares what happened to OTHER cover responses while `target_objective` was
  # being pursued - e.g. when the objective was "decrease_tre", what did AFG, SHR
  # and PFG cover do in those same treated pixels?
  #
  # This works on the *raw combined data* (df_in), not on `summaries`, because
  # split_by_objective() only ever pairs an objective with its own matching cover
  # response (decrease_afg <-> AFG, etc.) - it never keeps, say, the SHR rows for
  # pixels where the objective was decrease_tre. To look at those other responses
  # we have to re-filter the combined data directly.
  
  require(dplyr)
  require(ggplot2)
  
  df <- df_in[grepl(target_objective, df_in[[obj_col]]), ]
  df <- df[df[[fun_col]] %in% related_fun_groups, ]
  df <- df[df$year_diff > 0, ]
  
  stopifnot(nrow(df) > 0)
  
  tbl <- df |>
    group_by(.data[[fun_col]]) |>
    summarise(
      n_pix       = n(),
      pct_sig     = round(100 * mean(.data[[sig_col]]), 2),
      pct_sig_pos = round(100 * mean(.data[[sig_col]] & .data[[effect_col]] > 0), 2),
      pct_sig_neg = round(100 * mean(.data[[sig_col]] & .data[[effect_col]] < 0), 2),
      .groups = 'drop'
    )
  
  p0 <- df |>
    ggplot(aes(x = .data[[fun_col]], fill = as.character(.data[[sig_col]]))) +
    geom_bar(position = 'fill') +
    labs(
      x = 'Cover response', y = 'Proportion of pixels',
      fill = 'DART result\nsignificant?',
      title = paste0('Cover responses when the objective was "', target_objective, '"')
    ) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))
  
  return(list(table = tbl, plot = p0, data = df))
  
}
summarize_sig_effect <- function(
    df, group_col, effect_dir = c('positive', 'negative'),
    baseline = NULL, run_pairwise = TRUE, label = group_col,
    sig_col = 'sig', effect_col = 'effect'
) {
  
  effect_dir <- match.arg(effect_dir)
  cmp <- if (effect_dir == 'positive') `>` else `<`
  
  df$.sig_flag <- df[[sig_col]] == TRUE & cmp(df[[effect_col]], 0)
  
  grp_summary <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(
      n_pix   = dplyr::n(),
      n_sig   = sum(.sig_flag),
      pct_sig = round(100 * n_sig / n_pix, 2),
      .groups = 'drop'
    ) |>
    dplyr::arrange(dplyr::desc(pct_sig))
  
  p0 <- grp_summary |>
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(.data[[group_col]], pct_sig), y = pct_sig)) +
    ggplot2::geom_col(fill = 'grey35') +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = label, y = paste0('% of pixels with a significant ', effect_dir, ' effect'),
      title = paste0('Significant ', effect_dir, ' DART effects, by ', tolower(label))
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(color = 'black'))
  
  pairwise_df <- NULL
  
  if (run_pairwise) {
    
    if (is.null(baseline)) {
      baseline <- grp_summary[[group_col]][which.max(grp_summary$n_pix)]
    }
    
    other_levels <- setdiff(unique(df[[group_col]]), baseline)
    
    pairwise_list <- lapply(other_levels, function(lvl) {
      sub_df <- df[df[[group_col]] %in% c(baseline, lvl), ]
      ft <- stats::fisher.test(table(sub_df[[group_col]], sub_df$.sig_flag))
      
      data.frame(
        level            = lvl,
        n_pix            = sum(sub_df[[group_col]] == lvl),
        pct_sig          = round(100 * mean(sub_df$.sig_flag[sub_df[[group_col]] == lvl]), 2),
        baseline_pct_sig = round(100 * mean(sub_df$.sig_flag[sub_df[[group_col]] == baseline]), 2),
        p_value          = ft$p.value
      )
    })
    
    pairwise_df <- do.call(rbind, pairwise_list)
    pairwise_df$p_adj <- stats::p.adjust(pairwise_df$p_value, method = 'holm')
    pairwise_df <- pairwise_df[order(pairwise_df$p_adj), ]
    
  }
  
  return(list(summary = grp_summary, plot = p0, baseline = baseline, pairwise = pairwise_df))
  
}