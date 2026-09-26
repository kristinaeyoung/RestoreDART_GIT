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
plot_all_DART_sig <- function(input_df, obj, ptype = 1, ftype = "") {

    require(dplyr)
    require(ggplot2)

    fdf <- input_df |>
      group_by(year_diff, us_l4name, tx_coarse) |>
      summarise(prop_sig = mean(sig, na.rm = T), .groups = "drop") |>
      mutate(prop_sig = ifelse(prop_sig == 0, 0.001, prop_sig)) |>
      mutate(prop_sig = ifelse(prop_sig == 1, 0.999, prop_sig)) |>
      mutate(prop_sig_char = format(round(prop_sig, 2), nsmall = 2))

    p0 <- fdf |>
      ggplot(aes(x = year_diff, y = tx_coarse, fill = prop_sig)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", limits = c(0, 1),name = "Proportion\nsignificant\nDART pixels") +
      labs(
        x = "Years since treatment", y = "Treatment",
        title = paste0("What proportion of treated areas significantly increased ", ftype, "?"),
        subtitle = paste0("(when objective was ", obj, ")")
      ) +
      theme_bw() +
      theme(axis.text = element_text(color = 'black'), strip.text = element_text(color = 'black'))

    if (ptype == 1) {
      p0 <- p0 +
        facet_wrap(~ us_l4name, scales = 'free_y')
    } else if (ptype == 2) {
      p0 <- p0 +
        #geom_text(aes(label = prop_sig_char), size = 2, check_overlap = T) +
        coord_fixed()
    } else {
      stop('bad ptype')
    }

    return(p0)

  }