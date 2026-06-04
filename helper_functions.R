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
  combos$match_fun_group[which(combos$fun_group == 'BAR')] <- 'BAR'
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
    col_eco      = "us_l4code",
    col_sig      = "sig",
    col_effect   = "effect",
    bins         = 60
) {
  
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
    labs(x = "Count", y = "Frequency", title = "Polygon counts\n(for each level of: tx_coarse × us_l4code)") +
    scale_x_continuous(breaks = seq(max(poly_df))) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black'))
  
  # percent of significantly different DART pixels:
  pix_sig     <- round((table(df_in[[col_sig]]) / nrow(df_in)) * 100, 1)[['TRUE']]
  # percent of significantly different DART pixels that show a positive effect:
  pix_sig_pos <- round((nrow(df_in[df_in[[col_sig]] == T & df_in[[col_effect]] > 0, ]) / nrow(df_in)) * 100, 1)
  # percent of significantly different DART pixels that show a negative effect:
  pix_sig_neg <- round((nrow(df_in[df_in[[col_sig]] == T & df_in[[col_effect]] < 0, ]) / nrow(df_in)) * 100, 1)
  
  return(list(
    tbl_tx        = tbl_tx,
    tbl_eco       = tbl_eco,
    tbl_tx_eco    = tbl_tx_eco,
    tbl_poly      = tbl_poly,
    pixel_plot_0  = pixel_plot_0,
    pixel_plot_1  = pixel_plot_1,
    poly_plot     = poly_plot,
    pix_sig       = pix_sig,
    pix_sig_pos   = pix_sig_pos,
    pix_sig_neg   = pix_sig_neg
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