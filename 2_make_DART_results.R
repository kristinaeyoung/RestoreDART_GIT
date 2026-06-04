# BEM June 2026

library(ggplot2)
source('plot_functions.R')
source('helper_functions.R')

fig_dir <- '../results/figures'
in_fl <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data_03062026.csv'

d0 <- read.csv(in_fl)

d_by_obj <- split_by_objective(d0)

{
  # should be a function? or at least a loop
  # check sample sizes
  
  df_in <- d_by_obj[[1]]
  # treatment
  table(df_in$tx_coarse)
  # ecoregion
  table(df_in$us_l4code)
  # treatment x ecoregion level, for pixel
  table(df_in$tx_coarse, df_in$us_l4code)
  # plot the pixel frequency histogram
  pix_table <- as.data.frame(table(df_in$tx_coarse, df_in$us_l4code))
  colnames(pix_table) <- c("tx_coarse", "us_l4code", "Freq")
  
  ggplot(pix_table, aes(x = Freq)) +
    geom_histogram(bins = 60, fill = "grey35", linewidth = 0.3) +
    labs(title = "Histogram of Pixel Counts", x = "Count", y = "Frequency") +
    theme_classic()
  
  ggplot(pix_table[which(pix_table$Freq > 0), ], aes(x = Freq)) +
    geom_histogram(bins = 60, fill = "grey35", linewidth = 0.3) +
    labs(title = "Histogram of Pixel Counts (n > 0)", x = "Count", y = "Frequency") +
    theme_classic()
  
  # treatment x ecoregion level, for polygon
  df_poly <- df_in[, c('polygon', 'tx_coarse', 'us_l4code')]
  df_poly <- df_poly[!duplicated(df_poly), ]
  table(df_poly$tx_coarse, df_poly$us_l4code)
  # plot the polygon frequency histogram
  poly_df <- data.frame(count = as.vector(table(df_poly$tx_coarse, df_poly$us_l4code)))
  poly_n_breaks <- max(counts_df) + 1
  
  ggplot(poly_df, aes(x = count)) +
    geom_histogram(bins = poly_n_breaks, fill = "grey35", colour = "white", linewidth = 0.3) +
    labs(x = "Count", y = "Frequency", title = "Polygon counts\n(tx_coarse × us_l4code)") +
    theme_classic()
}


# check results
{
  #  "decrease_afg_AFG"
  round((table(df_in$sig) / nrow(df_in)) * 100, 1)
  # 72.1% not-significant, 27.9% significant
  round((nrow(filter(df_in, sig == T & effect > 0)) / nrow(df_in)) * 100, 1)
  # 22.5% significantly positive
  round((nrow(filter(df_in, sig == T & effect < 0)) / nrow(df_in)) * 100, 1)
  # 5.4% significantly negative
}

{
  # plot
  # this should be its own function loop
  i_d <- 1
  
  i_ob <- paste(unlist(strsplit(names(d_by_obj)[i_d], '_'))[c(1, 2)], collapse = '_')
  i_fg <- unlist(strsplit(names(d_by_obj)[i_d], '_'))[3]
  i_nm <- paste(i_ob, i_fg, '1.png', sep = '_')
  
  png(file.path(fig_dir, i_nm), 25, 9, 'in', res = 150)
  plot_sig_dart_pix(d_by_obj[[i_d]], 1, i_fg, i_ob)
  dev.off()
}
