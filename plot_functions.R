HighResTiff <- function(plot_obj, file, width_in, height_in, resolution_dpi, ...) {
  
  if (inherits(plot_obj, 'ggplot')) {
    tiff(filename = file, width = width_in, height = height_in, units = 'in', res = resolution_dpi)
    print(plot_obj)
    dev.off()
  } else if (inherits(plot_obj, 'list')) {
    tiff(filename = file, width = width_in, height = height_in, units = 'in', res = resolution_dpi)
    Multiplot(plotlist = plot_obj, ...)
    dev.off()
  }
  invisible()
  
}
#' @rdname plot_functions
#' @export
lm_eqn <- function(df0, xvar, yvar){
  
  df <- data.frame(x = df0[[xvar]], y = df0[[yvar]])
  
  m <- lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}
#' @rdname plot_functions
#' @export
Multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  # BEM note: this is pretty much verbatim copied from the R cookbook
  #
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    #plot(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      #plot(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
fig_1 <- function(input_df) {
  
  d1_f1 <- dplyr::group_by(
    input_df,
    years_since_treatment,
    eco,
    tx_coarse
  )
  
  d1_f1 <- dplyr::summarise(
    d1_f1,
    prop_sig = mean(sig_effect, na.rm = TRUE),
    .groups = "drop"
  )
  
  fig_1 <- ggplot2::ggplot(
    d1_f1,
    ggplot2::aes(
      x = years_since_treatment,
      y = tx_coarse,
      fill = prop_sig
    )
  ) +
    ggplot2::geom_tile() +
    # ggplot2::geom_text(aes(label = signif(prop_sig, 2)), size = 3) +
    ggplot2::facet_wrap(~ eco, scales = 'free_y') +
    #ggplot2::coord_fixed() +
    ggplot2::scale_fill_viridis_c(
      option = "magma",
      limits = c(0, 1),
      name = "Proportion\nSignificant\nDART Pixels"
    ) +
    ggplot2::labs(
      x = "Years Since Treatment",
      y = "Treatment (Coarse)",
      title = "What proportion of treated areas significantly increased PFG?",
      subtitle = "(when objective was increase_PFG)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(color = "black"),
      axis.text.y = ggplot2::element_text(color = "black"),
      strip.text = ggplot2::element_text(color = "black")
    )
  
  return(fig_1)
}
fig_2 <- function(input_df) {
  
  d1_f2 <- dplyr::group_by(
    input_df,
    years_since_treatment,
    tx_coarse
  )
  
  d1_f2 <- dplyr::summarise(
    d1_f2,
    prop_sig = mean(sig_effect, na.rm = TRUE),
    .groups = "drop"
  )
  
  fig_2 <- ggplot2::ggplot(
    d1_f2,
    ggplot2::aes(
      x = years_since_treatment,
      y = tx_coarse,
      fill = prop_sig
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(label = signif(prop_sig, 2)),
      size = 2
    ) +
    # ggplot2::facet_wrap(~ eco) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_viridis_c(
      option = "magma",
      limits = c(0, 1),
      name = "Proportion\nSignificant\nDART Pixels"
    ) +
    ggplot2::labs(
      x = "Years Since Treatment",
      y = "Treatment (Coarse)",
      title = "What proportion of treated areas significantly increased PFG?",
      subtitle = "(when objective was increase_PFG)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(color = "black"),
      axis.text.y = ggplot2::element_text(color = "black"),
      strip.text = ggplot2::element_text(color = "black")
    )
  
  return(fig_2)
}
plot_effects <- function(data, y_var = "effect") {
  
  ggplot(data,
         aes(x = year_RAP,
             y = .data[[y_var]],
             color = tx_coarse,
             fill = tx_coarse)) +
    geom_ribbon(aes(ymin = lower,
                    ymax = upper),
                alpha = 0.2,
                color = NA) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ us_l4code) +
    labs(x = "Year (RAP)",
         y = y_var,
         color = "Treatment",
         fill = "Treatment") +
    theme_bw() +
    theme(
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title.x = element_text(color = "black"),
      axis.title.y = element_text(color = "black")
    )
}