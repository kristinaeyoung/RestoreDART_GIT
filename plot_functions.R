# HighResTiff <- function(plot_obj, file, width_in, height_in, resolution_dpi, ...) {
#   
#   if (inherits(plot_obj, 'ggplot')) {
#     tiff(filename = file, width = width_in, height = height_in, units = 'in', res = resolution_dpi)
#     print(plot_obj)
#     dev.off()
#   } else if (inherits(plot_obj, 'list')) {
#     tiff(filename = file, width = width_in, height = height_in, units = 'in', res = resolution_dpi)
#     Multiplot(plotlist = plot_obj, ...)
#     dev.off()
#   }
#   invisible()
#   
# }
# lm_eqn <- function(df0, xvar, yvar){
#   
#   df <- data.frame(x = df0[[xvar]], y = df0[[yvar]])
#   
#   m <- lm(y ~ x, df)
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq))
# }
# Multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
#   # BEM note: this is pretty much verbatim copied from the R cookbook
#   #
#   # Multiple plot function
#   #
#   # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#   # - cols:   Number of columns in layout
#   # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#   #
#   # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#   # then plot 1 will go in the upper left, 2 will go in the upper right, and
#   # 3 will go all the way across the bottom.
#   require(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     #plot(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       #plot(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
plot_sig_dart_pix <- function(input_df, ptype = 1, ftype, obj) {
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
    scale_fill_viridis_c(option = "magma", limits = c(0, 1),name = "Proportion\nSignificant\nDART Pixels") +
    labs(
      x = "Years Since Treatment", y = "Treatment (Coarse)",
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