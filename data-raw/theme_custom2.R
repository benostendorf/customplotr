theme_custom2 <-
  ggplot2::theme(
    # text = element_text(family = "Helvetica-Narrow"),
    panel.grid = element_blank(),
    axis.line = element_line(size = custom_linewidth),
    axis.ticks = element_line(size = custom_linewidth),
    axis.ticks.length = unit(0.075, "cm"),
    # axis.text.x = element_text(hjust = 1),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 5, colour = "black"),
    plot.title = element_text(size = 7, hjust = 0.5),
    panel.background = element_blank(),
    legend.text = element_text(size = 5),
    legend.title = element_blank(),
    # legend.margin = margin(t = -1, r = 0, l = -0.3, unit = 'cm'),
    legend.key.size = unit(0.25, "line"),
    legend.key = element_blank()
  )
usethis::use_data(theme_custom2, overwrite = TRUE)
