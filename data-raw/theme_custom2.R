theme_custom2 <-
  ggplot2::theme(
    # text = ggplot2::element_text(family = "Helvetica-Narrow"),
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(size = custom_linewidth),
    axis.ticks = ggplot2::element_line(size = custom_linewidth),
    axis.ticks.length = ggplot2::unit(0.075, "cm"),
    # axis.text.x = ggplot2::element_text(hjust = 1),
    axis.title = ggplot2::element_text(size = 6),
    axis.text = ggplot2::element_text(size = 5, colour = "black"),
    plot.title = ggplot2::element_text(size = 7, hjust = 0.5),
    panel.background = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 5),
    legend.title = ggplot2::element_blank(),
    # legend.margin = margin(t = -1, r = 0, l = -0.3, ggplot2::unit = 'cm'),
    legend.key.size = ggplot2::unit(0.25, "line")
  )
usethis::use_data(theme_custom2, overwrite = TRUE)
