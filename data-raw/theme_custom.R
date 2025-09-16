theme_custom <-
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "gray", size = (custom_linewidth), linetype = "dotted"),
    panel.border = ggplot2::element_rect(size = custom_linewidth, colour = "black"),
    axis.text = ggplot2::element_text(size = 5,
                             colour = "black"),
    axis.text.x = ggplot2::element_text(angle = 45,
                               hjust = 1),
    axis.title = ggplot2::element_text(size = 6),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 3)),
    axis.title.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(size = custom_linewidth),
    axis.ticks.length = ggplot2::unit(0.075, "cm"),
    plot.title = ggplot2::element_text(size = 7, hjust = 0.5),
    strip.text = ggplot2::element_text(size = 6, face = "bold"),
    panel.background = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 5),
    legend.title = ggplot2::element_blank(),
    # legend.margin = ggplot2::margin(t = -1, r = 0, l = -0.3, ggplot2::unit = 'cm'),
    legend.key.size = ggplot2::unit(0.25, "line")
  )
usethis::use_data(theme_custom, overwrite = TRUE)
