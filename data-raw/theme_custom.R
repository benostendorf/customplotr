require(ggplot2)
theme_custom <-
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "gray", size = (custom_linewidth), linetype = "dotted"),
    panel.border = element_rect(size = custom_linewidth, colour = "black"),
    axis.text = element_text(size = 5,
                             colour = "black"),
    axis.text.x = element_text(angle = 45,
                               hjust = 1),
    axis.title = element_text(size = 6),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_line(size = custom_linewidth),
    axis.ticks.length = unit(0.075, "cm"),
    plot.title = element_text(size = 7, hjust = 0.5),
    strip.text = element_text(size = 6, face = "bold"),
    panel.background = element_blank(),
    legend.text = element_text(size = 5),
    legend.title = element_blank(),
    legend.margin = margin(t = -1, r = 0, l = -0.3, unit = 'cm'),
    legend.key.size = unit(0.25, "line")
  )
usethis::use_data(theme_custom, overwrite = TRUE)
