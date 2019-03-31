## color palettes
require(RColorBrewer)
color_palettes <- list(
  four_colors = c("#d6604d", "gray85", "#4393c3", "darkorchid"),
  six_colors = c("#ca0020",  "#f4a582", "gray85", "#92c5de", "#0571b0", "darkorchid"),
  YlOrBr = brewer.pal(5, "YlOrBr"),
  YlOrRd = brewer.pal(4, "YlOrRd"),
  Greens = brewer.pal(5, "Greens"))
usethis::use_data(color_palettes, overwrite = TRUE)
