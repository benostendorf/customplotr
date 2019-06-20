## color palettes
## old medium hue for gray was 'gray85'
require(RColorBrewer)
color_palettes <- list(
  four_colors = c("#d6604d", "gray75", "#4393c3", "darkorchid"),
  six_colors = c("#ca0020",  "#f4a582", "gray75", "#92c5de", "#0571b0", "darkorchid"),
  YlOrBr = brewer.pal(5, "YlOrBr"),
  YlOrRd = brewer.pal(4, "YlOrRd"),
  Greens = brewer.pal(5, "Greens"))
usethis::use_data(color_palettes, overwrite = TRUE)
