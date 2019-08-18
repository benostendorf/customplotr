## color palettes
## old medium hue for gray was 'gray85'
## palette "Okabe_Ito" was taken from Okabe and Ito, 2008, and was recommended by Claus Wilke, 2019
require(RColorBrewer)
color_palettes <- list(
  four_colors = c("#d6604d", "gray75", "#4393c3", "darkorchid"),
  six_colors = c("#ca0020",  "#f4a582", "gray75", "#92c5de", "#0571b0", "darkorchid"),
  YlOrBr = brewer.pal(5, "YlOrBr"),
  YlOrRd = brewer.pal(4, "YlOrRd"),
  Greens = brewer.pal(5, "Greens"),
  Okabe_Ito = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"))
usethis::use_data(color_palettes, overwrite = TRUE)
