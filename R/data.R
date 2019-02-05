##' Custom color palettes
##'
##' A dataset containing custom color palettes.
##' @format list of different color palettes
##' @docType data
##' @name color_palettes
##' @usage data(color_palettes)
##' @import RColorBrewer
NULL

## 'four_colors' and 'six_colors' palettes based on Mariathasan et al., Nature 2018
## (gray and darkorchid added)
# color_palettes <- list(
#   four_colors = c("#d6604d", "gray85", "#4393c3", "darkorchid"),
#   six_colors = c("#ca0020",  "#f4a582", "gray85", "#92c5de", "#0571b0", "darkorchid"),
#   YlOrBr = brewer.pal(5, "YlOrBr"),
#   Greens = brewer.pal(5, "Greens"))
# save(color_palettes, file = "data/color_palettes.RData")


##' Custom linewidth
##'
##' A variable containing a custom linewidth equalling XX pt.
##' @format numeric value
##' @docType data
##' @name custom_linewidth
##' @usage data(custom_linewidth)
NULL

# custom_linewidth <- 5/8 * 72.27 / 96 * 0.5
# save(custom_linewidth, file = "data/custom_linewidth.RData")


##' Custom ggplot theme
##'
##' A customized theme for ggplot2.
##' @format ggplot theme object
##' @docType data
##' @name theme_custom
##' @usage data(theme_custom)
##' @import ggplot2
NULL

# theme_custom <-
#   theme(
#     panel.grid = element_blank(),
#     panel.grid.major.y = element_line(colour = "gray", size = (custom_linewidth), linetype = "dotted"),
#     panel.border = element_rect(size = custom_linewidth, colour = "black"),
#     axis.text = element_text(size = 5,
#                              colour = "black"),
#     axis.text.x = element_text(angle = 45,
#                                hjust = 1),
#     axis.title = element_text(size = 6),
#     axis.title.y = element_text(margin = margin(r = 3)),
#     axis.title.x = element_blank(),
#     axis.line.x = element_blank(),
#     axis.ticks = element_line(size = custom_linewidth),
#     axis.ticks.length = unit(0.075, "cm"),
#     plot.title = element_text(size = 7, hjust = 0.5),
#     strip.text = element_text(size = 6, face = "bold"),
#     panel.background = element_blank(),
#     legend.text = element_text(size = 5),
#     legend.title = element_blank(),
#     legend.margin = margin(t = -1, r = 0, l = -0.3, unit = 'cm'),
#     legend.key.size = unit(0.25, "line")
#   )
# save(theme_custom, file = "data/theme_custom.RData")
