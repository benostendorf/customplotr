##' Customized Q-Q plot
##'
##' Creates a customized QQ-Plot and optionally exports it as a pdf.
##' @param data data.frame with columns with normalized gene expression values
##' and one column with a group stratifier, replicates/cells in rows
##' @param var character; name of gene to plot QQ-plot for, column in data
##' @param group character; column name data to use as stratifying variable in QQ-plot
##' @param pal character; custom palette for dot colours
##' @param filename character or \code{NULL}; filename for export or no export
##' @param width numeric; width of pdf export in cm
##' @param height numeric; height of pdf export in cm
##' @param return.data logical; return plot as list instead of printing
##' @author Benjamin N. Ostendorf
##'
##' @example
##' \dontrun{

##' }
##' @export
##'
##' @import ggplot2
##' @importFrom ggrastr geom_point_rast
##' @importFrom scales pretty_breaks
##'
custom_qqplot <- function(data,
                          var,
                          group,
                          width = NULL,
                          height = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          no.labels = FALSE,
                          pal = NULL,
                          filename = NULL,
                          title = TRUE,
                          raster = TRUE,
                          within.plot.label = FALSE,
                          return.data = FALSE) {

  ## Export plot as pdf if filename is passed to function
  if (is.character(filename)) {
    pdf(
      file = filename,
      width = width,
      height = height,
      pointsize = 7,
      useDingbats = FALSE
    )
  }

  group_1 <- names(table(data[, group]))[1]
  group_2 <- names(table(data[, group]))[2]
  var_group_1 <- as.numeric(data[, var][data[, group] == group_1])
  var_group_2 <- as.numeric(data[, var][data[, group] == group_2])
  max_var <- max(var_group_1, var_group_2)
  qqplot_res <- as.data.frame(qqplot(var_group_1, var_group_2, plot.it = FALSE))
  qqplot_res$direction <- ifelse(qqplot_res$x > qqplot_res$y, "group1_larger",
                                 ifelse(qqplot_res$y > qqplot_res$x, "group1_smaller",
                                 ifelse(qqplot_res$x == qqplot_res$y, "equal", NA)))
  qqplot_res$direction <- factor(qqplot_res$direction, ordered = TRUE)
  wilcox_res <- format.pval(wilcox.test(var_group_1, var_group_2)$p.value, digits = 3)

  ifelse(no.labels,
         axis.title_style <- element_blank(),
         axis.title_style <- element_text(size = 6))

  ifelse(title,
         plot.title_style <- element_text(size = 6, hjust = 0.5, face = "italic"),
         plot.title_style <- element_blank())

  ifelse(nlevels(qqplot_res$direction) == 3,
                 manual_color_scale <- c("black", "#ca0020", "#0571b0"),
         ifelse("group1_larger" %in% levels(qqplot_res$direction),
                manual_color_scale <- c("black", "#ca0020"),
                manual_color_scale <- c("black", "#0571b0")))

  ifelse(title,
         plot_margins <- margin(0.4, 0.1, 0.1, 0.1, "cm"),
         plot_margins <- margin(0.1, 0.1, 0.1, 0.1, "cm"))

  plot <-
    ggplot(qqplot_res) + {
      if (raster) ggrastr::geom_point_rast(aes(x = x, y = y, color = direction), size = 5,
                                                 raster.width = width, raster.height = height, raster.dpi = 600)
    } + {
      if (!raster) geom_point(aes(x = x, y = y, color = direction), size = 0.5, alpha = 0.2, shape = 16)
    } + {
      if (within.plot.label) annotate(geom = "text",
                                      x = 0,
                                      y = ceiling(max_var) * 0.95, hjust = 0,
                                      size = 2, label = var, fontface = 'italic')
    } +
    geom_abline(intercept = 0, slope = 1, color = "gray", size = custom_linewidth) +
    annotate(geom = "text",
             x = ceiling(max_var) * 1, hjust = 1,
             y = ceiling(max_var) * 0.05,
             size = 1.5,
             label = paste0("p = ", wilcox_res)) +
    xlim(0, ceiling(max_var)) +
    # ylim(0, ceiling(max_var)) +
    xlab(label = ifelse(is.null(xlab), group_1, xlab)) +
    # ylab(label = ifelse(is.null(ylab), group_2, ylab)) +
    scale_y_continuous(name = waiver(),
                       breaks = scales::pretty_breaks(ifelse(max_var < 2, 2, 4)),
                       limits = c(0, ceiling(max_var))
                       ) +
    labs(title = var) +
    scale_color_manual(values = ifelse(rep(is.null(pal), 3), manual_color_scale, pal), aesthetics = "color") +
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = custom_linewidth),
          axis.title = axis.title_style,
          axis.text = element_text(size = 5, color = "black"),
          axis.line = element_line(size = custom_linewidth),
          axis.ticks = element_line(size = custom_linewidth),
          axis.ticks.length = unit(0.075, "cm"),
          plot.title = plot.title_style,
          aspect.ratio = 1,
          legend.position = "none",
          plot.margin = plot_margins)


  if (return.data) {
    return(plot)
  } else {
    print(plot)
  }

  ## Close pdf export
  if (is.character(filename)) {
    dev.off()
    ## Crop whitespace
    system(paste("pdfcrop", filename, filename))
    message("Plot saved under ", filename)
  }
}
