##' Plot customized boxplot
##'
##' Creates a customized barchart and optionally exports it as a pdf.
##' @param data matrix; contains variables x and y
##' @param filename character or \code{NULL}; filename for export or no export
##' @param width numeric; width of pdf export in cm
##' @param height numeric; height of pdf export in cm
##' @param x character; variable for x aesthetic
##' @param y character; variable for y aesthetic
##' @param ylab character; y-axis label
##' @param log_y logical; switch logarithmic display of y-axis
##' @param jitter logical; overplot with jittered individual datapoints
##' @param custom_palette character; define color palette for x
##' @param title character; title of plot
##' @example
##' \dontrun{
##' custom_boxplot(data = dataframe,
##'                filename = "plots/boxplot.pdf",
##'                x = "categorical_variable",
##'                fill_variable = "numeric_variable",
##'                title = "Plot title")
##' }
##' @export
##' @import tidyverse
custom_boxplot <-
  function(data,
           filename = NULL,
           width = 1.4,
           height = 1.55,
           x,
           y,
           ylab = NULL,
           log_y = FALSE,
           title = NULL,
           jitter = FALSE,
           custom_palette = NULL) {

    ## load required packages
    require(grid)
    require(gridExtra)

    ## Export plot as pdf if filename is passed to function
    if (is.character(filename)){
      pdf(file = filename,
          width = width,
          height = height,
          pointsize = 7,
          useDingbats = FALSE)
    }

    ## Convert tibble to data frame
    if (is_tibble(data)) {data <- as.data.frame(data)}

    ## Drop empty levels from dataframe and remove lines with NAs
    ## (the latter prevents ggplot from emitting warnings about removing NAs!!)
    data[, x] <- factor(data[, x])
    data <- data[!(is.na(data[, x])), ]

    ## Define color palette for plot depending on number of groups
    if (is.null(custom_palette)) {
      ifelse(nlevels(data[, x]) < 5,
             pal <- color_palettes$four_colors[1:nlevels(data[, x])],
             pal <- color_palettes$six_colors[1:nlevels(data[, x])]
      )
    } else {
      pal <- custom_palette
    }

    ## define boxplot fill color depending on jitter option
    # if (jitter) {
    #   boxplot_fill <- NA
    # } else {
    #   boxplot_fill <- pal
    # }

    ## ggplot
    p <- ggplot(data = data, aes_string(x = x, y = y)) +
      geom_boxplot(
        fill = pal,
        alpha = ifelse(jitter, 0.5, 1),
        lwd = 72.27 / 96 * 0.5,
        outlier.size = 1,
        outlier.stroke = 72.27 / 96 * 0.5,
        outlier.shape = ifelse(jitter, NA, 1)
      ) +
      {
        if (jitter)
          geom_jitter(
            aes_string(fill = x),
            alpha = 0.8,
            size = 1.5,
            shape = 21,
            colour = "black",
            stroke = 72.27 / 96 * 0.5,
            show.legend = FALSE,
            width = 0.35
          )
      } +
      {if(jitter)scale_fill_manual(values = pal)} +
      {if(log_y)scale_y_continuous(trans = 'log2')} +
      # scale_y_continuous(breaks = c(0, 20, 40, 60), limits = c(0, 70)) +
      theme_bw() +
      theme_custom +
      ggtitle(title) +
      ylab(ifelse(!is.null(ylab), ylab, y)) +
      geom_text(data = data %>% count_(x),
                aes_string(
                  x = x,
                  y = ifelse(log_y, max(data[, y], na.rm = TRUE) ^ 1.1, max(data[, y], na.rm = TRUE) * 1.1),
                  vjust = -1.72,
                  label = bquote(paste0("(", n, ")")),
                  fill = NULL),
                size = 5*5/14)  +
      if (jitter == TRUE) {
        geom_text(data = data %>% count_(x),
                  aes_string(
                    x = x,
                    y = ifelse(log_y, max(data[, y], na.rm = TRUE) ^ 1.1, max(data[, y], na.rm = TRUE) * 1.1),
                    vjust = -1.72,
                    label = bquote(paste0("(", n, ")")),
                    fill = NULL),
                  size = 5*5/14)
      }

    # Override clipping
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)

    ## Close pdf export
    if (is.character(filename)) {
      dev.off()

      ## Crop whitespace
      system(paste("pdfcrop", filename, filename))

      message("Plot saved under ", filename)
    }
  }
