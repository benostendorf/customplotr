##' Plot customized barplot
##'
##' Creates a customized barchart and optionally exports it as a pdf.
##' @param data matrix; contains variables x and fill_variable
##' @param filename character or \code{NULL}; filename for export or no export
##' @param x character; variable for x aesthetic
##' @param fill_variable character; variable for fill aesthetic
##' @param custom_palette character; define palette of fill_variable
##' @param title character
##' @param ylab character; y-axis label
##'
##'
##' @export
custom_barplot <-
  function(data,
           filename = NULL,
           x,
           fill_variable,
           custom_palette = NULL,
           title = "Plot title",
           ylab = "Fraction of patients") {

    ## Convert to data frame
    data <- as.data.frame(data)

    ## Drop empty factor levels
    data[, x] <- factor(data[, x])
    data[, fill_variable] <- factor(data[, fill_variable])

    ## Export plot as pdf if filename is passed to function
    if (is.character(filename)) {
      pdf(
        file = filename,
        width = ifelse(nlevels(data[, x]) == 2, 1.2, ifelse(nlevels(data[, x]) == 3, 1.3, 1.45)),
        height = 1.6,
        pointsize = 7,
        useDingbats = FALSE
      )
    }

    ## Define color palette for plot depending on number of groups
    ## cave: here color palettes were reversed since stack order of bars was also reversed
    if (is.null(custom_palette)) {
      ifelse(
        nlevels(data[, fill_variable]) < 5,
        pal <-
          rev(customplotr::color_palettes$four_colors[1:nlevels(data[, fill_variable])]),
        pal <-
          rev(customplotr::color_palettes$six_colors [1:nlevels(data[, fill_variable])])
      )
    } else {
      pal <- rev(custom_palette)
    }

    ## Create auxiliary dataframe, reverse stacking order
    tmp_table <- table(data[, x], data[, fill_variable])
    tmp_rev <- tmp_table[, ncol(tmp_table):1]
    n_samples <- rowSums(tmp_table)

    ## barplot with reverted legends
    par(mar = c(2.3, 2.5, 2, 0), lwd = 0.5)
    p <- barplot(
      height = prop.table(t(tmp_rev), margin = 2),
      cex.names = 6 / 7,
      cex.axis = 5 / 7,
      cex.lab = 6 / 7,
      legend.text = rev(levels(data[, fill_variable])),
      col = pal,
      width = ifelse(nlevels(data[, x]) == 2, 0.18, 0.11),
      xlim = c(0, 1),
      args.legend = list(bty = "n",
                         cex = 5 / 7,
                         x = "topright"),
      xaxt = "n",
      yaxt = "n"
    )

    ## x-axis labels
    text(
      x = p,
      y = par("usr")[3] - 0.04,
      labels = levels(data[, x]),
      srt = 45,
      xpd = TRUE,
      adj = 1,
      cex = 5 / 7
    )
    ## y-axis with tick labels
    axis(
      2,
      at = seq(0, 1, 0.2),
      seq(0, 1, 0.2),
      las = 2,
      cex.axis = 5 / 7,
      lwd = 5 / 8,
      tck = -0.035,
      hadj = 0.5
    )
    ## y-axis title
    title(ylab = ylab,
          line = 1.6,
          cex.lab = 6 / 7)
    ## Add group sizes on top
    mtext(
      paste0("(", n_samples, ")"),
      side = 3,
      at = p,
      line = 0.1,
      cex = 0.7
    )
    ## Plot title
    mtext(
      title,
      side = 3,
      at = median(p),
      adj = 0.5,
      line = 1
    )

    # Close pdf export
    if (is.character(filename)) {
      dev.off()
      message("Plot saved under ", filename)
    }
  }
