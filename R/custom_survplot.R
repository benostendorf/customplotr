##' Plot customized survival graphs
##'
##' Creates a customized survival plot and optionally exports it as a pdf.
##' The risk.table argument of this function is based on the 'plotSurvival2' function developed
##' by Yuanyuan Xiao and Dorothee Nickles as part of the 'Imvigor210CoreBiologies'
##' package (http://research-pub.gene.com/IMvigor210CoreBiologies/).
##' @param survFit survfit object
##' @param filename character or \code{NULL}; filename for export or no export
##' @param title character; plot title
##' @param xlab character; x-axis label
##' @param ylab character; y-axis label
##' @param xmax numeric; x-axis limit
##' @param trend logical; toggle test for trend in logrank test
##' @param custom_palette character; color palette for groups
##' @param custom_legends character; overwrite legend labels (see warning below)
##' @param risk.table logical; toggle generation of number at risk table below survival plot
##' @param mar numeric vector; plotting margins
##' @param width numeric; width of pdf export in cm
##' @param height numeric; height of pdf export in cm
##' @author Benjamin N. Ostendorf
##'
##' @example
##' \dontrun{
##' survival.plot(survFit = survfit,
##'               filename = "plots/survival.pdf",
##'               custom_legends = c("Group 1", "Group 2"),
##'               title = "Plot title")
##' }
##' @export
##'
##' @import survival
##' @import survminer
custom_survplot <- function(survFit,
                            filename = NULL,
                            title = NULL,
                            xlab = "Overall survival (years)",
                            ylab = "Survival probability",
                            xmax = NULL,
                            trend = FALSE,
                            custom_palette = NULL,
                            custom_legends = NULL,
                            risk.table = FALSE,
                            mar = c(12,9,3,2),
                            width = 2,
                            height = 2.2) {

  if((length(survFit$strata) < 3) && (trend == TRUE)) {
    warning("Test for trend can only be applied when there are more than 2 groups. \n
            Test for trend was set to 'FALSE'. ")
    trend <- FALSE
  }

  ## Warning re: correct order of custom labels
  if(!is.null(custom_legends)) {
    message("When passing custom legends ensure proper order of labels in following df:")
    print(data.frame(custom_legends = custom_legends,
                     names_in_df = sapply(strsplit(names(survFit$strata), "="), "[", 2)))
  }

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
  ## Define color palette for plot depending on number of groups
  ifelse(
    length(survFit$strata) < 5,
    pal <- color_palettes$four_colors[1:length(survFit$strata)],
    pal <- color_palettes$six_colors[1:length(survFit$strata)]
  )
  ## Integrate custom palettes
  ifelse(is.null(custom_palette),
         pal <- pal,
         pal <- custom_palette)

  # ifelse(is.null(title), par(mar = c(2.3, 2.7, 0, 0)), par(mar = c(2.3, 2.7, 1.7, 0)))
  ## Customize x-axis limit if not provided
  if (is.null(xmax)) {
    xmax <- ifelse(max(survFit$time) <= 4 * 365.25, 4 * 365.25,
                   ifelse(max(survFit$time) < 5 * 365.25, 5 * 365.25,
                          ifelse(max(survFit$time) < 10 * 365.25, 10 * 365.25,
                                 ifelse(max(survFit$time) < 15 * 365.25, 15 * 365.25,
                                        ifelse(max(survFit$time) < 20 * 365.25, 20 * 365.25, max(survFit$time))))))
  }
  ## Plot
  p <- plot(
    survFit,
    frame = FALSE,
    lwd = 1.5,
    col = pal,
    xscale = 365.25,
    mark.time = TRUE,
    cex = 0.5,
    cex.axis = 5 / 7,
    xmax = xmax,
    yaxt = "n",
    xaxt = "n"
  )
  axis(1,
       at = seq(0, xmax, ifelse(xmax <= 4*365.25, 365.25, 730.5)),
       seq(0, xmax, ifelse(xmax <= 4*365.25, 365.25, 730.5))  / 365.25,
       cex.axis = 5 / 7,
       lwd = 5 / 8,
       tck = -0.025,
       padj = -1.8)
  axis(2,
       at = seq(0, 1, 0.2),
       seq(0, 1, 0.2),
       las = 2,
       cex.axis = 5 / 7,
       lwd = 5 / 8,
       tck = -0.025,
       hadj = 0.5)
  title(xlab = xlab,
        line = 1.3,
        cex.lab = 6 / 7)
  title(ylab = ylab,
        line = 1.8,
        cex.lab = 6 / 7)

  ## Plot title
  mtext(title,
        side = 3,
        # at = 0.5 * max(survFit$time),
        adj = 0.35,
        line = 0.5)

  ## Generate vector for legend labels with group sizes
  ifelse(risk.table,
         groups <- paste0(sapply(strsplit(names(survFit$strata), "="), "[", 2)),
         groups <- paste0(sapply(strsplit(names(survFit$strata), "="), "[", 2), " (", survFit$n, ")")
         )

  ## Integrate custom legend labels
  ifelse(
    is.null(custom_legends),
    groups <- groups,
    groups <- paste0(custom_legends, " (", survFit$n, ")")
  )

  ## Legend; here, both colors and labels can reversed in analogy to curves
  ## by adding 'rev()' to both the assignments for 'legend' and 'col'
  legend(
    "topright",
    legend = groups,
    col = pal,
    lty = 1,
    bty = "n",
    cex = 5 / 7
  )

  ## Include log-rank p-value
  pvalue <- survminer::surv_pvalue(survFit, test.for.trend = trend)$pval
  text(0.2 * xmax, 0.1,
       bquote(paste(italic("P") ~ "=" ~ .(format.pval(pvalue, digits = 2)))),
       cex = 6 /7)

  ## Risk table
  if (risk.table) {
    group.labels <- gsub(x = names(survFit$strata), pattern = ".*=| .*", "")

    ## Compute number at risk
    time.pt <- seq(0, xmax, ifelse(xmax <= 1826.25, 182.625,
                                   ifelse(xmax <= 6*365.25, 365.25, 730.5)))
    ix = 0
    n.risk = c()
    for (kk in 1:(length(survFit$strata))) {
      fit.n.risk = survFit$n.risk[(ix+1) : (ix + survFit$strata[kk])]
      fit.time = survFit$time[(ix+1) : (ix + survFit$strata[kk])]
      tmp = findInterval(time.pt, fit.time)
      n.risk=rbind(n.risk,
                   ifelse(tmp < length(fit.time), fit.n.risk[tmp+1], 0)
      )
      ix = ix + survFit$strata[kk]
    }
    dimnames(n.risk)[[2]] = time.pt

    if (mar[1] < 4 + length(group.labels)) {
      mar[1] <- 4 + length(group.labels)
    }
    org.mar <- par()$mar
    par(mar = mar)

    ## Plot number at risk
    cust_margin_dist <- seq(from = 9.4, by = 0.8, length.out = 10)
    for (i in 1:length(group.labels)) {
      mtext(side = 1,
            at = -0.24 * xmax,
            line = cust_margin_dist[i],
            text = group.labels[i],
            col = pal[i],
            adj = 0,
            cex = 5/7)
      mtext(side = 1,
            at = time.pt,
            line = cust_margin_dist[i],
            text = n.risk[i, ],
            # col = pal[i],
            cex = 5/7)
    }

    ## Reset mar that was changed to allow adding numbers underneath plot
    if (risk.table) {
      par(mar = org.mar)
    }
  }

  ## Close pdf export
  if (is.character(filename)) {
    dev.off()
    ## Crop whitespace
    system(paste("pdfcrop", filename, filename))
    message("Plot saved under ", filename)
  }
}
