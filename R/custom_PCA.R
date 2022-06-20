##'  Plot customized PCA
##'
##' This function is customized version of DESeq2's plotPCA function.
##'
##' @param object Deseq dataset object
##' @param intgroup character; variable defining groups
##' @param ntop numeric; number of genes with highes variance to include in analysis
##' @param returnData logical; return data rather than plot
##' @param custom_pal character; colors to customize group colors
##'
##' @export
custom_PCA <-
  function(object,
           intgroup = "condition",
           ntop = 500,
           pointsize = 1,
           returnData = FALSE,
           custom_pal = NULL) {

    rv <- rowVars(assay(object))
    select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
    pca <- prcomp(t(assay(object)[select,]))
    percentVar <- pca$sdev ^ 2 / sum(pca$sdev ^ 2)

    ## Integrate custom group palette
    ifelse(
      is.null(custom_pal),
      pal <- color_palettes$four_colors[c(2, 1, 3)],
      pal <- custom_pal
    )

    if (!all(intgroup %in% names(colData(object)))) {
      stop("the argument 'intgroup' should specify columns of colData(dds)")
    }
    intgroup.df <- as.data.frame(colData(object)[, intgroup,
                                                 drop = FALSE])
    group <- if (length(intgroup) > 1) {
      factor(apply(intgroup.df, 1, paste, collapse = ":"))
    }
    else {
      colData(object)[[intgroup]]
    }
    d <-
      data.frame(
        PC1 = pca$x[, 1],
        PC2 = pca$x[, 2],
        group = group,
        intgroup.df,
        name = colnames(object)
      )
    if (returnData) {
      attr(d, "percentVar") <- percentVar[1:2]
      return(d)
    }
    ggplot2::ggplot(data = d, aes_string(x = "PC1", y = "PC2", color = "group")) +
      geom_point(size = pointsize, alpha = 0.75, stroke = 0) +
      xlab(paste0("PC1: ", round(percentVar[1] * 100), " % variance")) +
      ylab(paste0("PC2: ", round(percentVar[2] * 100), " % variance")) +
      scale_color_manual(values = pal) +
      coord_fixed() +
      theme_bw() +
      theme(panel.border = element_rect(size = custom_linewidth, colour = "black"),
            panel.grid = element_blank(),
            panel.grid.major = element_line(size = (custom_linewidth), color = "grey85", linetype = "dotted"),
            legend.title = element_blank(),
            axis.text = element_text(size = 5, colour = "black"),
            axis.title = element_text(size = 6),
            axis.ticks = element_line(size = custom_linewidth),
            axis.ticks.length = unit(0.075, "cm"),
            axis.title.y = element_text(margin = margin(r = 3.2)),
            axis.title.x = element_text(margin = margin(t = 4)),
            plot.title = element_text(size = 7, hjust = 0.5),
            strip.text = element_text(size = 6, face = "bold"),
            panel.background = element_blank(),
            legend.text = element_text(size = 5, margin = margin(l = -10)),
            legend.margin =  margin(-10, 0, -10, -10),
            legend.key.height = unit(2, "mm"))
  }
