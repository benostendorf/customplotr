##' Customized plotcounts, based on DESeq2's plotCounts
##'
##' Plot normalized counts for multiple genes with facet wrapping.
##'
##' @param dds DESeq2 dataset; DESeq2 dataset object
##' @param genes character; names of gene(s) to plot
##' @param intgroup character; variable for grouping
##' @param group_levels character; state levels of intgroup for ordering
##' @param palette character; define color palette
##' @param stat.test character; e.g. "t.test" or "wilcox.test" for statistical comparison
##' @param stat.sidedness character; e.g. "two.sided", "less", "greater"
##' @param nrow numeric; number of rows for facetting
##'
##'
##' @export
##'
custom_plotcounts <-
  function(dds,
           genes,
           intgroup = "condition",
           group_levels = c("untreated", "treated"),
           palette = NULL,
           stat.test = "wilcox.test",
           stat.sidedness = "two.sided",
           nrow = 2){

    ## Bind output of plotcounts for individual genes
    data_ls <- lapply(genes, function(x) DESeq2::plotCounts(dds, x, returnData = TRUE))
    names(data_ls) <- genes
    df <- dplyr::bind_rows(data_ls, .id = "gene")

    if (is.null(palette)) {
      palette <- c("untreated" = "#A9A9A8", "treated" = "#0095FF")
    }

    ## Create dataframe for expansion of axis by given factor using `geom_blank`
    blank_data <-
      df |>
      dplyr::group_by(gene) |>
      dplyr::summarize(y = max(count) * 1.15) |>
      dplyr::mutate(condition = group_levels[length(group_levels)])

    ## Plot
    df %>%
      dplyr::mutate(condition = factor(.data[[intgroup]], levels = group_levels)) |>
      ggplot2::ggplot(aes(x = condition, y = count, fill = condition)) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", width = 0.8) +
      ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
      ggplot2::geom_jitter(size = 1, width = 0.2, alpha = 0.8, shape = 16) +
      ggpubr::stat_compare_means(method = stat.test,
                                 label = "p.format",
                                 method.args = list(alternative = stat.sidedness),
                                 size = 1.75, comparisons = list(group_levels),
                                 label.y.npc = 0.75) +
      ggplot2::labs(x = NULL) +
      ggplot2::scale_fill_manual(values = palette) +
      theme_custom2 +
      ggplot2::guides(x = guide_axis(angle = 60)) +
      ggplot2::theme(legend.position = "None",
            # plot.title = element_text(face = "italic"),
            strip.text = element_text(face = "italic", size = 6), #, margin = margin(1,0,1,0, "mm")),
            strip.background = element_blank()) +
      lemon::coord_capped_cart(left = "bottom", bottom = "both") +
      ggplot2::expand_limits(y = c(0)) +
      ## Extend y axis by given factor
      ggplot2::geom_blank(data = blank_data, aes(condition = group_levels[length(group_levels)], y = y)) +
      ggplot2::facet_wrap(~ gene, scales = "free_y", nrow = nrow) +
      NULL
  }
