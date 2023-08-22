##' Perform differential gene expression analysis for select feature and select
##' cell subset; perform p-adjustment for number of subsets to test for;
##' this function is based on Seurat's FindMarkers function and adds custom variables to
##' the resulting data frame
##'
##' Takes a Seurat object as input and requires specification of
##' idents and features to test for
##' @param data Seurat object
##' @param idents character; idents in Seurat object to perform DEA on;
##' passed to Seurat::FindMarkers
##' @param features character; genes in Seurat object to perform DEA on;
##' passed to Seurat::FindMarkers
##' @param grouping_var character; meta-feature of data to group for DEA;
##' passed to Seurat::FindMarkers
##' @param ident.1 character; specify group (one level in grouping_var) for denominator);
##' passed to Seurat::FindMarkers
##' @param min.pct numeric; define minimum fraction of cells in idents to perform DEA on;
##' passed to Seurat::FindMarkers
##' @param logfc.threshold numeric; passed to Seurat::FindMarkers
##' @author Benjamin N. Ostendorf
##'
##' @export
custom_FindMarkers <- function(data,
                       idents = NULL,
                       features,
                       grouping_var = "genotype",
                       ident.1 = NULL,
                       min.pct = 0,
                       logfc.threshold = 0
                       ) {

  if(is.null(idents)) {
    idents <- levels(Idents(data))
  }
  if(is.null(ident.1)) {
    ident.1 <- levels(as.factor(data@meta.data[, grouping_var]))[2]
  }

  DEA_results <- data.frame()

  for (subset in idents) {

    res <- Seurat::FindMarkers(
      data,
      subset.ident = subset,
      features = features,
      ident.1 = ident.1,
      group.by = grouping_var,
      min.pct = min.pct,
      logfc.threshold = logfc.threshold
      )

    res$subset <- subset
    res$feature <- rownames(res)

    res <-
      res %>%
      dplyr::group_by(feature) %>%
      dplyr::mutate(p_val_adj_subsets_BH = p.adjust(p_val, method = "BH", n = nlevels(data)),
             sig_p_val_adj_BH = case_when(p_val_adj_subsets_BH < 0.0001 ~ "****",
                                      p_val_adj_subsets_BH < 0.001 ~ "***",
                                      p_val_adj_subsets_BH < 0.01 ~ "**",
                                      p_val_adj_subsets_BH < 0.05 ~ "*",
                                      p_val_adj_subsets_BH >= 0.05 ~ "n.s."),
             sig_p_val_nonadj = case_when(p_val < 0.0001 ~ "****",
                                          p_val < 0.001 ~ "***",
                                          p_val < 0.01 ~ "**",
                                          p_val < 0.05 ~ "*",
                                          p_val >= 0.05 ~ "n.s.")) %>%
      dplyr::ungroup() %>%
      dplyr::rename(p_val_adj_all_bonf = p_val_adj)

    DEA_results <-
      dplyr::bind_rows(DEA_results, res)
  }
  return(dplyr::arrange(DEA_results, feature, subset))
}
