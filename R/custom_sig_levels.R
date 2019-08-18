##' Add significance labels (*, **, ***, ****) to data frame
##'
##' Takes a data frame as input with a column named `p_val` and returns the dataframe
##' with the additional column `signifcance` with significance levels
##' @param data data frame including one column named "p_val"
##' @param column character column holding p-values in data
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
##' @importFrom dplyr mutate
custom_sig_levels <- function(data,
                              column = "p_val"
                              ) {

  if(!(column %in% colnames(data))) {
    warning("This data frame does not contain the specificed column
            holding p-values (default: 'p_val'")
  }

   data_mut <- mutate(data,
                      significance = case_when(p_val < 0.0001 ~ "****",
                                               p_val < 0.001 ~ "***",
                                               p_val < 0.01 ~ "**",
                                               p_val < 0.05 ~ "*",
                                               p_val >= 0.05 ~ "n.s."))
   return(data_mut)

}