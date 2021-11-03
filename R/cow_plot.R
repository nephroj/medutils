#' Creation of the cowplot object
#'
#' Creation of the cowplot object
#'
#' @param data a dataframe to be used
#' @param outcome a outcome variable
#' @param varlist predictors
#' @param round round value
#' @param print.result print the name of significant vairables
#' @param p.cut P-value cut-off
#' @return cowplot obejct
#' @keywords cow_plot
#' @export
#' @examples
#'
cow_plot = function (n, ncol=n, font_size=10, scale=0.95, rel_widths=1, uppercase=T, labels=NULL) {
  text = paste0("g", 1:n)
  if(!is.null(labels)) {
    labels = labels
  } else if(uppercase==T) {
    labels = LETTERS[1:n]
  } else {
    labels = letters[1:n]
  }
  gg_list = list()
  for (t in text) {
    gg_list = c(gg_list, list(eval(parse(text=t))))
  }
  theme_set(theme_cowplot(font_size=font_size))
  plot_grid(plotlist=gg_list, labels=labels, ncol=ncol, scale=scale, rel_widths=rel_widths)
}
