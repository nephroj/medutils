#' Make a histogram categorized by grouping variable
#'
#' This function make a histogram categorized by grouping variable
#'
#' @param data a dataframe to be used
#' @param var a numeric variable
#' @param group a grouping variable (factor)
#' @param show.hist a logical indicating whether to show histogram
#' @param show.density a logical indicating whether to show density plot
#' @param show.mean a logical indicating whether to show mean values
#' @param bins number of bins
#' @param alpha transparency of density plot
#' @return ggplot2 histogram
#' @keywords hist_group
#' @export
#' @examples hist_group(mtcars, "mpg", "am")
#'
hist_group = function(data, var, group, show.hist=T, show.density=T, show.mean=T, bins=50, alpha=0.5){
  mu = data %>% group_by(!!as.name(group)) %>% summarise(grp.mean = mean(!!as.name(var), na.rm=T))
  g1 = ggplot(data, aes(x=!!as.name(var), color=!!as.name(group), fill=!!as.name(group))) +
    theme_classic()

  if (show.hist == TRUE) {g1 = g1 + geom_histogram(aes(y=..density..), position="identity", alpha=alpha, bins=bins)}
  if (show.density == TRUE) {g1 = g1 + geom_density(alpha=alpha)}
  if (show.mean == TRUE) {g1 = g1 + geom_vline(data=mu, aes(xintercept=grp.mean, color=!!as.name(group)), linetype="dashed")}

  return(g1)
}
