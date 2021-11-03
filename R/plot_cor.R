#' Correlation plot
#'
#' Draw a correlation plot
#'
#' @param data a dataframe to be used
#' @param x a variable for x-axis
#' @param y a variable for y-axis
#' @param r correlation coefficient
#' @param p P-value
#' @param xpos relative location of correlation coefficient
#' @param ypos relative location of correlation coefficient
#' @param x2pos relative x location of P-value
#' @param y2pos relative y location of P-value
#' @param method Pearson or Spearman test
#' @param round round
#' @param rsize rsize
#' @param low.limit low.limit
#' @param ci confidence interval
#' @return ggplot object
#' @keywords plot_cor
#' @export
#' @import ggplot2
#'
plot_cor = function(data, x, y, xlab=x, ylab=y, r=T, p=T,
                   xpos=0.7, ypos=0.7, x2pos=xpos, y2pos=ypos-0.1, method='pearson',
                   round = 3, rsize = 4, low.limit=F, ci=T){
  suppressWarnings(suppressMessages(library(ggplot2, quietly=T)))
  suppressWarnings(suppressMessages(library(dplyr, quietly=T)))
  data = data[!is.na(data[[x]]),]
  data = data[!is.na(data[[y]]),]

  if(method == 'pearson'){
    fit = cor.test(data[[x]], data[[y]], method = 'pearson')
    rtext = 'italic(R) == '
  }
  else{
    fit = suppressWarnings(cor.test(data[[x]], data[[y]], method = 'spearman'))
    rtext = 'italic(r[s]) == '
  }
  rvalue = round(fit$estimate, round)
  pvalue = round(fit$p.value, round)

  g1 = ggplot(data, aes_string(x = x, y = y)) +
    geom_point() +
    stat_smooth(method=lm, se=ci, color='blue', size=0.5, alpha=0.2) +
    xlab(xlab) +
    ylab(ylab)

  ymin = ggplot_build(g1)$layout$panel_params[[1]]$y.range[1]
  ymax = ggplot_build(g1)$layout$panel_params[[1]]$y.range[2]
  xmin = ggplot_build(g1)$layout$panel_params[[1]]$x.range[1]
  xmax = ggplot_build(g1)$layout$panel_params[[1]]$x.range[2]
  xposition = xmin + xpos * (xmax - xmin)
  yposition = ymin + ypos * (ymax - ymin)
  x2position = xmin + x2pos * (xmax - xmin)
  y2position = ymin + y2pos * (ymax - ymin)

  if(r == T){
    g1 = g1 +
      annotate("text", x=xposition, y=yposition, parse=T, label=paste0(rtext, rvalue),
               hjust=0, size=rsize)
  }
  if(p == T){
    if(fit$p.value <0.001){
      g1 = g1 +
        annotate("text", x=x2position, y=y2position, parse=T, label=paste0('italic(P) < 0.001'),
                 hjust=0, size=rsize)
    }
    else{
      g1 = g1 +
        annotate("text", x=x2position, y=y2position, parse=T, label=paste0('italic(P) == ', pvalue),
                 hjust=0, size=rsize)
    }
  }
  if(low.limit == T){
    ylim = max(data[[y]]) + abs(max(data[[y]])) * 0.15
    g1 = g1 + ylim(min(data[[y]]) - abs(ylim)*0.05, ylim)
  }
  g1
}

