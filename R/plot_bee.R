#' Beeswarm plot with confidence intervals
#'
#' Draw a beeswarm plot
#'
#' @param data a dataframe to be used
#' @param x a variable for x-axis
#' @param y a variable for y-axis
#' @param bar.width mean bar width
#' @param err.width error bar width
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param xlabels labels of x-axis groups
#' @param se SE (T) or 95% CI (F)
#' @param cex size of the dots
#' @param dot revealing dots or not
#' @param shape shape of the dots
#' @param bar_graph bar_graph (T) or beeswarm graph (F)
#' @return ggplot object
#' @keywords plot_bee
#' @export
#'
plot_bee = function(data, x, y, bar.width=0.5, err.width=0.15,
                    xlab=x, ylab=y, xlabels=levels(data[[x]]), se=F, cex=2.5, dot=T,
                    shape=rep(16, 10), bar_graph=F) {
  data[[x]] = factor(data[[x]])
  data = data[!is.na(data[[x]]),]
  data = data[!is.na(data[[y]]),]

  dtc = data %>%
    group_by(!!as.name(x)) %>%
    summarise(
      Average = mean(!!as.name(y), na.rm=TRUE),
      SD = sd(!!as.name(y), na.rm=TRUE),
      P_n = n()
    ) %>%
    mutate(
      P_se = SD/sqrt(P_n),
      uci = Average + ifelse(se==T, 1, qt(0.975, P_n-1)) * P_se,
      lci = Average - ifelse(se==T, 1, qt(0.975, P_n-1)) * P_se
    )

  if (bar_graph){

    # Bar graph
    g1 = ggplot(dtc, aes_string(x=x, fill=x)) +
      geom_hline(yintercept=0, color="gray60") +
      geom_bar(aes(y=Average, group=1), stat = "identity", width=bar.width, color="black") +
      geom_errorbar(width = err.width, aes(ymin = lci, ymax = uci)) +
      scale_x_discrete(labels = xlabels, name = xlab) +
      scale_fill_grey(start=1, end=0) +
      ylab(ylab) +
      guides(fill="none")
  } else{

    # Beeswarm graph
    dtc = suppressMessages(left_join(data, dtc, by=x))

    g1 = ggplot(dtc, aes_string(x=x, fill=x, shape=x))

    if (dot==T){
      g1 = g1 + geom_beeswarm(aes_string(y=y), cex=cex, color='grey70')
    }
    g1 = g1 +
      geom_errorbar(width = err.width, aes(ymin = lci, ymax = uci)) +
      geom_crossbar(aes(y=Average, ymin=Average, ymax=Average), width=0.5, size=0.4) +
      scale_x_discrete(labels = xlabels, name = xlab) +
      scale_fill_grey(start=1, end=0) +
      scale_shape_manual(values=shape) +
      ylab(ylab) +
      guides(fill="none", shape="none")
  }

  return(g1)
}
