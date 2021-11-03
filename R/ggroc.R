#' Draw ROC plot with cut-off values
#'
#' Draw ROC plot with cut-off values
#'
#' @param roc roc object made by pROC package
#' @param showAUC show AUC value
#' @param interval interval
#' @param breaks breaks
#' @param round round value
#' @param show.num number of cut-off values
#' @param print.result print analysis results
#' @param print.num number of cut-off values to be printed
#' @param font.size font size
#' @return ggplot object
#' @keywords ggroc
#' @export
#'
ggroc <- function(roc, showAUC = TRUE, interval = 0.2, breaks = seq(0, 1, interval),
                  round=2, show.num=3, print.result=F, print.num=5, font.size=3.5){
  if(class(roc) != "roc")
    simpleError("Please provide roc object from pROC package")
  plotx <- rev(roc$specificities)
  ploty <- rev(roc$sensitivities)
  best = pROC::coords(roc=roc, x = "local maximas", ret=c('threshold', 'specificity', 'sensitivity'))
  bestdt = as.data.frame(best)
  bestdt$youden = bestdt$specificity + bestdt$sensitivity - 1
  bestdt = bestdt[order(bestdt$youden, decreasing=T),]
  rownames(bestdt) = NULL

  g1 = ggplot(NULL, aes(x = plotx, y = ploty)) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5) +
    geom_step() +
    scale_x_reverse(name = "Specificity",limits = c(1,0), breaks = breaks,
                    expand = c(0.001,0.001)) +
    scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks,
                       expand = c(0.001, 0.001)) +
    theme_bw() +
    theme(axis.ticks = element_line(color = "grey30")
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank()
    ) +
    coord_equal() +
    annotate("text", x = 0.2, y = 0.2, vjust = 1, size=font.size,
             label = paste("AUC =",sprintf("%.3f",roc$auc)))

  for(i in 1:show.num){
    g1 = g1 +
      annotate("point", x=bestdt[i, 'specificity'], y=bestdt[i, 'sensitivity']) +
      annotate("text", x=bestdt[i, 'specificity']-0.01, bestdt[i, 'sensitivity']-0.01, size=font.size,
               label=paste0(round(bestdt[i, 'threshold'],round)," (",
                            round(bestdt[i, 'specificity'],round),", ",
                            round(bestdt[i, 'sensitivity'],round),")"),
               hjust=0, vjust=1)
  }
  if(print.result == T){
    print(head(bestdt, print.num))
  }
  return(g1)
}



#' Draw multiple ROC plots
#'
#' Draw multiple ROC plots
#'
#' @param roclist a list of roc objects
#' @param showAUC show AUC value
#' @param interval interval
#' @param breaks breaks
#' @param labels label of ROC lines
#' @return ggplot object
#' @keywords ggrocs
#' @export
#'
ggrocs <- function(roclist, showAUC = TRUE, interval = 0.2, breaks = seq(0, 1, interval),
                   labels = NULL){
  if(class(roclist) != "roc" & class(roclist) != "list"){
    simpleError("Please provide roc object from pROC package or list of roc objects")
  }
  dat = data.frame()
  aucvec = c()
  for(i in 1:length(roclist)){
    if(class(roclist[[i]]) != "roc"){
      simpleError("Please provide roc object from pROC package")
    }
    dat1 = data.frame(key = paste('Model', i), plotx = rev(roclist[[i]]$specificities), ploty = rev(roclist[[i]]$sensitivities))
    dat = rbind(dat, dat1)
    if (is.null(labels)){
      aucvec = c(aucvec, paste('Model', i,'  ',"AUC =",sprintf("%.3f", roclist[[i]]$auc)))
    } else {
      aucvec = c(aucvec, paste(labels[i],'  ',"AUC =",sprintf("%.3f", roclist[[i]]$auc)))
    }

  }
  ggplot(dat, aes(x = plotx, y = ploty)) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), color='gray70') +
    geom_step(aes(linetype=key)) +
    scale_x_reverse(name = "Specificity",limits = c(1,0), breaks = breaks,
                    expand = c(0.001,0.001)) +
    scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks,
                       expand = c(0.001, 0.001)) +
    theme_bw() +
    theme(axis.ticks = element_line(color = "grey30"),
          legend.position=c(0.9,0.1), legend.justification=c(1,0),
          legend.background = element_blank(),
          legend.key=element_blank()
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank()
    ) +
    labs(linetype = NULL) +
    coord_equal() +
    scale_linetype_discrete(labels=aucvec)
}
