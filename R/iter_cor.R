#' Iteration of correlation analyses with a specific variable
#'
#' This function perform multiple correlation analyses between a specific variable
#' and various continuous variables
#'
#' @param data a dataframe to be used
#' @param varlist a vector of continuous variables
#' @param var a variable of interest
#' @param method a character string indicating which correlation coefficient is to be used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @param print.result a logical indicating whether to show significant variables
#' @param p.round a specified number of decimal places to which rounds the values
#' @return Dataframe containing the result of correlation analyes
#' @keywords iter_cor
#' @export
#'
iter_cor = function(data, varlist, var, method = 'pearson', print.result = F, p.round=3){
  dt.wilc = data.frame()
  lowp = c()
  for(i in varlist){
    vec = data[[i]]
    if(sum(!is.na(vec))==0 | length(unique(vec[!is.na(vec)]))==1) next   ## NA로만 이루어져 있거나 NA 제외하고 값이 하나만 있는 경우는 skip!
    if(class(vec)=="numeric" | class(vec)=="integer"){                   ## interger나 numeric 형태만 분석
      d1 = suppressWarnings(cor.test(vec, data[[var]], method=method))
      dt = data.frame(lab = i, R = round(d1$estimate, p.round),
                      p.value = round(d1$p.value, p.round))
      dt.wilc = rbind(dt.wilc, dt)
      if(d1$p.value < 0.05 & !is.na(d1$p.value)){
        lowp = c(lowp, i)
      }
    }
  }
  if(print.result == T){
    cat('Significant variables:', paste0(lowp, collapse=', '), '\n')
  }
  rownames(dt.wilc) = NULL
  return(dt.wilc)
}
