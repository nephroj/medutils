#' Automation of univariable logistic analysis
#'
#' Automation of univariable logistic analysis
#'
#' @param data a dataframe to be used
#' @param outcome a outcome variable
#' @param varlist predictors
#' @param round round value
#' @param print.result print the name of significant vairables
#' @param p.cut P-value cut-off
#' @return data.frame
#' @keywords logistic_uni
#' @export
#'
logistic_uni = function(data, outcome, varlist, round=3, print.result=T, p.cut=0.05) {
  cont_dt = data.frame()
  sigvar = c()
  for (i in varlist){
    formula = as.formula(paste(outcome, "~", i))
    fit = glm(formula, family=binomial, data=data)
    result = suppressMessages(data.frame(
      names(fit$coef),
      round(exp(summary(fit)$coef[ ,1]), round),
      round(exp(confint(fit)), round),
      round(summary(fit)$coef[,4], round)
    )) %>%
      rename(var=1, OR=2, LCI=3, UCI=4, p.value=5) %>%
      filter(row_number() != 1)
    if (result$p.value < 0.05) {
      sigvar = c(sigvar, i)
    }
    cont_dt = rbind(cont_dt, result)
  }
  if(print.result==T){
    sigchr = paste0(sigvar, collapse=', ')
    cat('## Significant variables (p < ', p.cut, '): \n', sigchr, sep='')
  }
  return(cont_dt)
}

