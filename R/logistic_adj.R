#' Adjusted logistic regression results with various variable combination
#'
#' Adjusted logistic regression results with various variable combination
#'
#' @param data a dataframe to be used
#' @param outcome binary outcome variable
#' @param predictor predictor variable
#' @param vars_list list of variable combination
#' @param nsmall nsmall
#' @param digits digits
#' @param p.round P-value round
#' @return data.frame object
#' @keywords logistic_adj
#' @export
#'
logistic_adj = function(data, outcome, predictor, vars_list, nsmall=2, digits=1, p.round=3){
  fit0 = glm(as.formula(paste(outcome, '~', predictor)),
             data=data, family=binomial)
  fitlist = list(fit0)
  for (vars in vars_list){
    fit = glm(as.formula(paste(outcome, '~', predictor, '+', paste0(vars, collapse='+'))),
              data=data, family=binomial)
    fitl = list(fit)
    fitlist = append(fitlist, fitl)
  }

  findt = data.frame()
  n = 0
  cat('## Association of', predictor, 'with the odds-ratio of', outcome, '\n')
  for (fit in fitlist){
    n = n + 1
    Models = paste0('Model ', n)
    OR = suppressMessages(exp(summary(fit)$coef[2,1]))
    CI = suppressMessages(exp(confint(fit))[2,])
    p.value = round(summary(fit)$coef[2,4], p.round)
    mddt = data.frame(
      'Models' = Models,
      'OR' = format(OR, nsmall=nsmall, digits=digits),
      'CI' = paste0(format(CI, nsmall=nsmall, digits=digits), collapse='-'),
      'p.value' = p.value
    )
    findt = rbind(findt, mddt)
    if(n == 1) cat('Model 1 : Not adjusted\n')
    else cat('Model', n, ': adjusted by', paste0(attr(fit$terms, "term.labels")[-1], collapse = ', '), '\n')
  }
  return(findt)
}
