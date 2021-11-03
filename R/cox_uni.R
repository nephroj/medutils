#' Automation of univariable Cox PH analysis
#'
#' Automation of univariable Cox PH analysis
#'
#' @param data a dataframe to be used
#' @param event a event variable
#' @param eventtime eventtime variable
#' @param vars predictors
#' @param p.cut P-value cut-off
#' @param round round value
#' @param digits digits
#' @param nsmall nsmall
#' @param print.result print the name of significant vairables
#' @return data.frame
#' @keywords cox_uni
#' @export
#'
cox_uni = function(data, event, eventtime, vars, p.cut=0.05,
                   round=3, digits=1, nsmall=3,  print.result=F){
  univar = data.frame()
  errorvar = c()
  sigvar = c()
  for(i in vars){
    if((class(data[[i]])=='factor' | class(data[[i]])=='character') &
       sum(table(data[[event]], data[[i]]) == 0) >1){
      errorvar = c(errorvar, i)
    }
    else if((class(data[[i]])=='numeric' | class(data[[i]])=='integer') &
            sum(apply(table(data[[i]], data[[event]]), 2, sum) == 0) !=0){
      errorvar = c(errorvar, i)
    }
    else{
      fit = suppressWarnings(suppressMessages(coxph(as.formula(paste0('Surv(', eventtime, ',', event, ') ~', i)), data=data)))
      if(summary(fit)$coef[1,5] < p.cut){
        sigvar = c(sigvar, i)
      }
      b = data.frame(
        'var' = i,
        'HR'= format(round(summary(fit)$coef[1,2], round), nsmall=nsmall, digits=digits, scientific = 2),
        'LCI'= format(round(exp(confint(fit)[1,1]), round), nsmall=nsmall, digits=digits, scientific = 2),
        'UCI'= format(round(exp(confint(fit)[1,2]), round), nsmall=nsmall, digits=digits, scientific = 2),
        'p.value'= format(round(summary(fit)$coef[1,5], round), nsmall=nsmall+1, digits=digits, scientific = 2),
        stringsAsFactors = F)
      rownames(b) = NULL
      univar = suppressWarnings(suppressMessages(rbind(univar, b)))
    }
  }
  if(print.result==T){
    sigchr = paste0(sigvar, collapse=' + ')
    errorchr = paste0(errorvar, collapse=', ')
    cat('## Significant variables (p < ', p.cut, '): \n', sigchr, sep='')
    cat('\n\n## Error variables:\n', errorchr, sep='')
  }
  return(univar)
}
