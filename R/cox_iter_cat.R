#' Making Cox PH models of multiple outcomes (categorical predictor variable)
#'
#' Making Cox PH models of multiple outcomes (categorical predictor variable)
#'
#' @param data a dataframe to be used
#' @param outcome_list list of outcomes
#' @param main_predictor main predictor variable
#' @param adjusted_var variables list for adjustment
#' @param round round
#' @return data.frame object
#' @keywords cox_iter_cat
#' @export
#'
cox_iter_cat = function(data, outcome_list, main_predictor, adjusted_var=NULL, round=4){
  outcome_result = data.frame()
  ## make formula for cox model using "adjusted_var"
  if (!is.null(adjusted_var) & length(adjusted_var) >0){
    adjusted_var_list = paste0("+", str_c(adjusted_var, collapse="+"))
  } else {
    adjusted_var_list = ""
  }
  data = data[!is.na(data[[main_predictor]]), ]             ## remove NAs of main_predictor
  data[[main_predictor]] = factor(data[[main_predictor]])   ## make main_predictor to factor
  main_predictor_n = length(unique(data[[main_predictor]])) ## count the number of the category of main_predictor

  for (outcome in outcome_list) {
    formula = paste0("Surv(", outcome, "Days, ", outcome, ") ~", main_predictor, adjusted_var_list)
    fit = coxph(as.formula(formula), data=data)
    data_slim = data[!is.na(data[[outcome]]), ]
    for (var in adjusted_var) {
      data_slim = data_slim[!is.na(data_slim[[var]]), ]
    }
    n = as.vector(table(data_slim[[main_predictor]]))
    event_n = xtabs(as.formula(paste0("~", main_predictor, "+", outcome)), data=data_slim)[,"1"]
    HR = c(1, round(exp(coef(fit))[1:main_predictor_n-1], round))
    LCI = c(NA, round(exp(confint(fit)[1:main_predictor_n-1, 1]), round))
    UCI = c(NA, round(exp(confint(fit)[1:main_predictor_n-1, 2]), round))
    p.value = c(NA, round(coef(summary(fit))[1:main_predictor_n-1, "Pr(>|z|)"], round))
    if (sum(is.na(HR)) >0 | max(UCI, na.rm=T) > 10^5 | min(LCI, na.rm=T) < 10^-5){
      HR = c(1, rep(NA, main_predictor_n-1))
    }
    result = data.frame(
      outcome=c(outcome, rep("", main_predictor_n-1)),
      group=paste0(main_predictor, "=", levels(data[[main_predictor]])),
      n, event_n, HR, LCI, UCI, p.value, stringsAsFactors = F
    )
    outcome_result = rbind(outcome_result, result)
  }
  rownames(outcome_result) = NULL
  return(outcome_result)
}
