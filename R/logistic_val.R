#' Validation of logistic regression results with the combiation of variables
#'
#' Validation of logistic regression results with the combiation of variables
#'
#' @param data a dataframe to be used
#' @param outcome binary outcome variable
#' @param vars predictor variables
#' @param auc.cutoff cut-off value of AUC
#' @param n.aic number of the combination to be showed (arranged by AIC values)
#' @param n.bic number of the combination to be showed (arranged by BIC values)
#' @param fold cross-validation fold number
#' @param repeats cross-validation repeat number
#' @param boot.repeat boot-strap repeat number
#' @param maxvar maximum number of variables
#' @return data.frame object
#' @keywords roc_comparison
#' @export
#' @examples
#'
logistic_val = function(data, outcome, vars, auc.cutoff=0.65,
                        n.aic=10, n.bic=10, fold=10, repeats=100, boot.repeat=1000,
                        maxvar=ceiling(min(table(data[[outcome]])) * 0.1)+2){  #one in ten rule
  all_list = list()
  fin = data.frame()
  for(i in 1:min(maxvar, length(vars))){
    all_list = append(all_list, combn(vars, i, simplify = F))
  }
  for(i in all_list){
    a = paste0(i, collapse = '+')
    fit = glm(as.formula(paste(outcome, '~', a)), data=data, family=binomial)
    data$fitted = predict(fit, newdata=data, type='response')
    roc = suppressWarnings(roc(data[[outcome]], data$fitted))
    b = data.frame('var'=a, 'AIC'=AIC(fit), 'BIC'=BIC(fit), 'AUC'=as.numeric(roc$auc))
    fin = suppressWarnings(bind_rows(fin, b))
  }
  fin_aic = fin %>% arrange(AIC) %>% head(n.aic)
  fin_bic = fin %>% arrange(BIC) %>% head(n.bic)
  fin_ab = suppressMessages(fin_aic %>% full_join(fin_bic))
  for(i in fin_ab$var){
    ctrl = trainControl(method = "repeatedcv", number=fold, repeats=repeats, savePredictions = TRUE)
    trainfit = train(as.formula(paste(outcome, '~', i)), data=data, method="glm",
                     family="binomial", trControl = ctrl, tuneLength = 5, na.action  = na.pass)
    fin[fin$var == i, 'CVacc'] = trainfit$results$Accuracy

    ctrl = trainControl(method = "boot", number=boot.repeat, savePredictions = TRUE)
    trainfit = train(as.formula(paste(outcome, '~', i)), data=data, method="glm",
                     family="binomial", trControl = ctrl, tuneLength = 5, na.action  = na.pass)
    fin[fin$var == i, 'BSacc'] = trainfit$results$Accuracy
  }
  return(fin %>% arrange(BIC))
}



#' Validation of logistic regression results of multiple models
#'
#' Validation of logistic regression results of multiple models
#'
#' @param data a dataframe to be used
#' @param outcome binary outcome variable
#' @param models logistic regression models
#' @param auc.cutoff cut-off value of AUC
#' @param n.aic number of the combination to be showed (arranged by AIC values)
#' @param n.bic number of the combination to be showed (arranged by BIC values)
#' @param fold cross-validation fold number
#' @param repeats cross-validation repeat number
#' @param boot.repeat boot-strap repeat number
#' @return data.frame object
#' @keywords logistic_models_val
#' @export
#' @examples
#'
logistic_models_val = function(data, outcome, models,
                               n.aic=10, n.bic=10, fold=10, repeats=100, boot.repeat=1000){
  fin = data.frame()
  for(fit in models){
    fitname = as.character(fit$call)[2]
    data$fitted = predict(fit, newdata=data, type='response')
    roc = suppressWarnings(roc(data[[outcome]], data$fitted))
    ## Cross-validation
    ctrl = trainControl(method = "repeatedcv", number=fold, repeats=repeats, savePredictions = TRUE)
    trainfit = train(as.formula(fit$call), data=data, method="glm",
                     family="binomial", trControl = ctrl, tuneLength = 5, na.action  = na.pass)
    CVacc = trainfit$results$Accuracy
    ## Bootstrapping
    ctrl = trainControl(method = "boot", number=boot.repeat, savePredictions = TRUE)
    trainfit = train(as.formula(fit$call), data=data, method="glm",
                     family="binomial", trControl = ctrl, tuneLength = 5, na.action  = na.pass)
    BSacc = trainfit$results$Accuracy

    b = data.frame('var'=fitname, 'AIC'=AIC(fit), 'BIC'=BIC(fit), 'AUC'=as.numeric(roc$auc),
                   'CVacc'=CVacc, 'BSacc'=BSacc)
    fin = suppressWarnings(bind_rows(fin, b))
  }
  return(fin)
}
