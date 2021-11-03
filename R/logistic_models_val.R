#' Validation of logistic regression results of multiple models
#'
#' Validation of logistic regression results of multiple models
#'
#' @param data a dataframe to be used
#' @param outcome binary outcome variable
#' @param models logistic regression models
#' @param n.aic number of the combination to be showed (arranged by AIC values)
#' @param n.bic number of the combination to be showed (arranged by BIC values)
#' @param fold cross-validation fold number
#' @param repeats cross-validation repeat number
#' @param boot.repeat boot-strap repeat number
#' @return data.frame object
#' @keywords logistic_models_val
#' @export
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
