#' Comparison of two ROC objects
#'
#' Comparison of two ROC objects
#'
#' @param roc1 roc object
#' @param roc2 roc object
#' @param venkatraman add the result of venkatraman method
#' @return data.frame
#' @keywords roc_comparison
#' @export
#'
roc_comparison = function(roc1, roc2, venkatraman=F){
  method = c('delong', 'bootstrap')
  findf = data.frame()
  for(m in method){
    fit = roc.test(roc1, roc2, method=m)
    p.value = round(fit$p.value, 3)
    comparison_method = fit$method
    roc1_auc = round(ci.auc(roc1, of='auc', method=m), 3)
    Model1_AUC = paste0(roc1_auc[2], ' (', roc1_auc[1], '-', roc1_auc[3], ')')
    roc2_auc = round(ci.auc(roc2, of='auc', method=m), 3)
    Model2_AUC = paste0(roc2_auc[2], ' (', roc2_auc[1], '-', roc2_auc[3], ')')
    df = data.frame(Model1_AUC, Model2_AUC, p.value, 'method'=m)
    findf = rbind(findf, df)
  }
  if (venkatraman == T){
    fit = roc.test(roc1, roc2, method='venkatraman')
    p.value = round(fit$p.value, 3)
    comparison_method = fit$method
    Model1_AUC = paste0(round(fit$roc1$auc, 3), ' (not available)')
    Model2_AUC = paste0(round(fit$roc2$auc, 3), ' (not available)')
    df = data.frame(Model1_AUC, Model2_AUC, p.value, 'method'='venkatraman')
    findf = rbind(findf, df)
  }
  return(findf)
}
