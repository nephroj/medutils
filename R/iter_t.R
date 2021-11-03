#' Iteration of Student's t-tests with a specific variable
#'
#' This function perform Student's t-tests between a specific variable
#' and various categorical variables
#'
#' @param data a dataframe to be used
#' @param varlist a vector of categorical variables
#' @param var a variable of interest
#' @param method parametric (1) or non-parametric test (2)
#' @param maxgroup a maximum number of groups
#' @param p.round a specified number of decimal places to which rounds the values
#' @return Dataframe containing the result of t-tests
#' @keywords iter_t
#' @export
#' @examples
#'
iter_t = function(data, varlist, var, method=1, maxgroup=5, p.round=3){
  dt.wilb = data.frame()
  for(i in varlist){
    data[[i]] = factor(data[[i]])
    levels.num = length(levels(data[[i]]))
    if(length(levels(data[[i]])) == 2 & length(unique(data[[i]])) >1){
      if(method == 1){
        vareq = leveneTest(as.formula(paste(var, '~', i)), data=data)
        if(vareq[["Pr(>F)"]][1] < 0.05){
          fit = t.test(as.formula(paste(var, '~', i)), data=data, var.equal=F)
          test = 't(welch)'
        }
        else{
          fit = t.test(as.formula(paste(var, '~', i)), data=data, var.equal=T)
          test = 't-test'
        }
        pvalue = fit$p.value
      }
      else if(method == 2){
        d1 = suppressWarnings(wilcox.test(as.formula(paste(var, "~", i)), data=data))
        pvalue = d1$p.value
      }
      else{
        cat('"method" should be "1 (normal)" or "2 (non-normal)"')
        stop()
      }
      dtc = data.frame(
        lab = i, p.value = round(pvalue, p.round),
        levels.num = levels.num, test = test
      )
      dt.wilb = rbind(dt.wilb, dtc)
    }
    else if(length(levels(data[[i]])) >= 3 & length(levels(data[[i]])) <= maxgroup & length(unique(data[[i]])) >= 3){
      if(method == 1){
        vareq = leveneTest(as.formula(paste(var, '~', i)), data=data)
        if(vareq[["Pr(>F)"]][1] < 0.05){
          fit = oneway.test(as.formula(paste(var, "~", i)), data=data, var.equal=F)
          test = 'anova(welch)'
        }
        else{
          fit = oneway.test(as.formula(paste(var, "~", i)), data=data, var.equal=T)
          test = 'anova'
        }
        pvalue = fit$p.value
      }
      else if(method == 2){
        d1 = suppressWarnings(kruskal.test(as.formula(paste(var, "~", i)), data=data))
        pvalue = d1$p.value
        test = "kruskal"
      }
      dtc = data.frame(
        lab = i, p.value = round(pvalue, p.round),
        levels.num = levels.num, test = test
      )
      dt.wilb = rbind(dt.wilb, dtc)
    } else{
      dtc = data.frame(
        lab = i, p.value = 999,
        levels.num = levels.num, test = "error"
      )
      dt.wilb = rbind(dt.wilb, dtc)
    }
  }
  rownames(dt.wilb) = NULL
  return(dt.wilb)
}
