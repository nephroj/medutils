#' Print multiple comparison results
#'
#' Print multiple comparison results
#'
#' @param data a dataframe to be used
#' @param x a variable for x-axis
#' @param y a variable for y-axis
#' @param digits digits
#' @param nsmall nsmall
#' @param p.adj P-value adjustment method for multiple comparison
#' @param method parametric (1) or non-parametric (2) test
#' @param print.option the options of print results
#' @return print output
#' @keywords roc_comparison
#' @export
#'
data_summary = function(data, x, y, digits=2, nsmall=1, p.adj='bonferroni', method=2, print.option=c(1,2,3)){
  data[[x]] = factor(data[[x]])
  data = data[!is.na(data[[x]]),]
  data = data[!is.na(data[[y]]),]
  lvs = levels(data[[x]])
  f.nsmall = paste0("%.", nsmall, "f")

  if(method == 2){
    d2 = data %>%
      group_by(!!as.name(x)) %>%
      summarise(
        Median = median(!!as.name(y), na.rm=TRUE),
        IQR25 = quantile(!!as.name(y), na.rm=TRUE, prob=0.25),
        IQR75 = quantile(!!as.name(y), na.rm=TRUE, prob=0.75)
      ) %>%
      mutate(value = paste0(sprintf(Median, fmt=f.nsmall), ' (',
                            sprintf(IQR25, fmt=f.nsmall), ', ', sprintf(IQR75, fmt=f.nsmall), ')')) %>%
      select(-Median, -IQR25, -IQR75)
    all.p = kruskal.test(as.formula(paste(y, '~', x)), data=data)$p.value
    a = suppressWarnings(pairwise.wilcox.test(data[[y]], data[[x]], p.adj=p.adj))
    ad = as.data.frame(round(a$p.value, 3))
    ad$var2 = rownames(ad)
    ad = ad %>%
      gather('var1', 'p.value', 1:(length(lvs)-1)) %>%
      filter(!is.na(p.value)) %>%
      mutate(variables = paste0(var1, '---', var2)) %>%
      select(variables, p.value)
    colnames(ad) = c(x, 'p.value')
    ps = ifelse(ad$p.value <0.001, "'***'", ifelse(ad$p.value <0.01, "'**'", "'*'"))
    ad$p.value = case_when(
      ad$p.value < 0.001 ~ '<0.001***',
      ad$p.value < 0.01  ~ paste0(ad$p.value, '**'),
      ad$p.value < 0.05  ~ paste0(ad$p.value, '*'),
      ad$p.value == 1    ~ '1.000',
      TRUE               ~ as.character(ad$p.value)
    )
  }
  if(method == 1){
    d2 = data %>%
      group_by(!!as.name(x)) %>%
      summarise(
        Average = mean(!!as.name(y), na.rm = TRUE),
        SD = sd(!!as.name(y), na.rm = TRUE)
      ) %>%
      mutate(value = paste0(sprintf(Average, fmt=f.nsmall), ' + ', sprintf(SD, fmt=f.nsmall))) %>%
      select(-Average, -SD)
    all.p = oneway.test(as.formula(paste(y, '~', x)), data=data)$p.value
    a = suppressWarnings(pairwise.t.test(data[[y]], data[[x]], p.adj=p.adj))
    ad = as.data.frame(round(a$p.value, 3))
    ad$var2 = rownames(ad)
    ad = ad %>%
      gather('var1', 'p.value', 1:(length(lvs)-1)) %>%
      filter(!is.na(p.value)) %>%
      mutate(variables = paste0(var1, '---', var2)) %>%
      select(variables, p.value)
    colnames(ad) = c(x, 'p.value')
    ps = ifelse(ad$p.value <0.001, "'***'", ifelse(ad$p.value <0.01, "'**'", "'*'"))
    ad$p.value = case_when(
      ad$p.value < 0.001 ~ '<0.001***',
      ad$p.value < 0.01  ~ paste0(ad$p.value, '**'),
      ad$p.value < 0.05  ~ paste0(ad$p.value, '*'),
      ad$p.value == 1    ~ '1.000',
      TRUE               ~ as.character(ad$p.value)
    )
  }
  colnames(d2) = c(x,y)
  if (1 %in% print.option){
    cat('## Difference of "', y, '"', ' according to "', x, '" (ANOVA), p-value = ', round(all.p, 3), sep="")
  }
  if (2 %in% print.option){
    cat('\n\n#', y, 'values according to each level of', x)
    pandoc.table(d2, justify=c('center', 'center'))
  }
  if (3 %in% print.option){
    cat('\n# Pairwise differences of', y)
    if (length(lvs) > 2) {
      cat('\n  (P-value adjustment: ', a$p.adjust.method, ')', sep="")
    }
    pandoc.table(ad, justify=c('center', 'left'))

  }
  cat('\n')
}
