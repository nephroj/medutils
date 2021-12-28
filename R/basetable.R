#' Classify variables to categorical or numerical variables
#'
#' This function classifies variables to three differente types including
#' categorical, non-normal numerical or normal numerical variables.
#'
#' @param dt Dataframe to be used
#' @param myVars Variables to be categorized
#' @param len.max Maximum number of elements to convert to factor
#' @param print.result Print the analyzed result
#' @param p_norm Cut-off p-value classifying normal and non-normal
#' @return lists of three different types
#' @keywords varClassifier
#' @export
#' @examples varClassifier(dt=mtcars, myVars=c("am", "cyl", "mpg"))
#'
varClassifier = function(dt, myVars, len.max = 5, print.result=T, p_norm=0.05){
  facVar = c()
  normVar = c()
  nonnorm = c()
  inapp = c()
  inapp_inf = c()
  NAVar = c()
  colnames_dt = colnames(dt)
  for(i in myVars){
    if(!i %in% colnames_dt){
      NAVar = c(NAVar, i)
    }
    else if(class(dt[[i]])=="numeric" | class(dt[[i]])=="integer"){
      if (Inf %in% dt[[i]] | -Inf %in% dt[[i]]){
        inapp_inf = c(inapp_inf, i)
      }
      else{
        d1 = shapiro.test(dt[[i]])
        if(d1$p.value >p_norm){
          normVar = c(normVar, i)
        }
        else{
          nonnorm = c(nonnorm, i)
        }
      }
    }
    else if(class(dt[[i]])=="factor" |
            (length(unique(dt[[i]])) <= len.max &
             length(unique(dt[[i]])) >= 2)){
      facVar = c(facVar, i)
    }
    else{
      inapp = c(inapp, i)
    }
  }
  final = list('fac' = facVar, 'norm' = normVar, 'nonnorm' = nonnorm, 'notavail' = NAVar)
  if(print.result == T & !(length(inapp)==0 & length(inapp_inf)==0)){
    cat('Not included in columns of the data: (', paste0(NAVar, collapse=', '),')\n', sep = '')
    cat('Variables which has Inf or -Inf values: (', paste0(inapp_inf, collapse=', '),')\n', sep = '')
    cat('Inappropriate variables: (', paste0(inapp, collapse=', '), ')\n\n', sep = '')
  }
  return(final)
}


#' Create a baseline characteristics table
#'
#' This function create a baesline characteristics tables according to
#' the variables included in a specific dataframe.
#'
#' @param data Dataframe to be used
#' @param varlist Vector of variables to be displayed
#' @param grvar Grouping variable
#' @param len.max Maximum number of elements to convert to factor
#' @param y.correct a logical indicating whether to apply continuity correction
#' @param trend a logical whether to do trend test
#' @param gr.name Elements labels of grouping variables
#' @param method Select between parametric, nonparametric or combined testing
#' @param combine Display overall patient summary values as well
#' @param print.result Print varClassifier result
#' @param p_norm Cut-off p-value classifying normal and non-normal
#' @return Dataframe of baseline characteristics table
#' @keywords basetable
#' @export
#' @examples basetable(data=mtcars, varlist=c("cyl", "mpg", "wt", "hp"), grvar="am")
#'
basetable = function(data, varlist, grvar=NULL, len.max=5, y.correct=T, trend=F,
                     gr.name=NULL, method=3, combine=F, print.result=T, p_norm=0.05){

  alldt = data.frame()
  solodt = data.frame()
  errorvec = c()
  digit = function(x, digits, nsmall, trim=T){   # Number formatting function
    if(x <2 & nsmall <2 & digits <3){
      format(x, digits=digits, nsmall=2)
    }
    else{
      format(x, digits=digits, nsmall=nsmall, trim=T)
    }
  }
  digitC = function(x, digits, nsmall, trim=T){
    suppressWarnings(digit(x, digits, nsmall, trim=T))
  }

  ## gr.name (그룹 변수의 elements 이름을 gr.name에 저장)
  data[[grvar]] = factor(data[[grvar]])
  if(is.null(gr.name)){
    gr.name = levels(data[[grvar]])
  }

  ## 변수를 factor 및 continuous variable (normal, non-normal)로 분류해 줌
  vars = varClassifier(data, varlist, len.max=len.max, print.result=print.result, p_norm=p_norm)

  ####### Grouping 없이 표현 #######
  if(is.null(grvar) | combine==T){
    if(method == 1){
      vars$norm = c(vars$norm, vars$nonnorm)
      vars$nonnorm = NULL
    }
    else if(method == 2){
      vars$nonnorm = c(vars$nonnorm, vars$norm)
      vars$norm = NULL
    }

    for(i in varlist){
      #### Normal ####
      if(i %in% vars$notavail){
        next
      }
      else if(i %in% vars$norm){
        d2 = data %>%
          summarise(
            Average = mean(!!as.name(i), na.rm = TRUE),
            SD = sd(!!as.name(i), na.rm = TRUE)
          ) %>%
          mutate(
            var = i,
            value = ifelse(abs(Average) >= 10,
                           paste0(sprintf(Average, fmt='%.1f'), ' + ', sprintf(SD, fmt='%.1f')),
                           paste0(sprintf(Average, fmt='%.2f'), ' + ', sprintf(SD, fmt='%.2f')))
          ) %>%
          select(var, value)
        solodt = bind_rows(solodt, d2)
      }

      #### Non-normal #####
      else if(i %in% vars$nonnorm){
        d2 = data %>%
          summarise(
            Median = median(!!as.name(i), na.rm=TRUE),
            IQR25 = quantile(!!as.name(i), na.rm=TRUE, prob=0.25),
            IQR75 = quantile(!!as.name(i), na.rm=TRUE, prob=0.75)
          ) %>%
          mutate(
            var = i,
            value = ifelse(abs(Median) >= 10,
                           paste0(sprintf(Median, fmt='%.1f'), ' (', sprintf(IQR25, fmt='%.1f'), ', ', sprintf(IQR75, fmt='%.1f'), ')'),
                           paste0(sprintf(Median, fmt='%.2f'), ' (', sprintf(IQR25, fmt='%.2f'), ', ', sprintf(IQR75, fmt='%.2f'), ')'))

          ) %>%
          select(var, value)
        solodt = bind_rows(solodt, d2)
      }

      #### Factor #####
      else if(i %in% vars$fac){
        data[[i]] = factor(data[[i]])
        var = paste0(i, '=', levels(data[[i]]))
        value = paste0(table(data[[i]]), " (", sprintf(table(data[[i]])/NROW(data) * 100, fmt='%.1f'), ")")
        d2 = data.frame(var, value)
        solodt = bind_rows(solodt, d2)
      }
      #### 이상 있는 변수
      else{
        d2 = data.frame(var = i, value = 'error')
        solodt = bind_rows(solodt, d2)
      }
    }
    all = paste0('All patients', ' (N = ', NROW(data), ')')
    colnames(solodt) = c('var', all)
    if(is.null(grvar)){
      return(solodt)
    }
  }

  ####### Grouping하여 표현 #######
  if(!is.null(grvar)){
    ## 그룹변수로 dataframe을 나눴을 때, 그룹이 안 이루어지고 비어있는 level이 있는 경우, error!
    if(sum(table(data[[grvar]]) == 0) != 0){
      cat('There is no data in a group (or groups)\n')
    }
    else if(length(unique(data[[grvar]])) > len.max){
      cat('There are too many levels in grouping variable\n')
    }
    else if(length(gr.name) != length(levels(data[[grvar]]))){
      cat('Reassignment of names of grouping variable is required\n')
    }
    else{
      if(method == 1){
        vars$norm = c(vars$norm, vars$nonnorm)
        vars$nonnorm = NULL
      }
      else if(method == 2){
        vars$nonnorm = c(vars$nonnorm, vars$norm)
        vars$norm = NULL
      }

      # Grouping variable의 이름에 N 수를 같이 넣어줌
      data[[grvar]]=factor(data[[grvar]],
                           labels=paste0(gr.name, ' (n = ', table(data[[grvar]]), ')'))

      for(i in varlist){
        if(i %in% vars$notavail){
          next
        }
        #### Normal #####
        else if(i %in% vars$norm){
          d2 = data[!is.na(data[[grvar]]), ]
          d2 = d2 %>%
            group_by(!!as.name(grvar)) %>%
            summarise(
              Average = mean(!!as.name(i), na.rm = TRUE),
              SD = sd(!!as.name(i), na.rm = TRUE)) %>%
            mutate(
              var = i,
              max_Average = max(abs(Average), na.rm=T),
              Aver_SD = ifelse(max_Average >= 10,
                               paste0(sprintf(Average, fmt='%.1f'), ' + ', sprintf(SD, fmt='%.1f')),
                               paste0(sprintf(Average, fmt='%.2f'), ' + ', sprintf(SD, fmt='%.2f')))
            )
          noNA = ifelse(sum(is.na(d2)) == 0, T, F)
          d2 = d2 %>%
            select(-Average, -SD, -max_Average) %>%
            spread_(grvar, 'Aver_SD')

          if(noNA == F){
            p.value = 100
            test = 'no test'
          }
          else if(length(unique(data[[grvar]])) == 1){
            p.value = 100
            test = 'no test'
          }
          else if(trend==TRUE){
            fit = JonckheereTerpstraTest(data[[i]], ordered(data[[grvar]]))
            p.value = fit$p.value
            test = "Jonckheere"
          }
          else if(length(unique(data[[grvar]])) == 2){
            vareq = leveneTest(as.formula(paste(i, '~', grvar)), data=data)
            if(vareq[["Pr(>F)"]][1] < 0.05){
              fit = t.test(as.formula(paste(i, '~', grvar)), data=data, var.equal=F)
              test = 't(welch)'
            }
            else{
              fit = t.test(as.formula(paste(i, '~', grvar)), data=data, var.equal=T)
              test = 't-test'
            }
            p.value = fit$p.value
          }
          else{
            vareq = leveneTest(as.formula(paste(i, '~', grvar)), data=data)
            if(vareq[["Pr(>F)"]][1] < 0.05){
              fit = oneway.test(as.formula(paste(i, "~", grvar)), data=data, var.equal=F)
              test = 'anova(welch)'
            }
            else{
              fit = oneway.test(as.formula(paste(i, "~", grvar)), data=data, var.equal=T)
              test = 'anova'
            }
            p.value = fit$p.value
          }
          d2[1, 'p.value'] = p.value
          d2[1, 'test'] = test
          alldt = bind_rows(alldt, d2)
        }

        #### Non-normal #####
        else if(i %in% vars$nonnorm){
          d2 = data[!is.na(data[[grvar]]), ]
          d2 = d2 %>%
            group_by(!!as.name(grvar)) %>%
            summarise(
              Median = median(!!as.name(i), na.rm=TRUE),
              IQR25 = quantile(!!as.name(i), na.rm=TRUE, prob=0.25),
              IQR75 = quantile(!!as.name(i), na.rm=TRUE, prob=0.75)
            ) %>%
            mutate(
              var = i,
              max_Median = max(abs(Median), na.rm=T),
              Med_IQR = ifelse(max_Median >= 10,
                               paste0(sprintf(Median, fmt='%.1f'), ' (', sprintf(IQR25, fmt='%.1f'), ', ', sprintf(IQR75, fmt='%.1f'), ')'),
                               paste0(sprintf(Median, fmt='%.2f'), ' (', sprintf(IQR25, fmt='%.2f'), ', ', sprintf(IQR75, fmt='%.2f'), ')'))
            )
          noNA = ifelse(sum(is.na(d2)) == 0, T, F)
          d2 = d2%>%
            select(-Median, -IQR25, -IQR75, -max_Median) %>%
            spread_(grvar, 'Med_IQR')

          if(noNA == F){
            p.value = 100
            test = 'no test'
          }
          else if(length(unique(data[[grvar]])) == 1){
            p.value = 100
            test = 'no test'
          }
          else if(trend==TRUE){
            fit =  JonckheereTerpstraTest(data[[i]], ordered(data[[grvar]]))
            p.value = fit$p.value
            test = "Jonckheere"
          }
          else if(min(table(data[[grvar]])) >50){
            if(length(unique(data[[grvar]])) == 2){
              vareq = leveneTest(as.formula(paste(i, '~', grvar)), data=data)
              if(vareq[["Pr(>F)"]][1] < 0.05){
                fit = t.test(as.formula(paste(i, '~', grvar)), data=data, var.equal=F)
                test = 't(welch)'
              }
              else{
                fit = t.test(as.formula(paste(i, '~', grvar)), data=data, var.equal=T)
                test = 't-test'
              }
              p.value = fit$p.value
            }
            else{
              vareq = leveneTest(as.formula(paste(i, '~', grvar)), data=data)
              if(vareq[["Pr(>F)"]][1] < 0.05){
                fit = oneway.test(as.formula(paste(i, "~", grvar)), data=data, var.equal=F)
                test = 'anova(welch)'
              }
              else{
                fit = oneway.test(as.formula(paste(i, "~", grvar)), data=data, var.equal=T)
                test = 'anova'
              }
              p.value = fit$p.value
            }
          }
          else{
            if(length(unique(data[[grvar]])) == 2){
              fit = suppressWarnings(wilcox.test(as.formula(paste(i, '~', grvar)), data=data))
              p.value = fit$p.value
              test = 'wilcox'
            }
            else{
              fit = suppressWarnings(kruskal.test(as.formula(paste(i, "~", grvar)), data=data))
              p.value = fit$p.value
              test = 'kruskal'
            }
          }
          d2[1, 'p.value'] = p.value
          d2[1, 'test'] = test
          alldt = bind_rows(alldt, d2)
        }

        #### Factor #####
        else if(i %in% vars$fac){
          data[[i]] = factor(data[[i]])
          tab = table(data[[i]], data[[grvar]])
          #prop.tab = prop.table(tab, margin=2) * 100
          div = as.vector(table(data[[grvar]]))
          prop.tab = matrix(as.vector(t(tab)) / div, ncol=length(levels(data[[grvar]])), byrow = T) * 100
          m = matrix(paste0(tab, ' (', sprintf(prop.tab, fmt='%.1f'), ')'),
                     nrow=dim(tab)[1])
          tabdt_table = as.data.frame(m)                               # table data
          colnames(tabdt_table)= colnames(tab)
          tabdt_var = data.frame(var = paste0(i, "=", rownames(tab)))  # variable name
          tabdt = tabdt_var %>% bind_cols(tabdt_table)                 # variable name + table data
          prop = sum(tab < 5) / length(tab)
          if(trend==TRUE){
            fit = CochranArmitageTest(tab)
            p.value = fit$p.value
            test = "Cochran"
          }
          else if(prop <= 0.2){
            fit = suppressWarnings(chisq.test(tab, correct = y.correct))
            p.value = fit$p.value
            test = 'chisq'
          }
          else{
            fit = suppressWarnings(fisher.test(tab, workspace=2e+07, hybrid=TRUE))
            p.value = fit$p.value
            test = 'fisher'
          }
          tabdt[1, 'p.value'] = p.value
          tabdt[1, 'test'] = test
          alldt = bind_rows(alldt, tabdt)
        }

        #### 이상 있는 변수
        else{
          nc = length(levels(data[[grvar]])) + 3
          dt = data.frame(matrix(c(i, "error", "error", 100, "error"), ncol=nc), stringsAsFactors=F)
          colnames(dt) = c('var', levels(data[[grvar]]), 'p.value', 'test')
          dt$p.value = as.numeric(dt$p.value)
          alldt = bind_rows(alldt, dt)
        }
      }
      alldt = alldt %>%
        mutate(
          p.value = ifelse(is.na(p.value), '',
                           ifelse(p.value == 100, 'not tested',
                                  ifelse(p.value < 0.001, '<0.001',
                                         sprintf(p.value, fmt='%.3f')))),
          test = ifelse(is.na(test), '', test)
        )
      if(combine == T){
        alldt = left_join(solodt, alldt, by='var')
      }
      return(alldt)
    }
  }
}
