#' x by x table (n & percent)
#'
#' x by x table (n & percent)
#'
#' @param data a dataframe to be used
#' @param outcome outcome variable
#' @param predictor predictor variable
#' @return data.frame object
#' @keywords x_table
#' @export
#'
x_table = function(data, outcome, predictor) {
  dt = data %>%
    group_by(!!as.name(outcome), !!as.name(predictor)) %>%
    count() %>%
    group_by(!!as.name(outcome))

  dt_for_chi = dt %>%
    spread(!!as.name(predictor), n)
  print(chisq.test(dt_for_chi))

  dt_for_print = dt %>%
    mutate(
      total_n = sum(n),
      percent = n / total_n * 100,
      n_percent = paste0(n, " (", round(percent, 1), ")"),
      !!as.name(predictor) := paste0(predictor, "_", !!as.name(predictor)),
      outcome := paste0(outcome, "_", !!as.name(outcome)),
    ) %>%
    ungroup() %>%
    select(outcome, !!as.name(predictor), n_percent) %>%
    spread(!!as.name(predictor), n_percent)
  return(dt_for_print)
}
