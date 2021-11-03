#' Add asterisks for presenting significance
#'
#' Add asterisks for presenting significance
#'
#' @param data a dataframe that has p.value
#' @return data.frame object
#' @keywords add_sig
#' @export
#' @examples
#'
add_sig = function(data) {
  d2 = data %>%
    mutate(
      sig = case_when(
        is.na(p.value) | p.value == ''                    ~ '',
        as.numeric(p.value) < 0.001 | grepl("<", p.value) ~ '***',
        as.numeric(p.value) < 0.01                        ~ '**',
        as.numeric(p.value) < 0.05                        ~ '*',
        TRUE                                              ~ ""
      )
    )
  return(d2)
}
