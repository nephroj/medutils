#' New study ID generator
#'
#' New study ID generator
#'
#' @param seed seed number
#' @param prefix prefix for study ID
#' @param max_digit maximum digits
#' @return data.frame
#' @keywords newid_gen
#' @export
#'
newid_gen = function(seed, prefix="", max_digit=7) {
  set.seed(seed)
  max_no = 10^(max_digit)
  new_id = sample(1:max_no, max_no)

  id_table = tibble(new_id = new_id) %>%
    mutate(
      old_id = row_number()
    ) %>%
    select(old_id, new_id)

  if (prefix != "") {
    id_table = id_table %>%
      mutate(
        new_id = str_c(prefix, str_pad(new_id, max_digit, "left", "0"))
      )
  }

  return(id_table)
}
