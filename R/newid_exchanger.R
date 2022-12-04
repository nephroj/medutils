#' Exchange old patient ID to new study ID
#'
#' Exchange old patient ID to new study ID
#'
#' @param data original data
#' @param newid_data dataframe with old ID and new ID
#' @param old_id_col column pointing to old ID
#' @param new_id_col study ID column
#' @return data.frame object
#' @keywords newid_exchanger
#' @export
#'
newid_exchanger = function(data, newid_data, old_id_col, new_id_col = "studyid") {
  newid_data = newid_data %>%
    rename(
      !!old_id_col := old_id,
      !!new_id_col := new_id
    )

  new_data = data %>%
    left_join(newid_data, by=old_id_col) %>%
    select(-!!old_id_col) %>%
    relocate(!!new_id_col)

  return(new_data)
}
