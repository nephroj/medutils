#' Read CSV file with EUC-KR encoding
#'
#' Read CSV file with EUC-KR encoding
#'
#' @param file CSV file
#' @param ... parameters of read_csv
#' @return data.frame object
#' @keywords read_csvk
#' @export
#'
read_csvk = function(file, ...) {
  file_with_encoding = readr::read_csv(file, locale=locale('ko', encoding='euc-kr'), ...)
  return(file_with_encoding)
}
