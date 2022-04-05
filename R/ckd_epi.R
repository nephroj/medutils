#' Calculate CKD-EPI eGFR
#'
#' Calculate CKD-EPI eGFR
#'
#' @param Cr serum creatine value
#' @param sex sex 
#' @param age age 
#' @param text_male word refered to male
#' @param text_female word refered to female
#' @return integer value
#' @keywords ckd_epi
#' @export
#'
ckd_epi = function(Cr, sex, age, text_male="M", text_female="F") {
  if (!sex %in% c(text_male, text_female)) {
    stop(paste("Sex variable must be within", text_male, "or", text_female))
  }
  
  GFR = 141 * 
    pmin(Cr/ifelse(sex==text_female, 0.7, 0.9), 1)^ifelse(sex==text_female, -0.329, -0.411) * 
    pmax(Cr/ifelse(sex==text_female, 0.7, 0.9), 1)^(-1.209) * 0.993^floor(as.numeric(age)) * 
    ifelse(sex==text_female, 1.018, 1)
  return(GFR)
}