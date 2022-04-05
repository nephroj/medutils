#' Calculate CKD-EPI eGFR
#'
#' Calculate CKD-EPI eGFR
#'
#' @param Cr serum creatine value
#' @param sex sex
#' @param age age
#' @param factor_male word refered to male
#' @param factor_female word refered to female
#' @return integer value
#' @keywords ckd_epi
#' @export
#'
ckd_epi = function (Cr, sex, age, factor_male, factor_female) {

  score_sex1 = ifelse(sex==factor_female, 0.7, ifelse(sex==factor_male, 0.9, NA))
  score_sex2 = ifelse(sex==factor_female, -0.329, ifelse(sex==factor_male, -0.411, NA))
  score_sex3 = ifelse(sex==factor_female, 1.018, ifelse(sex==factor_male, 1, NA))

  GFR = 141 *
    pmin(Cr/score_sex1, 1)^score_sex2 *
    pmax(Cr/score_sex1, 1)^(-1.209) * 0.993^floor(as.numeric(age)) *
    score_sex3
  return(GFR)
}
