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
ckd_epi = function (Cr, sex, age, factor_male, factor_female, black = F, version = 2021) {

  if (version == 2021) {

    # 2021 version
    score_sex1 = case_when(
      sex==factor_female ~ 0.7,
      sex==factor_male ~ 0.9,
      TRUE ~ NA_real_
    )
    score_sex2 = case_when(
      sex==factor_female ~ -0.241,
      sex==factor_male ~ -0.302,
      TRUE ~ NA_real_
    )
    score_sex3 = case_when(
      sex==factor_female ~ 1.012,
      sex==factor_male ~ 1,
      TRUE ~ NA_real_
    )

    GFR = 142 *
      pmin(Cr/score_sex1, 1)^score_sex2 *
      pmax(Cr/score_sex1, 1)^(-1.200) * 0.9938^floor(as.numeric(age)) *
      score_sex3

  } else if (version == 2009) {
    # 2009 version
    score_sex1 = case_when(
      sex==factor_female ~ 0.7,
      sex==factor_male ~ 0.9,
      TRUE ~ NA_real_
    )
    score_sex2 = case_when(
      sex==factor_female ~ -0.329,
      sex==factor_male ~ -0.411,
      TRUE ~ NA_real_
    )
    score_sex3 = case_when(
      sex==factor_female ~ 1.018,
      sex==factor_male ~ 1,
      TRUE ~ NA_real_
    )
    score_black = case_when(
      black==T ~ 1.159,
      black==F ~ 1,
      TRUE ~ NA_real_
    )

    GFR = 141 *
      pmin(Cr/score_sex1, 1)^score_sex2 *
      pmax(Cr/score_sex1, 1)^(-1.209) * 0.993^floor(as.numeric(age)) *
      score_sex3 * score_black

  } else {
    GFR = NA_real_
    print("Select a correct vesion.")
  }

  return(GFR)
}
