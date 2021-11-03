#' Make words to vector
#'
#' This function makes a vector from words list
#'
#' @param vars words to be a vector
#' @return vector
#' @keywords vec
#' @export
#' @examples vec(a, b, c)
#'
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @import ggplot2
#' @importFrom rlang :=
#' @importFrom readxl read_excel
#' @importFrom cowplot theme_cowplot
#' @importFrom cowplot plot_grid
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom pROC roc
#' @importFrom pROC roc.test
#' @importFrom pROC coords
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom car leveneTest
#' @importFrom PMCMRplus cuzickTest
#' @importFrom DescTools CochranArmitageTest
#'
vec = function(vars, ...){
  arg <- deparse(substitute(vars))
  dots <- substitute(list(...))[-1]
  vars = c(arg, sapply(dots, deparse))
  return(vars)
}

