#' Make words to vector
#'
#' This function makes a vector from words list
#'
#' @param vars words to be a vector
#' @param ... ...
#' @return vector
#' @keywords vec
#' @export
#' @examples vec(a, b, c)
#' @import ggplot2
#' @importFrom readxl read_excel
#' @importFrom rlang :=
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_all
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr summarise_all
#' @importFrom dplyr summarise_at
#' @importFrom dplyr arrange
#' @importFrom dplyr starts_with
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr spread_
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_lower
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom cowplot theme_cowplot
#' @importFrom cowplot plot_grid
#' @importFrom ggbeeswarm geom_beeswarm
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

