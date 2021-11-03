#' Load R packages
#'
#' This function loads R packages easily.
#'
#' @param pkg R packages to be loaded
#' @return
#' @keywords loadpacks
#' @export
#' @examples loadpacks(plyr, tidyverse)
#'
loadpacks = function(pkg, ...) {
  arg <- deparse(substitute(pkg))
  dots <- substitute(list(...))[-1]
  librarylist = c(arg, sapply(dots, deparse))
  for(i in librarylist){
    if(!i %in% rownames(installed.packages())){
      install.packages(i)
    }
    suppressWarnings(suppressMessages(library(i, character.only=T, quietly=T)))
  }
}

