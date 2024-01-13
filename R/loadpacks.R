#' Load R packages
#'
#' This function loads R packages easily.
#'
#' @param pkg R packages to be loaded
#' @param ... ...
#' @keywords loadpacks
#' @export
#'
loadpacks = function(pkg, ...) {
  arg <- deparse(substitute(pkg))
  dots <- substitute(list(...))[-1]
  librarylist = c(arg, sapply(dots, deparse))
  for(i in librarylist){
    if(!i %in% rownames(installed.packages())){
      install.packages(i)
    }
    suppressWarnings(suppressMessages(requireNamespace(i, character.only=T, quietly=T)))
  }
}

