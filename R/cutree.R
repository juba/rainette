#' @export

cutree <- function(tree, ...) {
  if (inherits(tree, "rainette")) {
    return(cutree.rainette(tree, ...))
  }
  stats::cutree(tree, ...)
}


#' @export

cutree.rainette <- function(hres, k = NULL, h = NULL) {
  if (!is.null(h)) {
    stop("cutree.rainette only works with k argument")
  }
  hres$uce_groups[[k-1]]
}

