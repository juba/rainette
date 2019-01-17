#' Cut a tree into groups
#'
#' @param tree the hclust tree object to be cut
#' @param ... arguments passed to other methods
#'
#' @details
#' If `tree` is of class `rainette`, invokes `cutree.rainette`. Otherwise, just run `stats::cutree`.
#'
#' @export

cutree <- function(tree, ...) {
  if (inherits(tree, "rainette")) {
    return(cutree_rainette(tree, ...))
  }
  stats::cutree(tree, ...)
}


#' Cut a rainette result tree into groups of documents
#' 
#' @param hres the `rainette` result object to be cut
#' @param k the desired number of groups
#' @param h unsupported
#' @param ... arguments passed to other methods
#'
#' @export

cutree_rainette <- function(hres, k = NULL, h = NULL,...) {
  if (!is.null(h)) {
    stop("cutree.rainette only works with k argument")
  }
  hres$uce_groups[[k-1]]
}

