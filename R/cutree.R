#' Cut a tree into groups
#'
#' @param tree the hclust tree object to be cut
#' @param ... arguments passed to other methods
#'
#' @details
#' If `tree` is of class `rainette`, invokes [cutree.rainette()]. Otherwise, just run [stats::cutree()].
#'
#' @export

cutree <- function(tree, ...) {
  if (inherits(tree, "rainette")) {
    return(cutree_rainette(tree, ...))
  }
  if (inherits(tree, "rainette2")) {
    return(cutree_rainette2(tree, ...))
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
    stop("cutree_rainette only works with k argument")
  }
  hres$uce_groups[[k-1]]
}

#' Cut a rainette2 result object into groups of documents
#' 
#' @param res the `rainette2` result object to be cut
#' @param k the desired number of groups
#' @param criterion criterion to use to choose the best partition. `chi2` means 
#'    the partition with the maximum sum of chi2, `n` the partition with the 
#'    maximum size.
#' @param ... arguments passed to other methods
#'
#' @export

cutree_rainette2 <- function(res, k, criterion = c("chi2", "n"), ...) {
  criterion <- match.arg(criterion)
  line <- res %>% filter(k == !!k)
  if (criterion == "chi2") {
    line <- line %>% 
      filter(chi2 == max(chi2))
  }
  if (criterion == "n") {
    line <- line %>% 
      filter(n == max(n))
  }
  line %>% pull(groups) %>% unlist
}
