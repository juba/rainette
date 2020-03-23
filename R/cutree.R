#' Cut a tree into groups
#'
#' @param tree the hclust tree object to be cut
#' @param ... arguments passed to other methods
#'
#' @return
#' A vector with group membership.
#'
#' @details
#' If `tree` is of class `rainette`, invokes [cutree_rainette()]. Otherwise, just run [stats::cutree()].
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
#' @return
#' A vector with group membership.
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
#' @return
#' A vector with group membership.
#'
#' @seealso [rainette2_complete_groups()]
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


#' Complete groups membership with knn classification
#' 
#' Starting with groups membership computed from a `rainette2` clustering, 
#' every document not assigned to a cluster is reassigned using a k-nearest
#' neighbour classification.
#' 
#' @param dfm dfm object used for `rainette2` clustering.
#' @param groups group membership computed by `cutree` on `rainette2` result.
#' @param k number of neighbours considered.
#' @param ... other arguments passed to `FNN::knn`.
#' 
#' @return
#' Completed group membership vector.
#' 
#' @seealso [cutree_rainette2()], [FNN::knn()]
#' 
#' @export

rainette2_complete_groups <- function(dfm, groups, k = 1, ...) {
  
  if (!requireNamespace("FNN", quietly = TRUE)) {
    stop("Package \"FNN\" needed for this function to work. Please install it.",
      call. = FALSE)
  }
  
  m <- quanteda::convert(dfm, to = "matrix")
  
  test <- m[is.na(groups), ]
  train <- m[!is.na(groups), ]
  train_groups <- groups[!is.na(groups)]
  
  new_groups <- FNN::knn(train, test, train_groups, k = k, ...)
  groups[is.na(groups)] <- new_groups
  
  groups
  
}

