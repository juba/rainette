if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("Dim1", "distance", "index", "segment", "segment_source", "weight", "p", 
    "chi2", "n_both", "n1", "n2", "g1", "g2", "level1", "level2", "members", "clusters", "feature",
    "Group", "rainette_group", "text"))


#' Merges uces into uc according to minimum uc size
#' 
#' `rainette_uc_index` docvar
#' 
#' @param dtm dtm of uces, with a `rainette_uce_id` docvar
#' @param min_uc_size minimum number of forms by uc
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return the original dtm with a new `rainette_uc_id` docvar.

compute_uc <- function(dtm, min_uc_size = 10) {
  
  ## Add id to documents
  if (nrow(docvars(dtm)) == 0 && utils::packageVersion("quanteda") < "2.0.0") {
    dtm@docvars <- data.frame(rainette_uce_id = 1:nrow(dtm))
  } else {
    docvars(dtm, field = "rainette_uce_id") <- 1:nrow(dtm)
  }
  
  if (min_uc_size <= 1) {
    docvars(dtm, field = "rainette_uc_id") <- docvars(dtm, "rainette_uce_id")
    return(dtm)
  }
  
  ## Size of each uce
  terms_by_uce <- rowSums(dtm)
  if (any(terms_by_uce < min_uc_size)) {
    index <- 1
    uc_id <- docvars(dtm, "rainette_uce_id")
    while (index < length(terms_by_uce)) {
      current_size <- terms_by_uce[index]
      grouping_index <- index
      ## While current uc size is smaller than min, regroup with following uce
      while(current_size < min_uc_size) {
        grouping_index <- grouping_index + 1
        if (grouping_index > length(terms_by_uce)) {
          stop("can't compute uc with respect to min_uc_size")
        }
        current_size <- current_size + terms_by_uce[grouping_index]
        uc_id[grouping_index] <- index
      }
      index <- grouping_index + 1
    }
    ## Add computed uc ids to docvars
    docvars(dtm, "rainette_uc_id") <- uc_id
  } else {
    docvars(dtm, "rainette_uc_id") <- docvars(dtm, "rainette_uce_id")
  }
  
  return(dtm)
}


#' @importFrom rlang sym

stat_col <- function(measure) {

    stat_col <- switch(measure,
    "chi2" = "chi2",
    "lr" = "G2"
  )
  stat_col <- rlang::sym(stat_col)
  
}


