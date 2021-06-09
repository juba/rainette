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
#' @param doc_id character name of a dtm docvar which identifies source documents.
#'
#' @details
#' If `min_uc_size == 0`, different uc ids are added to the dtm docvars
#' (ie no uce are merged together). If `min_uc_size > 0` then `doc_id` must be provided
#' unless the corpus comes from `split_segments`, in this case
#' `segment_source` is used by default.
#'
#' @return the original dtm with a new `rainette_uc_id` docvar.
#' @export

compute_uc <- function(dtm, min_uc_size = 10, doc_id = NULL) {

  ## Add id to documents
  quanteda::docvars(dtm, field = "rainette_uce_id") <- seq_len(nrow(dtm))

  if (min_uc_size <= 1) {
    ## Do nothing
    quanteda::docvars(dtm, field = "rainette_uc_id") <- quanteda::docvars(dtm, "rainette_uce_id")
    return(dtm)
  }

  ## Check for min_uc_size and doc_id values
  if (is.null(doc_id)) {
    if ("segment_source" %in% names(docvars(dtm))) doc_id <- "segment_source"
    else stop("If min_uc_size > 0, you must provide a doc_id value.")
  }

  ## Size of each uce
  terms_by_uce <- rowSums(dtm)
  doc_ids <- quanteda::docvars(dtm, doc_id)
  if (any(terms_by_uce < min_uc_size)) {
    uc_size_warning <- FALSE
    index <- 1
    uc_id <- quanteda::docvars(dtm, "rainette_uce_id")
    while (index < length(terms_by_uce)) {
      current_size <- terms_by_uce[index]
      grouping_index <- index
      ## While current uc size is smaller than min, regroup with following uce
      while (current_size < min_uc_size) {
        if ((grouping_index + 1) <= length(terms_by_uce) && 
              doc_ids[grouping_index] == doc_ids[grouping_index + 1]) {
          grouping_index <- grouping_index + 1
          current_size <- current_size + terms_by_uce[grouping_index]
          uc_id[grouping_index] <- index
        } else {
          ## If new index is out of bounds or in another document
          uc_size_warning <- TRUE
          break
        }
      }
      index <- grouping_index + 1
    }
    if (uc_size_warning) {
      warning("some uc will have a size < min_uc_size")
    }
    ## Add computed uc ids to docvars
    quanteda::docvars(dtm, "rainette_uc_id") <- uc_id
  } else {
    quanteda::docvars(dtm, "rainette_uc_id") <- quanteda::docvars(dtm, "rainette_uce_id")
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
