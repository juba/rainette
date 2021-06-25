if (getRversion() >= "2.15.1")
  utils::globalVariables(c("Dim1", "distance", "index", "segment", "segment_source", "weight", "p",
    "chi2", "n_both", "n1", "n2", "g1", "g2", "level1", "level2", "members", "clusters", "feature",
    "Group", "rainette_group", "text"))


#' Merges uces into uc according to minimum uc size
#'
#' `rainette_uc_index` docvar
#'
#' @param dtm dtm of uces, with a `rainette_uce_id` docvar
#' @param min_segment_size minimum number of forms by uc
#' @param doc_id character name of a dtm docvar which identifies source documents.
#'
#' @details
#' If `min_segment_size == 0`, different uc ids are added to the dtm docvars
#' (ie no uce are merged together). If `min_segment_size > 0` then `doc_id` must be provided
#' unless the corpus comes from `split_segments`, in this case
#' `segment_source` is used by default.
#'
#' @return the original dtm with a new `rainette_uc_id` docvar.
#' @export

merge_segments <- function(dtm, min_segment_size = 10, doc_id = NULL) {

  ## Add id to documents
  quanteda::docvars(dtm, field = "rainette_uce_id") <- seq_len(nrow(dtm))

  if (min_segment_size <= 1) {
    ## Do nothing
    quanteda::docvars(dtm, field = "rainette_uc_id") <- quanteda::docvars(dtm, "rainette_uce_id")
    return(dtm)
  }

  ## Check for min_segment_size and doc_id values
  if (is.null(doc_id)) {
    if ("segment_source" %in% names(docvars(dtm))) doc_id <- "segment_source"
    else stop("If min_segment_size > 0, you must provide a doc_id value.")
  }

  ## Size of each uce
  terms_by_uce <- rowSums(dtm)
  doc_ids <- quanteda::docvars(dtm, doc_id)

  ## If all uce are already above the minimum size
  if (all(terms_by_uce >= min_segment_size)) {
    quanteda::docvars(dtm, "rainette_uc_id") <- quanteda::docvars(dtm, "rainette_uce_id")
    return(dtm)
  }

  ## else
  index <- 1
  uc_id <- quanteda::docvars(dtm, "rainette_uce_id")
  while (index <= length(terms_by_uce)) {
    current_size <- terms_by_uce[index]
    grouping_index <- index
    ## While current uc size is smaller than min, regroup with following uce
    while (current_size < min_segment_size) {
      if (
          (grouping_index + 1) <= length(terms_by_uce) &&
          doc_ids[grouping_index] == doc_ids[grouping_index + 1]
        ) {
          grouping_index <- grouping_index + 1
          current_size <- current_size + terms_by_uce[grouping_index]
          uc_id[grouping_index] <- index
      } else {
        ## If new index is out of bounds or in another document
        ## replace current uc index with the previous one, if any
        current_doc_id <- doc_ids[grouping_index ]
        current_uc_id <- uc_id[grouping_index]
        other_uc_ids <- uc_id[doc_ids == current_doc_id & uc_id < current_uc_id]
        if (length(other_uc_ids) > 0) {
          previous_uc_id <- max(other_uc_ids)
          uc_id[uc_id == current_uc_id] <- previous_uc_id
        }
        break
      }
    }
    index <- grouping_index + 1
  }
  ## Add computed uc ids to docvars
  quanteda::docvars(dtm, "rainette_uc_id") <- uc_id

  ## Test if any uc is below min_segment_size
  dtm_uc_size <- quanteda::dfm_group(dtm, quanteda::docvars(dtm, "rainette_uc_id"))
  if (any(rowSums(dtm_uc_size) < min_segment_size)) {
    warning("some segments will have a size < min_segment_size")
  }

  return(dtm)
}


#' Returns the number of segment of each cluster for each source document
#'
#' @param obj a corpus, tokens or dtm object
#' @param clust_var name of the docvar with the clusters
#' @param doc_id docvar identifying the source document
#' @param prop if TRUE, returns the percentage of each cluster by document
#'
#' @details
#' This function is only useful for previously segmented corpus. If `doc_id` 
#' is NULL and there is a `sement_source` docvar, it will be used instead.
#' 
#' @seealso [docs_by_cluster_table()]
#'
#' @examples
#' \donttest{
#' require(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' tok <- tokens(corpus, remove_punct = TRUE)
#' tok <- tokens_remove(tok, stopwords("en"))
#' dtm <- dfm(tok, tolower = TRUE)
#' dtm <- dfm_trim(dtm, min_docfreq = 2)
#' res <- rainette(dtm, k = 3, min_segment_size = 15)
#' corpus$cluster <- cutree(res, k = 3)
#' clusters_by_doc_table(corpus, clust_var = "cluster", prop = TRUE)
#' }
#' @export

clusters_by_doc_table <- function(obj, clust_var = NULL, doc_id = NULL, prop = FALSE) {

  if (!inherits(obj, "corpus") && !inherits(obj, "dfm") && !inherits(obj, "tokens")) {
    stop("obj must be a corpus, a tokens or a dfm object.")
  }

  if (is.null(doc_id) && "segment_source" %in% names(docvars(obj))) {
    doc_id <- "segment_source"
  }


  ids <- quanteda::docvars(obj, doc_id)
  res <- dplyr::tibble(
    doc_id = factor(ids, levels = unique(ids)),
    cluster = docvars(obj, clust_var)
  )

  names_prefix <- ""
  if (is.numeric(res$cluster)) {
    res$cluster <- as.character(res$cluster)
    names_prefix <- "clust_"
  }

  ## Convert NA to "NA" to keep them if there is not already "NA" values
  if (any(is.na(res$cluster))) {
    if (!("NA" %in% res$cluster)) {
      res$cluster[is.na(res$cluster)] <- "NA"
    } else {
      res$cluster[is.na(res$cluster)] <- "NA_missing_"
    }
  }

  ## Count clusters
  res <- res %>%
    dplyr::count(.data$doc_id, .data$cluster)

  ## Compute percenteages
  if (prop) {
    res <- res %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::mutate(n = n / sum(n) * 100) %>%
      dplyr::ungroup()
  }

  ## Pivoting
  res <- res %>%
    tidyr::pivot_wider(
      id_cols = .data$doc_id,
      names_from = .data$cluster,
      values_from = n,
      names_prefix = names_prefix,
      values_fill = 0
    ) %>%
    dplyr::mutate(doc_id = as.character(.data$doc_id))

  cols <- sort(colnames(res))
  cols <- cols[cols != "doc_id"]
  dplyr::relocate(res, .data$doc_id, cols)
}


#' Returns, for each cluster, the number of source documents with at least n
#' segments of this cluster
#'
#' @param obj a corpus, tokens or dtm object
#' @param clust_var name of the docvar with the clusters
#' @param doc_id docvar identifying the source document
#' @param threshold the minimal number of segments of a given cluster that a document
#'   must include to be counted
#'
#' @details
#' This function is only useful for previously segmented corpus. If `doc_id` is NULL
#' and there is a `sement_source` docvar, it will be used instead.
#'
#' @seealso [clusters_by_doc_table()]
#' 
#' @examples
#' \donttest{
#' require(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' tok <- tokens(corpus, remove_punct = TRUE)
#' tok <- tokens_remove(tok, stopwords("en"))
#' dtm <- dfm(tok, tolower = TRUE)
#' dtm <- dfm_trim(dtm, min_docfreq = 2)
#' res <- rainette(dtm, k = 3, min_segment_size = 15)
#' corpus$cluster <- cutree(res, k = 3)
#' docs_by_cluster_table(corpus, clust_var = "cluster")
#' }
#' @export

docs_by_cluster_table <- function(obj, clust_var = NULL, doc_id = NULL, threshold = 1) {

  count <- clusters_by_doc_table(obj, clust_var = clust_var, doc_id = doc_id, prop = FALSE)
  n_docs <- nrow(count)

  count %>%
    dplyr::select(-.data$doc_id) %>%
    dplyr::mutate(dplyr::across(.fns = function(v) v >= threshold)) %>%
    dplyr::summarise(dplyr::across(.fns = sum)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "cluster", values_to = "n") %>%
    dplyr::mutate(`%` = .data$n / n_docs * 100)

}


#' @importFrom rlang sym

stat_col <- function(measure) {

  stat_col <- switch(measure,
    "chi2" = "chi2",
    "lr" = "G2",
    "frequency" = "frequency",
    "docprop" = "docprop"
  )
  rlang::sym(stat_col)

}
