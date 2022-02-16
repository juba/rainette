#' Generate cluster keyness statistics from a rainette result
#'
#' @param groups groups membership computed by `cutree_rainette` or `cutree_rainette2`
#' @param dtm the dfm object used to compute the clustering
#' @param measure statistics to compute
#' @param n_terms number of terms to display in keyness plots
#' @param show_negative if TRUE, show negative keyness features
#' @param max_p maximum keyness statistic p-value
#'
#' @seealso [quanteda.textstats::textstat_keyness()], [rainette_explor()], [rainette_plot()]
#'
#' @return
#' A list with, for each group, a data.frame of keyness statistics for the most specific
#' n_terms features.
#'
#' @export
#' @examples
#' \donttest{
#' require(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' tok <- tokens(corpus, remove_punct = TRUE)
#' tok <- tokens_remove(tok, stopwords("en"))
#' dtm <- dfm(tok, tolower = TRUE)
#' dtm <- dfm_trim(dtm, min_docfreq = 3)
#' res <- rainette(dtm, k = 3, min_segment_size = 15)
#' groups <- cutree_rainette(res, k = 3)
#' rainette_stats(groups, dtm)
#' }

rainette_stats <- function(
  groups, dtm,
  measure = c("chi2", "lr", "frequency", "docprop"),
  n_terms = 15,
  show_negative = TRUE,
  max_p = 0.05) {

  measure <- match.arg(measure)
  stat_col <- stat_col(measure)

  groups_list <- sort(unique(groups))
  groups_list <- groups_list[!is.na(groups_list)]
  tabs <- purrr::map(groups_list, function(group) {
    select <- (groups == group & !is.na(groups))

    empty_tab <- tibble(
      feature = character(0),
      col = double(0),
      sign = character(0)
    )
    names(empty_tab) <- c("feature", stat_col(measure), "sign")

    ## Keyness
    if (measure %in% c("chi2", "lr")) {
      tab <- quanteda.textstats::textstat_keyness(dtm, select, measure = measure) %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(desc(abs(.data[[stat_col]])))
      if (all(is.nan(tab$p))) return(empty_tab)
      if (show_negative) {
        tab <- tab %>%
          dplyr::filter(p <= max_p) %>%
          dplyr::slice(1:.env$n_terms) %>%
          dplyr::mutate(
            sign = ifelse(.data[[stat_col]] > 0, "positive", "negative"),
            sign = factor(sign, levels = c("positive", "negative"))
          )
      } else {
        tab <- tab %>%
          dplyr::filter(.data[[stat_col]] > 0, p <= max_p) %>%
          dplyr::slice(1:.env$n_terms) %>%
          dplyr::mutate(sign = "positive")
      }
    }
    if (measure %in% c("frequency", "docprop")) {
      tmp_dtm <- quanteda::dfm_subset(dtm, select)
      if (all(colSums(tmp_dtm) == 0)) return(empty_tab)
      tab <- quanteda.textstats::textstat_frequency(tmp_dtm) %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(docprop = .data$docfreq / ndoc(tmp_dtm)) %>%
          dplyr::arrange(desc(.data[[stat_col]])) %>%
          dplyr::slice(1:.env$n_terms) %>%
          dplyr::mutate(sign = "positive")
    }
    return(tab)
  })

  tabs

}
