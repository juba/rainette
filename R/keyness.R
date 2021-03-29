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
#' library(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' dtm <- dfm(corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
#' dtm <- dfm_trim(dtm, min_termfreq = 3)
#' res <- rainette(dtm, k = 3)
#' groups <- cutree_rainette(res, k = 3)
#' rainette_stats(groups, dtm)
#' }

rainette_stats <- function(
  groups, dtm,
  measure = c("chi2", "lr"),
  n_terms = 15,
  show_negative = TRUE,
  max_p = 0.05) {

  measure <- match.arg(measure)
  stat_col <- stat_col(measure)

  groups_list <- sort(unique(groups))
  groups_list <- groups_list[!is.na(groups_list)]
  tabs <- purrr::map(groups_list, function(group) {
    select <- (groups == group & !is.na(groups))
    tab <- quanteda.textstats::textstat_keyness(dtm, select, measure = measure) %>%
      as_tibble() %>%
      arrange(desc(abs(!!stat_col)))
    if (show_negative) {
      tab %>%
        filter(p <= max_p) %>%
        slice(1:n_terms) %>%
        mutate(sign = if_else(!!stat_col > 0, "positive", "negative"),
          sign = factor(sign, levels = c("positive", "negative")))
    } else {
      tab %>%
        filter(!!stat_col > 0, p <= max_p) %>%
        slice(1:n_terms) %>%
        mutate(sign = "positive")
    }
  })

  tabs

}
