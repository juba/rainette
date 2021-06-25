library(testthat)
library(quanteda)
context("stats functions")

mini_corpus <- head(data_corpus_inaugural, n = 10)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(tokens(mini_corpus, remove_punct = TRUE), tolower = TRUE)
dtm <- dfm_remove(dtm, stopwords("en"))
dtm <- dfm_trim(dtm, min_termfreq = 3)

res <- rainette(dtm, k = 5, min_segment_size = 5, min_split_members = 3)

test_that("rainette_stats", {

  ## chi2
  groups <- cutree_rainette(res, k = 3)
  stats <- rainette_stats(groups, dtm, n_terms = 10)
  expect_equal(length(stats), max(groups))
  n_stats_pos <- sum(stats[[2]]$sign == "positive")
  expect_equivalent(
    stats[[2]][stats[[2]]$sign == "positive", -ncol(stats[[2]])],
    head(quanteda.textstats::textstat_keyness(dtm, groups == 2), n_stats_pos)
  )
  n_stats_neg <- sum(stats[[3]]$sign == "negative")
  expect_equivalent(
    stats[[3]][stats[[3]]$sign == "negative",  -ncol(stats[[3]])] %>%
      dplyr::arrange(desc(chi2)),
    tail(quanteda.textstats::textstat_keyness(dtm, groups == 3), n_stats_neg)
  )

  ## lr
  groups <- cutree_rainette(res, k = 4)
  stats <- rainette_stats(groups, dtm, measure = "lr", n_terms = 8)
  expect_equal(length(stats), max(groups))
  n_stats_pos <- sum(stats[[1]]$sign == "positive")
  expect_equivalent(
    stats[[1]][stats[[1]]$sign == "positive", -ncol(stats[[1]])],
    head(quanteda.textstats::textstat_keyness(dtm, measure = "lr", groups == 1), n_stats_pos)
  )
  n_stats_neg <- sum(stats[[4]]$sign == "negative")
  expect_equivalent(
    stats[[4]][stats[[4]]$sign == "negative", -ncol(stats[[4]])] %>%
      dplyr::arrange(desc(G2)),
    tail(quanteda.textstats::textstat_keyness(dtm, measure = "lr", groups == 4), n_stats_neg)
  )

  ## max_p
  stats <- rainette_stats(groups, dtm, n_terms = 10, max_p = 1e-09)
  for (i in seq_along(stats)) {
    expect(all(stats[[i]]$p <= 1e-09), "p not less than max_p")
  }

  ## frequency and docprop
  groups <- cutree_rainette(res, k = 3)
  stats <- rainette_stats(groups, dtm, n_terms = 10, measure = "frequency")
  expect_equal(length(stats), max(groups))
  dtm2 <- dfm_subset(dtm, groups == 2)
  expect_equivalent(
    stats[[2]] %>% select(-sign),
    quanteda.textstats::textstat_frequency(dtm2) %>%
      as_tibble() %>%
      mutate(docprop = .data$docfreq / ndoc(dtm2)) %>%
      arrange(desc(frequency)) %>%
      slice(1:10)
  )
  stats <- rainette_stats(groups, dtm, n_terms = 10, measure = "docprop")
  expect_equal(length(stats), max(groups))
  dtm3 <- dfm_subset(dtm, groups == 3)
  expect_equivalent(
    stats[[3]] %>% select(-sign),
    quanteda.textstats::textstat_frequency(dtm3) %>%
      as_tibble() %>%
      mutate(docprop = .data$docfreq / ndoc(dtm3)) %>%
      arrange(desc(docprop)) %>%
      head(10)
  )
})
