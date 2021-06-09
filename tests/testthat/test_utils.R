library(quanteda)
context("utils functions")

m <- matrix(
  c(1, 1, 1, 0, 0,
    0, 0, 1, 1, 0,
    1, 0, 0, 0, 0,
    1, 1, 1, 1, 0,
    1, 1, 1, 1, 1,
    1, 0, 0, 0, 0,
    1, 1, 1, 0, 0
    ),
  ncol = 5,
  byrow = TRUE,
)
mini_dfm <- as.dfm(m)
docvars(mini_dfm, "rainette_uce_id") <- seq_len(nrow(mini_dfm))
docvars(mini_dfm, "doc_id") <- "doc1"


# compute_uc

test_that("computed uc are ok when min_uc_size == 0", {
  dfm_uc <- rainette::compute_uc(mini_dfm, min_uc_size = 0)
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 3, 4, 5, 6, 7))
})

test_that("error when min_uc_size > 0 and no doc_id", {
  expect_error(compute_uc(mini_dfm, min_uc_size = 3))
})

test_that("computed uc are ok when min_uc_size > 0", {
  dfm_uc <- rainette::compute_uc(mini_dfm, min_uc_size = 3, doc_id = "doc_id")
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 2, 4, 5, 6, 6))
})

test_that("warning if uc can't be computed", {
  expect_warning(compute_uc(mini_dfm, min_uc_size = 5, doc_id = "doc_id"))
})

mini_dfm_docs <- mini_dfm
test_that("computed uc are ok with respect to doc_id", {
  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc1", "doc1", "doc2", "doc2", "doc2", "doc3")
  expect_warning(dfm_uc <- rainette::compute_uc(mini_dfm_docs, min_uc_size = 3, doc_id = "doc_id"))
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 2, 4, 5, 6, 7))

  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc2", "doc3", "doc4", "doc4", "doc5", "doc5")
  expect_warning(dfm_uc <- rainette::compute_uc(mini_dfm_docs, min_uc_size = 3, doc_id = "doc_id"))
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 3, 4, 5, 6, 6))

  corpus <- head(data_corpus_inaugural, n = 10)
  corpus <- split_segments(corpus)
  dtm <- dfm(tokens(corpus, remove_punct = TRUE), tolower = TRUE)
  dtm <- dfm_remove(dtm, stopwords("en"))
  dtm <- dfm_trim(dtm, min_termfreq = 3)
  expect_warning(dfm_uc <- rainette::compute_uc(dtm, min_uc_size = 500))
  tmp <- docvars(dfm_uc) |>
    group_by(rainette_uc_id) |>
    summarise(nd = n_distinct(segment_source)) |>
    filter(nd > 1)
  expect_equal(nrow(tmp), 0)
})

mini_dfm_segsource <- mini_dfm
docvars(mini_dfm_segsource, "segment_source") <- "doc1"
test_that("segment_source used by default when min_uc_size > 0", {
  dfm_uc <- rainette::compute_uc(mini_dfm_segsource, min_uc_size = 3)
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 2, 4, 5, 6, 6))
})





# stat_col

test_that("stat_col is ok", {
  expect_equal(stat_col("chi2"), rlang::sym("chi2"))
  expect_equal(stat_col("lr"), rlang::sym("G2"))
})
