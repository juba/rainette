library(quanteda)
context("cutree functions")

mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(tokens(mini_corpus, remove_punct = TRUE), tolower = TRUE)
dtm <- dfm_remove(dtm, stopwords("en"))
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)
res <- rainette(dtm, k = 3, min_uc_size = 10)

## cutree

test_that("generic cutree still works", {
  hc <- hclust(dist(USArrests))
  expect_length(cutree(hc, h = 250), 50)
})

test_that("cutree_rainette is ok", {
  expect_error(cutree(res, h = 200))
  expect_equal(cutree(res, k = 3), c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 2L,
    2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L,
    2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 1L, 1L))
})

test_that("rainette2_complete_groups", {
  res1 <- rainette(dtm, k = 5, min_uc_size = 2, min_split_members = 2)
  res2 <- rainette(dtm, k = 5, min_uc_size = 3, min_split_members = 2)
  res_double <- rainette2(res1, res2, min_members = 2)

  groups <- cutree(res_double, k = 4)
  expect_equal(sum(is.na(rainette2_complete_groups(dtm, groups))), 0)
})
