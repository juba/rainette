library(quanteda)
context("cutree functions")

mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)
res <- rainette(dtm, k = 3, min_uc_size = 15)

## cutree

test_that("generic cutree still works", {
  hc <- hclust(dist(USArrests))
  expect_length(cutree(hc, h = 250), 50)
})

test_that("h argument generates error in cutree.rainette", {
  expect_error(cutree(res, h = 200))
  expect_equal(cutree(res, k = 3), c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 
    2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 
    3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L))
})