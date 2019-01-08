library(quanteda)
context("simple reinert classification")

m <- matrix(
  c(1,1,1,0,0,
    0,0,1,1,0,
    1,0,0,0,0,
    1,1,1,1,0,
    1,1,1,1,1,
    1,0,0,0,0,
    1,1,1,0,0
    ),
  ncol = 5,
  byrow = TRUE,
)
mini_dfm <- as.dfm(m)
docvars(mini_dfm)$rainette_uce_id <- 1:nrow(mini_dfm)

test_that("computed uc are ok", {
  dfm_uc <- compute_uc(mini_dfm, min_uc_size = 3)
  expect_equal(docvars(dfm_uc)$rainette_uc_id, c(1,2,2,4,5,6,6))
})

test_that("error if uc can't be computed", {
  expect_error(compute_uc(mini_dfm, min_uc_size = 5)) 
})

mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)

test_that("warning if tab too small", {
  expect_warning(rainette(dtm, k = 4, min_uc_size = 10, min_members = 1))
})

test_that("warning if no more group > min_members", {
  expect_warning(rainette(dtm, k = 3, min_uc_size = 10, min_members = 15))
})

test_that("all uce belong to the same group", {
  res <- rainette(dtm, k = 3, min_uc_size = 15)
  expect_equal(anyDuplicated(unique(cbind(res$corresp_uce_uc$uc, res$group))), 0)
})

corpus <- head(data_corpus_inaugural, n = 10)
corpus <- split_segments(corpus)
dtm <- dfm(corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)

test_that("groups correspondance is ok", {
  res <- rainette(dtm, k = 6, min_uc_size = 5)
  df <- data.frame(res$uce_groups)
  tab <- table(df[,4], df[,5])
  expect_equal(sum(tab > 0), 6)
  tab <- table(df[,3], df[,4])
  expect_equal(sum(tab > 0), 5)
})



