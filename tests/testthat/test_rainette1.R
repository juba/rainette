library(quanteda)
context("simple reinert classification")

m <- matrix(
  c(
    1L, 1L, 1L, 0L, 0L,
    0L, 0L, 1L, 1L, 0L,
    1L, 0L, 0L, 0L, 0L,
    1L, 1L, 1L, 1L, 0L,
    1L, 1L, 1L, 1L, 1L,
    1L, 0L, 0L, 0L, 0L,
    1L, 1L, 1L, 0L, 0L
  ),
  ncol = 5,
  byrow = TRUE,
)
mini_dfm <- as.dfm(m)


## docs_order_by_ca

indices <- rainette:::order_docs(mini_dfm)

test_that("order_docs", {
  skip_if_not_installed("quanteda.textmodels")
  if (requireNamespace("quanteda.textmodels", quietly = TRUE)) {
    ca_values <- quanteda.textmodels::textmodel_ca(mini_dfm)$rowcoord[, 1]
    expect_equal(order(ca_values), indices)
  }
})

## split_tab_by_chisq

tab <- convert(mini_dfm, to = "matrix")
storage.mode(tab) <- "integer"
res_split <- rainette:::cpp_split_tab(m, indices)

test_that("split_tab", {
  manual_res <- data.frame(index = -1, chisq = -1)
  for (i in 1:6) {
    tab1 <- m[indices[1:i], , drop = FALSE]
    tab2 <- m[indices[(i + 1):7], , drop = FALSE]
    expect_warning(chisq <- chisq.test(rbind(colSums(tab1), colSums(tab2)))$statistic)
    manual_res <- rbind(manual_res, c(index = indices[i], chisq = chisq))
  }
  max <- which.max(manual_res$chisq)
  expect_equal(res_split$max_index, manual_res$index[max])
  expect_equal(res_split$max_chisq, manual_res$chisq[max], tolerance = 1e-06)
})

## switch_docs

test_that("switch_docs", {
  ## Wrong max_index et max_chisq
  max_chisq <- 4.5
  max_index <- 1
  res_switch <- rainette:::switch_docs(tab, indices, max_index, max_chisq)
  expect_equal(res_switch$indices1, c(3, 6))
  expect_equal(res_switch$indices2, c(7, 4, 5, 2, 1))
  expect_equal(res_switch$chisq, res_split$max_chisq)
})

## select_features

test_that("select_features", {
  res_switch <- rainette:::switch_docs(tab, indices, res_split$max_index, res_split$max_chisq)
  indices1 <- res_switch$indices1
  indices2 <- res_switch$indices2
  res <- rainette:::select_features(tab, indices1, indices2)
  expect_equal(res$cols1, character(0))
  expect_equal(res$cols2, c("feat2", "feat3", "feat4"))
  indices1 <- c(4, 3, 5, 6, 2)
  indices2 <- c(7, 1)
  res <- rainette:::select_features(tab, indices1, indices2)
  expect_equal(res$cols1, c("feat1", "feat3", "feat4"))
  expect_equal(res$cols2, character(0))
})

## rainette

test_that("rainette on mini_dfm", {
  res <- rainette(mini_dfm, k = 2, min_segment_size = 1, min_split_members = 3)
  expect_is(res, c("rainette", "hclust"))
  expect_equal(res$group, c(2, 2, 1, 2, 2, 1, 2))
  expect_equal(which(res$group == 1), indices[1:which(indices == res_split$max_index)])
  expect_equal(res$height, res_split$max_chisq)
})


mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(tokens(mini_corpus, remove_punct = TRUE), tolower = TRUE)
dtm <- dfm_remove(dtm, stopwords("en"))
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)

test_that("Stopping if tab too small", {
  expect_message(
    res <- rainette(dtm, k = 12, min_segment_size = 10, min_split_members = 3),
    "! Tab to be splitted is not big enough. Stopping after iteration"
  )
  expect_equal(max(res$group), 9)
})

test_that("Stopping if no more group > min_split_members", {
  expect_message(
    res <- rainette(dtm, k = 3, min_segment_size = 10, min_split_members = 12),
    "^! No more group bigger than min_split_members. Stopping after iteration"
  )
  expect_equal(max(res$group), 2)
  expect_message(
    res <- rainette(dtm, k = 3, min_segment_size = 10, min_split_members = 15),
    "! No computed clusters. Returning NULL."
  )
  expect_null(res)
})

test_that("all uce belong to the same group", {
  res <- rainette(dtm, k = 3, min_segment_size = 5)
  dtm_uc <- rainette:::merge_segments(dtm, min_segment_size = 5)
  expect_equal(anyDuplicated(unique(cbind(docvars(dtm_uc, "rainette_uc_id"), res$group))), 0)
  expect_equal(anyDuplicated(unique(cbind(docvars(dtm_uc, "rainette_uce_id"), res$group))), 0)
})

corpus <- head(data_corpus_inaugural, n = 10)
corpus <- split_segments(corpus)
dtm <- dfm(tokens(corpus, remove_punct = TRUE), tolower = TRUE)
dtm <- dfm_remove(dtm, stopwords("en"))
dtm <- dfm_trim(dtm, min_termfreq = 3)

test_that("groups correspondance is ok", {
  res <- rainette(dtm, k = 6, min_segment_size = 5)
  df <- data.frame(res$uce_groups)
  tab <- table(df[, 4], df[, 5])
  expect_equal(sum(tab > 0), 6)
  tab <- table(df[, 3], df[, 4])
  expect_equal(sum(tab > 0), 5)
})


## Special cases

test_that("dfm with 'document' feature is ok", {
  m_document <- m
  colnames(m_document) <- c("document", "éé", "ùù", "feat4", "feat5")
  dfm_document <- as.dfm(m_document)
  res_document <- rainette(dfm_document, k = 2, min_segment_size = 1, min_split_members = 3)
  res <- rainette(mini_dfm, k = 2, min_segment_size = 1, min_split_members = 3)
  expect_equal(res$group, res_document$group)
  expect_equal(res$height, res_document$height)
})


## min_members deprecation

test_that("min_members is deprecated", {
  expect_warning(
    res_deprecated <- rainette(mini_dfm, k = 2, min_segment_size = 1, min_members = 3),
    "min_members is deprecated. Use min_split_members instead."
  )
  res_deprecated$call <- NULL
  res <- rainette(mini_dfm, k = 2, min_segment_size = 1, min_split_members = 3)
  res$call <- NULL
  expect_equal(
    res,
    res_deprecated
  )
})
