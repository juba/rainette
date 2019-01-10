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

## docs_order_by_ca

indices <- rainette:::docs_order_by_ca(mini_dfm)

test_that("doc_order_by_ca results are ok", {
  expect_equal(order(quanteda::textmodel_ca(mini_dfm)$rowcoord[,1]),
               indices)
})

## split_tab_by_chisq
 
tab <- mini_dfm %>% 
  convert(to = "data.frame")
tab <- tab[, -1] %>% 
  t %>% 
  as.data.frame
res_split <- rainette:::split_tab_by_chisq(tab, indices)

test_that("split_tab_by_chisq results are ok", {
  manual_res <- data.frame(index = -1, chisq = -1)
  for (i in 2:5) {
    tab1 <- tab[, indices[1:i], drop = FALSE]
    tab2 <- tab[, indices[(i + 1):7, drop = FALSE]]
    expect_warning(chisq <- chisq.test(cbind(rowSums(tab1), rowSums(tab2)))$statistic)
    manual_res <- rbind(manual_res, c(index = indices[i], chisq = chisq))
  }
  max <- which.max(manual_res$chisq)
  expect_equal(res_split$max_index, manual_res$index[max])
  expect_equal(res_split$max_chisq, manual_res$chisq[max])
})

## switch_docs_by_chisq

test_that("switch_docs_by_chisq results are ok", {
  ## Wrong max_index et max_chisq
  max_chisq <- 4.5
  max_index <- 1
  res_switch <- rainette:::switch_docs_by_chisq(tab, indices, max_index, max_chisq)
  expect_equal(res_switch$indices1, c(3, 6))
  expect_equal(res_switch$indices2, c(7, 4, 5, 2, 1))
  expect_equal(res_switch$chisq, res_split$max_chisq)
})

## features_selection

test_that("features_selection is ok", {
  res_switch <- rainette:::switch_docs_by_chisq(tab, indices, res_split$max_index, res_split$max_chisq)
  indices1 <- res_switch$indices1
  indices2 <- res_switch$indices2
  res <- rainette:::features_selection(tab, indices1, indices2)
  expect_equal(res$cols1, character(0))
  expect_equal(res$cols2, c("feat2", "feat3", "feat4"))
  indices1 <- c(4, 3, 5, 6, 2)
  indices2 <- c(7, 1)
  res <- rainette:::features_selection(tab, indices1, indices2)
  expect_equal(res$cols1, c("feat1", "feat3", "feat4"))
  expect_equal(res$cols2, character(0))
})

## rainette

test_that("rainette on mini_dfm is ok", {
  res <- rainette(mini_dfm, k = 2, min_uc_size = 1, min_members = 1)
  expect_is(res, c("rainette", "hclust"))
  expect_equal(res$group, c(2, 2, 1, 2, 2, 1, 2))
  expect_equal(which(res$group == 1), indices[1:which(indices == res_split$max_index)])
  expect_equal(res$height, res_split$max_chisq)
})

test_that("rainette on mini_dfm with verbose is ok", {
  res <- rainette(mini_dfm, k = 2, min_uc_size = 1, min_members = 1, verbose = TRUE)
  expect_equal(dim(res$corresp_uce_uc), c(7, 2))
  expect_is(res$res, "list")
})

mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)

test_that("warning if tab too small", {
  expect_warning(rainette(dtm, k = 8, min_uc_size = 10, min_members = 1))
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


## Special cases

test_that("dfm with 'document' feature is ok", {
  m_document <- m
  colnames(m_document) <- c("document", "éé", "ùù", "feat4", "feat5")
  dfm_document <- as.dfm(m_document)
  res_document <- rainette(dfm_document, k = 2, min_uc_size = 1, min_members = 1)
  res <- rainette(mini_dfm, k = 2, min_uc_size = 1, min_members = 1)
  expect_equal(res$group, res_document$group)
  expect_equal(res$height, res_document$height)  
})









