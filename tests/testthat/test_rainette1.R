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

# compute_uc

test_that("computed uc are ok", {
  dfm_uc <- rainette:::compute_uc(mini_dfm, min_uc_size = 3)
  expect_equal(docvars(dfm_uc)$rainette_uc_id, c(1,2,2,4,5,6,6))
})

test_that("error if uc can't be computed", {
  expect_error(compute_uc(mini_dfm, min_uc_size = 5)) 
})

# fchisq_val

test_that("fchisq_val result is ok", {
  set.seed(13370)
  t1 <- round(runif(100, 5, 50))
  t2 <- round(runif(100, 10, 200))
  row_sums <- t1 + t2
  total <- sum(t1 + t2)
  expect_equal(rainette:::fchisq_val(t1, t2, row_sums, total),
               unname(chisq.test(cbind(t1, t2))$statistic))
}) 


## docs_order_by_ca

test_that("doc_order_by_ca results are ok", {
  expect_equal(order(quanteda::textmodel_ca(mini_dfm)$rowcoord[,1]),
               rainette:::docs_order_by_ca(mini_dfm))
})

## split_tab_by_chisq
 
test_that("split_tab_by_chisq results are ok", {
  indices <- rainette:::docs_order_by_ca(mini_dfm)
  tab <- mini_dfm %>% 
    convert(to = "data.frame")
  tab <- tab[, -1] %>% 
    t %>% 
    as.data.frame
  res <- rainette:::split_tab_by_chisq(tab, indices)
  manual_res <- data.frame(index = -1, chisq = -1)
  for (i in 2:5) {
    tab1 <- tab[, indices[1:i], drop = FALSE]
    tab2 <- tab[, indices[(i + 1):7, drop = FALSE]]
    chisq <- chisq.test(cbind(rowSums(tab1), rowSums(tab2)))$statistic
    manual_res <- rbind(manual_res, c(index = indices[i], chisq = chisq))
  }
  max <- which.max(manual_res$chisq)
  expect_equal(res$max_index, manual_res$index[max])
  expect_equal(res$max_chisq, manual_res$chisq[max])
})


# rainette

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

## cutree

test_that("generic cutree still works", {
  hc <- hclust(dist(USArrests))
  expect_length(cutree(hc, h = 250), 50)
})

test_that("h argument generates error in cutree.rainette", {
  expect_error(cutree(res, h = 200))
})













