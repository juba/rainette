library(quanteda)
context("double reinert classification")

mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus, 5)
dtm <- dfm(tokens(mini_corpus, remove_punct = TRUE), tolower = TRUE)
dtm <- dfm_remove(dtm, stopwords("en"))
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)

res1 <- rainette(dtm, k = 5, min_segment_size = 2, min_split_members = 3)
res2 <- rainette(dtm, k = 5, min_segment_size = 3, min_split_members = 3)
res12 <- rainette2(dtm, max_k = 5, min_segment_size1 = 2, min_segment_size2 = 3, min_members = 3)

res <- rainette2(res1, res2, min_members = 2)
res_notfull <- rainette2(res1, res2, min_members = 2, full = FALSE)

test_that("compute_chi2", {
  tab <- matrix(c(45, 121 - 45, 257 - 45, 1213 - 121 - 257 + 45), nrow = 2)
  expect_equal(rainette:::compute_chi2(45, 121, 257, 1213),
    chisq.test(tab)$statistic)
  tab <- matrix(c(0, 12, 25, 121 - 12 - 25), nrow = 2)
  expect_equal(rainette:::compute_chi2(0, 12, 25, 121),
    suppressWarnings(-chisq.test(tab)$statistic))
})

test_that("get_groups", {
  expect_equal(dim(rainette:::get_groups(res1)), c(316, 4))
  expect_equal(substr(rainette:::get_groups(res1)[[2]],3,3), as.character(res1$uce_groups[[2]]))
  expect_equal(substr(rainette:::get_groups(res1)[[2]],1,2), rep("2.", length(res1$group)))
})

test_that("groups_crosstab", {
  g1 <- tibble(c(1,1,2,2), c(1,1,2,3))
  g2 <- tibble(c(1,2,1,2), c(3,1,2,1))
  colnames(g1) <- 1:2
  colnames(g2) <- 1:2
  n_tot <- 4
  tab <- rainette:::groups_crosstab(g1, g2, min_members = -Inf, min_chi2 = -Inf)

  expect_equal(nrow(tab), 25)
  expect_equal(
    colnames(tab),
    c("g1", "g2", "n_both", "level1", "level2", "n1", "n2", "chi2", "interclass")
  )
  tmp <- tab %>% dplyr::filter(level1 == 2, level2 == 2, g1 == 2, g2 == 2)
  expect_equal(tmp$n_both, 1)
  expect_equal(tmp$n1, 1)
  expect_equal(tmp$n2, 1)
  expect_equal(tmp$chi2, unname(rainette:::compute_chi2(tmp$n_both, tmp$n1, tmp$n2, n_tot)))
  expect_equal(tmp$interclass, "2x2")
})

test_that("crosstab_add_members and filtering", {
  g1 <- tibble(c(1,1,1,1,2), c(1,1,2,3,3))
  g2 <- tibble(c(1,1,1,1,2), c(3,1,3,1,2))
  colnames(g1) <- 1:2
  colnames(g2) <- 1:2
  n_tot <- 5
  tab <- rainette:::groups_crosstab(g1, g2, min_members = 2, min_chi2 = 0.5)
  tab <- rainette:::crosstab_add_members(tab, g1, g2)

  expect_equal(nrow(tab), 1)
  expect_equal(tab$g1, 1)
  expect_equal(tab$g2, 1)
  expect_equal(tab$n_both, 4)
  expect_equal(tab$interclass, "1x1")
  expect_equal(tab$chi2,
    suppressWarnings(unname(chisq.test(matrix(c(4,0,0,1), nrow=2))$statistic)))
  expect_equal(tab$members, list(1:4))
})

test_that("crosstab_keep_max", {
  df <- tribble(
    ~ g1, ~ g2, ~chi2,
      11,   21,   10,
      11,   22,   5,
      11,   23,   8,
      12,   21,   9, 
      12,   22,   8,
      12,   23,   10,
      13,   21,   8,
      13,   22,   2,
      13,   23,   2
  )
  res <- crosstab_keep_max(df)
  expect_equal(res$g1, c(11, 12))
  expect_equal(res$g2, c(21, 23))
})

test_that("cross_sizes", {
  tab <- tibble(interclass = c("1.1x1.1", "1.2x1.1", "2.1x2.2", "2.1x1.1"),
    members = list(c(1,2,3), c(4,5,6), c(1,2), 5), id = 1:4)
  sizes <- rainette:::cross_sizes(tab)
  expect_equal(sizes, structure(c(1, 1, 1, 1, 0, 1, 1, 1, 2, 0, 1, 1, 0, 1, 0, 1), .Dim = c(4L,
    4L)))
})

test_that("next_partitions", {
  tab <- tibble(interclass = c("1.1x1.1", "1.2x1.1", "2.1x2.2", "2.1x1.1"),
    members = list(c(1,2,3), c(4,5,6), c(1,2), 7), id = 1:4)
  sizes <- rainette:::cross_sizes(tab)
  partitions <- list(tab$id)
  partitions[[2]] <- which(sizes == 0, arr.ind = TRUE, useNames = FALSE) %>%
      asplit(1)

  partitions[[3]] <- rainette:::next_partitions_for(partitions[[2]], sizes)
  expect_equal(partitions[[3]], list(c(1, 2, 4), c(2, 3, 4)))
  partitions[[3]] <- rainette:::next_partitions_parallel(partitions[[2]], sizes)
  expect_equal(partitions[[3]], list(c(1, 2, 4), c(2, 3, 4)))
  expect_equal(rainette:::next_partitions_for(partitions[[3]], sizes), NULL)
  expect_equal(rainette:::next_partitions_parallel(partitions[[3]], sizes), NULL)
})


test_that("get_optimal_partitions", {
  valid <- tibble(interclass = c("1x1", "2x2", "3x3", "4x4"),
                  n_both = c(3, 3, 2, 1),
                  chi2 = c(4, 5, 10, 4),
                  members = list(c(1,2,3), c(4,5,6), c(1,2), 7),
                  id = 1:4)
  partitions <- list(
    list(c(1, 2), c(2, 3), c(1, 4)),
    list(c(1, 2, 4)))
  n_tot <- 7

  # With full = TRUE
  res <- rainette:::get_optimal_partitions(partitions, valid, n_tot, full = TRUE)
  tmp <- res[res$k == 2,]
  expect_equal(tmp$clusters, list(c("1x1", "2x2"), c("2x2", "3x3")))
  expect_equal(tmp$chi2, c(9, 15))
  expect_equal(tmp$n, c(6, 5))
  expect_equal(tmp$groups, list(c(1,1,1,2,2,2,NA),c(2,2,NA,1,1,1,NA)))
  tmp <- res[res$k == 3,]
  expect_equal(tmp$clusters, list(c("1x1", "2x2", "4x4")))
  expect_equal(tmp$chi2, 13)
  expect_equal(tmp$n, 7)
  expect_equal(tmp$groups, list(c(1,1,1,2,2,2,3)))

  # With full = FALSE
  res <- rainette:::get_optimal_partitions(partitions, valid, n_tot, full = FALSE)
  tmp <- res[res$k == 2,]
  expect_equal(tmp$clusters, list(c("2x2", "3x3")))
  expect_equal(tmp$chi2, 15)
  expect_equal(tmp$n, 5)
  expect_equal(tmp$groups, list(c(2,2,NA,1,1,1,NA)))
  tmp <- res[res$k == 3,]
  expect_equal(tmp$clusters, list(c("1x1", "2x2", "4x4")))
  expect_equal(tmp$chi2, 13)
  expect_equal(tmp$n, 7)
  expect_equal(tmp$groups, list(c(1,1,1,2,2,2,3)))
})

test_that("rainette2 gives the same result on dtm and on two clustering results", {
  expect_equal(res[,c("k", "chi2", "n")], res12[,c("k", "chi2", "n")])
  expect_equal(res$groups, res12$groups)
  expect_equal(res$clusters, res12$clusters)
})

test_that("rainette2 display message when stopping before max_k", {
  res1 <- rainette(dtm, k = 4, min_segment_size = 2, min_split_members = 30)
  res2 <- rainette(dtm, k = 4, min_segment_size = 3, min_split_members = 30)
  expect_message(res <- rainette::rainette2(res1, res2, max_k = 4, min_members = 50),
    "^! No more partitions found, stopping at k=2")
  expect_equal(max(res$k), 2)
})

test_that("rainette2_plot error if full=FALSE and criterion=n", {
  expect_error(
    rainette2_plot(res_notfull, dtm, k = 2, criterion = "n"),
    "if rainette2 has been computed with full=FALSE, only 'chi2' criterion is available"
  )
  expect_error(
    rainette2_plot(res, dtm, k = 2, criterion = "n"),
    NA
  )
})

test_that("same results with different values of full and parallel", {
  res_f <- rainette2(res1, res2, max_k = 5, min_members = 2, full = TRUE)
  res_fp <- rainette2(res1, res2, max_k = 5, min_members = 2, full = TRUE, parallel = TRUE)
  expect_equal(res_f, res_fp)
  expect_equal(attr(res_f, "full"), TRUE)
  expect_equal(attr(res_fp, "full"), TRUE)

  res_c <- rainette2(res1, res2, max_k = 5, min_members = 2, full = FALSE)
  res_cp <- rainette2(res1, res2, max_k = 5, min_members = 2, full = FALSE, parallel = TRUE)
  expect_equal(res_c, res_cp)
  expect_equal(attr(res_c, "full"), FALSE)
  expect_equal(attr(res_cp, "full"), FALSE)
})

test_that("plot functions class checking", {
  expect_error(rainette2_plot(res1, k = 5), "res must be a rainette2 result object")
  expect_error(rainette_plot(res12, k = 5), "res must be a rainette result object")
})