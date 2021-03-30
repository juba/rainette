library(quanteda)
context("double reinert classification")

mini_corpus <- head(data_corpus_inaugural, n = 2)
mini_corpus <- split_segments(mini_corpus, 5)
dtm <- dfm(tokens(mini_corpus, remove_punct = TRUE), tolower = TRUE)
dtm <- dfm_remove(dtm, stopwords("en"))
dtm <- dfm_wordstem(dtm, language = "english")
dtm <- dfm_trim(dtm, min_termfreq = 3)

res1 <- rainette(dtm, k = 5, min_uc_size = 2, min_split_members = 2)
res2 <- rainette(dtm, k = 5, min_uc_size = 3, min_split_members = 2)
res12 <- rainette2(dtm, max_k = 5, uc_size1 = 2, uc_size2 = 3, min_members = 2)

res <- rainette2(res1, res2, min_members = 2)

test_that("compute_chi2 is ok", {
  tab <- matrix(c(45, 121 - 45, 257 - 45, 1213 - 121 - 257 +45), nrow = 2)
  expect_equal(rainette:::compute_chi2(45, 121, 257, 1213),
    chisq.test(tab)$statistic)
  tab <- matrix(c(0, 12, 25, 121 - 12 - 25), nrow = 2)
  expect_equal(rainette:::compute_chi2(0, 12, 25, 121),
    suppressWarnings(-chisq.test(tab)$statistic))
})

test_that("get_groups is ok", {
  expect_equal(dim(rainette:::get_groups(res1)), c(316, 4))
  expect_equal(substr(rainette:::get_groups(res1)[[2]],3,3), as.character(res1$uce_groups[[2]]))
  expect_equal(substr(rainette:::get_groups(res1)[[2]],1,2), rep("2.", length(res1$group)))
})

test_that("classes_crosstab is ok", {
  g1 <- tibble(c(1,1,2,2), c(1,1,2,3))
  g2 <- tibble(c(1,2,1,2), c(3,1,2,1))
  colnames(g1) <- 1:2
  colnames(g2) <- 1:2
  n_tot <- 4
  tab <- rainette:::classes_crosstab(g1, g2, n_tot)

  expect_equal(nrow(tab), 25)
  expect_equal(colnames(tab), c("g1", "g2", "n_both", "n1", "n2", "level1", "level2", "chi2"))
  tmp <- tab %>% dplyr::filter(level1 == 2, level2 == 2, g1 == 2, g2 == 2)
  expect_equal(tmp$n_both, 1)
  expect_equal(tmp$n1, 1)
  expect_equal(tmp$n2, 1)
  expect_equal(tmp$chi2, unname(rainette:::compute_chi2(tmp$n_both, tmp$n1, tmp$n2, n_tot)))
})

test_that("filter_crosstab is ok", {
  g1 <- tibble(c(1,1,1,1,2), c(1,1,2,3,3))
  g2 <- tibble(c(1,1,1,1,2), c(3,1,3,1,2))
  colnames(g1) <- 1:2
  colnames(g2) <- 1:2
  n_tot <- 5
  tab <- rainette:::classes_crosstab(g1, g2, n_tot)
  ftab <- rainette:::filter_crosstab(tab, g1, g2, min_members = 2, min_chi2 = 0.5)

  expect_equal(nrow(ftab), 1)
  expect_equal(ftab$g1, 1)
  expect_equal(ftab$g2, 1)
  expect_equal(ftab$n_both, 4)
  expect_equal(ftab$interclass, "1x1")
  expect_equal(ftab$chi2,
    suppressWarnings(unname(chisq.test(matrix(c(4,0,0,1), nrow=2))$statistic)))
  expect_equal(ftab$members, list(1:4))
})

test_that("cross_sizes is ok", {
  tab <- tibble(interclass = c("1.1x1.1", "1.2x1.1", "2.1x2.2", "2.1x1.1"),
    members = list(c(1,2,3), c(4,5,6), c(1,2), 5))
  sizes <- rainette:::cross_sizes(tab)
  expect_equal(sizes, structure(c(1, 1, 1, 1, 0, 1, 1, 1, 2, 0, 1, 1, 0, 1, 0, 1), .Dim = c(4L,
    4L), .Dimnames = list(c("1.1x1.1", "1.2x1.1", "2.1x2.2", "2.1x1.1"
    ), c("1.1x1.1", "1.2x1.1", "2.1x2.2", "2.1x1.1"))))
})

test_that("next_partitions is ok", {
  tab <- tibble(interclass = c("1.1x1.1", "1.2x1.1", "2.1x2.2", "2.1x1.1"),
    members = list(c(1,2,3), c(4,5,6), c(1,2), 7))
  sizes <- rainette:::cross_sizes(tab)
  partitions <- list(colnames(sizes))

  partitions[[2]] <- rainette:::next_partitions(partitions, sizes)
  expect_equal(partitions[[2]], list(c("1.1x1.1", "1.2x1.1"), c("1.1x1.1", "2.1x1.1"), c("1.2x1.1",
    "2.1x2.2"), c("1.2x1.1", "2.1x1.1"), c("2.1x2.2", "2.1x1.1")))

  partitions[[3]] <- rainette:::next_partitions(partitions, sizes)
  expect_equal(partitions[[3]], list(c("1.1x1.1", "1.2x1.1", "2.1x1.1"), c("1.2x1.1", "2.1x2.2",
    "2.1x1.1")))
  expect_equal(rainette:::next_partitions(partitions, sizes), NULL)
})


test_that("get_optimal_partitions is ok", {
  valid <- tibble(interclass = c("1x1", "2x2", "3x3", "4x4"),
                  n_both = c(3, 3, 2, 1),
                  chi2 = c(4, 5, 10, 4),
                  members = list(c(1,2,3), c(4,5,6), c(1,2), 7), stringsAsFactors = FALSE)
  partitions <- list(
    list(c("1x1", "2x2"), c("2x2", "3x3"), c("1x1", "4x4")),
    list(c("1x1", "2x2", "4x4")))
  n_tot <- 7
  res <- rainette:::get_optimal_partitions(partitions, valid, n_tot)
  tmp <- res[res$k == 2,]
  expect_equal(tmp$clusters, list(c("1x1", "2x2"), c("2x2","3x3")))
  expect_equal(tmp$chi2, c(9, 15))
  expect_equal(tmp$n, c(6, 5))
  expect_equal(tmp$groups, list(c(1,1,1,2,2,2,NA),c(2,2,NA,1,1,1,NA)))
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

test_that("rainette2 is ok when stopping before max_k", {
  res1 <- rainette(dtm, k = 4, min_uc_size = 2, min_split_members = 30)
  res2 <- rainette(dtm, k = 4, min_uc_size = 3, min_split_members = 30)
  expect_message(res <- rainette2(res1, res2, max_k = 4, min_members = 50),
    "^! No more partitions found, stopping at k=2")
  expect_equal(max(res$k), 2)
})
