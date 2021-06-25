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


# merge_segments

test_that("computed uc are ok when min_segment_size == 0", {
  dfm_uc <- rainette::merge_segments(mini_dfm, min_segment_size = 0)
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 3, 4, 5, 6, 7))
})

test_that("computed uc are ok when min_segment_size == 1", {
  dfm_uc <- rainette::merge_segments(mini_dfm, min_segment_size = 1)
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 3, 4, 5, 6, 7))
})

test_that("error when min_segment_size > 1 and no doc_id", {
  expect_error(merge_segments(mini_dfm, min_segment_size = 3))
})

test_that("computed uc are ok when min_segment_size > 0", {
  dfm_uc <- rainette::merge_segments(mini_dfm, min_segment_size = 3, doc_id = "doc_id")
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 2, 4, 5, 6, 6))
})

test_that("warning if uc can't be computed", {
  expect_warning(merge_segments(mini_dfm, min_segment_size = 20, doc_id = "doc_id"))
})

mini_dfm_docs <- mini_dfm
test_that("computed uc are ok with respect to doc_id", {
  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc1", "doc1", "doc2", "doc2", "doc2", "doc3")
  dfm_uc <- rainette::merge_segments(mini_dfm_docs, min_segment_size = 3, doc_id = "doc_id")
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 2, 4, 5, 5, 7))

  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc1", "doc1", "doc2", "doc3", "doc3", "doc3")
  dfm_uc <- rainette::merge_segments(mini_dfm_docs, min_segment_size = 4, doc_id = "doc_id")
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 1, 1, 4, 5, 6, 6))

  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc1", "doc1", "doc1", "doc3", "doc3", "doc3")
  dfm_uc <- rainette::merge_segments(mini_dfm_docs, min_segment_size = 6, doc_id = "doc_id")
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 1, 1, 1, 5, 5, 5))

  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc2", "doc3", "doc4", "doc4", "doc5", "doc5")
  expect_warning(dfm_uc <- rainette::merge_segments(mini_dfm_docs, min_segment_size = 3, doc_id = "doc_id"))
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 3, 4, 5, 6, 6))

  corpus <- head(data_corpus_inaugural, n = 10)
  corpus <- split_segments(corpus)
  dtm <- dfm(tokens(corpus, remove_punct = TRUE), tolower = TRUE)
  dtm <- dfm_remove(dtm, stopwords("en"))
  dtm <- dfm_trim(dtm, min_termfreq = 3)
  expect_warning(dfm_uc <- rainette::merge_segments(dtm, min_segment_size = 500))
  tmp <- docvars(dfm_uc) %>%
    group_by(rainette_uc_id) %>%
    summarise(nd = n_distinct(segment_source)) %>%
    filter(nd > 1)
  expect_equal(nrow(tmp), 0)
})

test_that("warning if uc can't be computed with respect to doc_id", {
  docvars(mini_dfm_docs, "doc_id") <- c("doc1", "doc1", "doc1", "doc2", "doc2", "doc2", "doc3")
  expect_warning(merge_segments(mini_dfm_docs, min_segment_size = 4, doc_id = "doc_id"))
})

mini_dfm_segsource <- mini_dfm
docvars(mini_dfm_segsource, "segment_source") <- "doc1"
test_that("segment_source used by default when min_segment_size > 0", {
  dfm_uc <- rainette::merge_segments(mini_dfm_segsource, min_segment_size = 3)
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1, 2, 2, 4, 5, 6, 6))
})


# clusters_by_doc_table
mini_dfm_count <- mini_dfm
docvars(mini_dfm_count, "segment_source") <- c("doc1", "doc1", "doc2", "doc2", "doc2", "doc2", "doc3")
docvars(mini_dfm_count, "doc_id") <- docvars(mini_dfm_count, "segment_source")
test_that("clusters_by_doc_table results", {
  docvars(mini_dfm_count, "cluster") <- c(1, 2, 2, 1, 2, NA, 1)
  expect_equal(
    clusters_by_doc_table(mini_dfm_count, clust_var = "cluster"),
    structure(list(doc_id = c("doc1", "doc2", "doc3"), clust_1 = c(1L,
      1L, 1L), clust_2 = c(1L, 2L, 0L), clust_NA = c(0L, 1L, 0L)), row.names = c(NA,
      -3L), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_equal(
    clusters_by_doc_table(mini_dfm_count, clust_var = "cluster", prop = TRUE),
    structure(list(doc_id = c("doc1", "doc2", "doc3"), clust_1 = c(50,
      25, 100), clust_2 = c(50, 50, 0), clust_NA = c(0, 25, 0)), row.names = c(NA,
      -3L), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_equal(
    clusters_by_doc_table(mini_dfm_count, clust_var = "cluster", doc_id = "doc_id", prop = TRUE),
    structure(list(doc_id = c("doc1", "doc2", "doc3"), clust_1 = c(50,
      25, 100), clust_2 = c(50, 50, 0), clust_NA = c(0, 25, 0)), row.names = c(NA,
      -3L), class = c("tbl_df", "tbl", "data.frame"))
  )
  docvars(mini_dfm_count, "cluster") <- c("C1", "C2", "C2", "C1", "NA", NA, NA)
  expect_equal(
    clusters_by_doc_table(mini_dfm_count, clust_var = "cluster"),
    structure(list(doc_id = c("doc1", "doc2", "doc3"), C1 = c(1L,
      1L, 0L), C2 = c(1L, 1L, 0L), `NA` = c(0L, 1L, 0L), NA_missing_ = c(0L,
      1L, 1L)), row.names = c(NA, -3L), class = c("tbl_df", "tbl",
      "data.frame"))
  )
})

# docs_by_cluster_table
test_that("docs_by_cluster_table results", {
  docvars(mini_dfm_count, "cluster") <- c(1, 2, 2, 1, 2, NA, 1)
  expect_equal(
    docs_by_cluster_table(mini_dfm_count, clust_var = "cluster"),
    structure(list(cluster = c("clust_1", "clust_2", "clust_NA"),
      n = 3:1, `%` = c(100, 66.6666666666667, 33.3333333333333)), row.names = c(NA,
      -3L), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_equal(
    docs_by_cluster_table(mini_dfm_count, clust_var = "cluster", threshold = 2),
    structure(list(cluster = c("clust_1", "clust_2", "clust_NA"),
      n = c(0, 1, 0), `%` = c(0, 33.3333333333333, 0)), row.names = c(NA,
      -3L), class = c("tbl_df", "tbl", "data.frame"))
  )
  docvars(mini_dfm_count, "cluster") <- c("C1", "C2", "C2", "C1", "NA", NA, NA)
  expect_equal(
    docs_by_cluster_table(mini_dfm_count, clust_var = "cluster", doc_id = "doc_id"),
    structure(list(cluster = c("C1", "C2", "NA", "NA_missing_"),
      n = c(2L, 2L, 1L, 2L), `%` = c(66.6666666666667, 66.6666666666667,
      33.3333333333333, 66.6666666666667)), row.names = c(NA, -4L
      ), class = c("tbl_df", "tbl", "data.frame"))
  )
})

# stat_col

test_that("stat_col is ok", {
  expect_equal(stat_col("chi2"), rlang::sym("chi2"))
  expect_equal(stat_col("lr"), rlang::sym("G2"))
})
