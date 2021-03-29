## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("juba/rainette")

## ----message=FALSE------------------------------------------------------------
library(rainette)
library(quanteda)
## Import du corpus
fichier <- system.file("extdata", "manifeste_pc.txt", package = "rainette")
corpus <- import_corpus_iramuteq(fichier)

## -----------------------------------------------------------------------------
corpus

## ----paged.print=TRUE---------------------------------------------------------
docvars(corpus)

## ----message=FALSE------------------------------------------------------------
corpus <- split_segments(corpus, segment_size = 40)

## -----------------------------------------------------------------------------
corpus

## -----------------------------------------------------------------------------
head(docvars(corpus))

## -----------------------------------------------------------------------------
as.character(corpus)[1:2]

## -----------------------------------------------------------------------------
dtm <- dfm(corpus, remove = stopwords("fr"), tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE)

## -----------------------------------------------------------------------------
dtm <- dfm_wordstem(dtm, language = "french")
dtm <- dfm_trim(dtm, min_termfreq = 3)

## ----message=FALSE------------------------------------------------------------
res <- rainette(dtm, k = 5, min_uc_size = 10, min_split_members = 10)

## -----------------------------------------------------------------------------
res

## ----eval = FALSE-------------------------------------------------------------
#  rainette_explor(res, dtm)

## ----eval=FALSE---------------------------------------------------------------
#  ## Clustering description plot
#  rainette_plot(res, dtm, k = 5, type = "bar", n_terms = 20, free_scales = FALSE,
#      measure = "chi2", show_negative = "TRUE", text_size = 11)
#  ## Groups
#  cutree_rainette(res, k = 5)

## -----------------------------------------------------------------------------
corpus$group <- cutree_rainette(res, k = 5)
head(docvars(corpus))

## -----------------------------------------------------------------------------
rainette_stats(corpus$group, dtm, n_terms = 5)

## ----message=FALSE------------------------------------------------------------
res1 <- rainette(dtm, k = 7, min_uc_size = 10, min_split_members = 10)
res2 <- rainette(dtm, k = 7, min_uc_size = 15, min_split_members = 10)

## ----message=FALSE------------------------------------------------------------
res <- rainette2(res1, res2, max_k = 7, min_members = 10)

## ----eval=FALSE---------------------------------------------------------------
#  res <- rainette2(dtm, uc_size1 = 10, uc_size2 = 15, max_k = 7, min_members = 10)

## ----eval=FALSE---------------------------------------------------------------
#  rainette2_explor(res, dtm)

## -----------------------------------------------------------------------------
groups <- cutree(res, 3)
rainette_stats(groups, dtm, n_terms = 5)

## -----------------------------------------------------------------------------
groups_completed <- rainette2_complete_groups(dtm, groups)

