## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----warnings = FALSE, message  = FALSE---------------------------------------
library(quanteda)
library(rainette)
corpus <- split_segments(data_corpus_inaugural)

## -----------------------------------------------------------------------------
corpus

## -----------------------------------------------------------------------------
head(docvars(corpus))

## -----------------------------------------------------------------------------
dtm <- dfm(corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)

## -----------------------------------------------------------------------------
dtm <- dfm_trim(dtm, min_termfreq = 10)

## ----message = FALSE----------------------------------------------------------
res <- rainette(dtm, k = 5, min_uc_size = 15)

## ----eval = FALSE-------------------------------------------------------------
#  rainette_explor(res, dtm)

## -----------------------------------------------------------------------------
groups <- cutree_rainette(res, k = 3)

## -----------------------------------------------------------------------------
corpus$group <- groups
head(docvars(corpus))

## -----------------------------------------------------------------------------
rainette_stats(corpus$group, dtm, n_terms = 5)

## ----message=FALSE------------------------------------------------------------
res1 <- rainette(dtm, k = 5, min_uc_size = 10, min_split_members = 10)
res2 <- rainette(dtm, k = 5, min_uc_size = 15, min_split_members = 10)

## ----message=FALSE------------------------------------------------------------
res <- rainette2(res1, res2, max_k = 5, min_members = 10)

## ----eval=FALSE---------------------------------------------------------------
#  res <- rainette2(dtm, uc_size1 = 10, uc_size2 = 15, max_k = 5, min_members = 10)

## ----eval=FALSE---------------------------------------------------------------
#  rainette2_explor(res, dtm)

## -----------------------------------------------------------------------------
groups <- cutree(res, 3)
rainette_stats(groups, dtm, n_terms = 5)

## -----------------------------------------------------------------------------
groups_completed <- rainette2_complete_groups(dtm, groups)

