library(quanteda)
library(tm)
library(tibble)
context("split_segments")

data(acq)
data(data_corpus_inaugural)
corpus <- head(data_corpus_inaugural)
text <- corpus$documents$texts[[1]]

split_char <- split_segments(text)
split_tm <- split_segments(acq)
split_quanteda <- split_segments(corpus)

test_that("split_segments.character is ok", {
  expect_equal(dim(split_char), c(37,1))
  expect_equal(as.character(split_char[1,]), "Fellow-Citizens of the Senate and of the House of Representatives:\n\nAmong the vicissitudes incident to life no event could have filled me with greater anxieties than that of which the notification was transmitted by your order, and received on the 14th day of the present month.")
  expect_equal(split_segments("One more test."), tibble(segment = "One more test."))
  expect_equal(split_segments("J'apprends des techniques d'écriture. Cela me permet d'écrire autre choses que des lettres ou un récit de ma vie/journal_intime. Cela donne des idées pour écrire des fictions, sortir de son quotidien. J'aime bien l'effort intellectuel que cela demande. C'est un rendez-vous apaisant."), tibble(segment = "J'apprends des techniques d'écriture. Cela me permet d'écrire autre choses que des lettres ou un récit de ma vie/journal_intime. Cela donne des idées pour écrire des fictions, sortir de son quotidien. J'aime bien l'effort intellectuel que cela demande. C'est un rendez-vous apaisant."))
  
  expect_equal(split_segments("Cool ! Cool cool cool."), 
    tibble(segment = c("Cool ! Cool cool cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 1), 
    tibble(segment = c("Cool", "!", "Cool", "cool", "cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 1, 1), 
    tibble(segment = c("Cool !", "Cool", "cool cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 1, 2), 
    tibble(segment = c("Cool !", "Cool cool cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 2), 
    tibble(segment = c("Cool !", "Cool cool", "cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 2, 2), 
    tibble(segment = c("Cool !", "Cool cool cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 3), 
    tibble(segment = c("Cool !", "Cool cool cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 3, 2), 
    tibble(segment = c("Cool ! Cool cool cool.")))
  expect_equal(split_segments("Cool ! Cool cool cool.", 3, 3), 
    tibble(segment = c("Cool ! Cool cool cool.")))
  
}) 

test_that("split_segments.Corpus is ok", {
  expect_equal(ndoc(split_tm), 188)
  expect_equal(docvars(split_tm)$segment_source[1], "10")
  expect_equal(rownames(docvars(split_tm))[1], "10_1")
})

test_that("split_segments.corpus is ok", {
  expect_equal(ndoc(split_quanteda), 229)
  expect_equal(docvars(split_quanteda)$segment_source[1], "1789-Washington")
  expect_equal(rownames(docvars(split_quanteda))[1], "1789-Washington_1")
})

test_that("split_segments input checking is ok", {
  expect_error(rainette:::split_segments.character(1:10))
  expect_error(rainette:::split_segments.corpus(1:10))
  expect_error(rainette:::split_segments.Corpus(1:10))
  expect_error(rainette:::split_segments.character(c("a", "b")))
})
