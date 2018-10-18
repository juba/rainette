library(quanteda)
library(tm)
context("split_segments")

data(acq)
data(data_corpus_inaugural)
corpus <- head(data_corpus_inaugural)
text <- corpus$documents$texts[[1]]

split_char <- split_segments(text)
split_tm <- split_segments(acq)
split_quanteda <- split_segments(corpus)

test_that("split_segments.character is ok", {
  expect_equal(dim(split_char), c(40,1))
  expect_equal(as.character(split_char[1,]), "Fellow-Citizens of the Senate and of the House of Representatives : Among the vicissitudes incident to life no event could have filled me with greater anxieties than that of which the notification was transmitted by your order , and received on the 14th day of the present month .")
}) 

test_that("split_segments.Corpus is ok", {
  expect_equal(ndoc(split_tm), 183)
  expect_equal(docvars(split_tm)$segment_source[1], "10")
  expect_equal(rownames(docvars(split_tm))[1], "10_1")
})

test_that("split_segments.corpus is ok", {
  expect_equal(ndoc(split_quanteda), 248)
  expect_equal(docvars(split_quanteda)$segment_source[1], "1789-Washington")
  expect_equal(rownames(docvars(split_quanteda))[1], "1789-Washington_1")
})

