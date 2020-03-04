library(quanteda)
context("Rcpp functions")

m <- matrix(
  c(1L,1L,1L,0L,0L,
    0L,0L,1L,1L,0L,
    1L,0L,0L,0L,0L,
    1L,1L,1L,1L,0L,
    1L,1L,1L,1L,1L,
    1L,0L,0L,0L,0L,
    1L,1L,1L,0L,0L
    ),
  ncol = 5,
  byrow = TRUE,
)
mini_dfm <- as.dfm(m)

# cpp_chisq

test_that("cpp_chisq", {
  set.seed(13370)
  t1 <- as.integer(round(runif(100, 5, 50)))
  t2 <- as.integer(round(runif(100, 10, 200)))
  expect_equal(rainette:::cpp_chisq(t1, t2, t1 + t2),
               unname(chisq.test(cbind(t1, t2))$statistic))
})

