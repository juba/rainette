library(quanteda)
context("RcppEigen functions")

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
mini_dfm@docvars <- data.frame(rainette_uce_id = 1:nrow(mini_dfm))

# fchisq_val

test_that("eigen_chisq", {
  set.seed(13370)
  t1 <- as.integer(round(runif(100, 5, 50)))
  t2 <- as.integer(round(runif(100, 10, 200)))
  row_sums <- t1 + t2
  total <- sum(t1 + t2)
  expect_equal(rainette:::eigen_chisq(t1, t2, row_sums, total),
               unname(chisq.test(cbind(t1, t2))$statistic))
}) 

