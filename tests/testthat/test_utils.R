library(quanteda)
context("utils functions")

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
docvars(mini_dfm, "rainette_uce_id") <- 1:nrow(mini_dfm)

# compute_uc

test_that("computed uc are ok", {
  dfm_uc <- rainette:::compute_uc(mini_dfm, min_uc_size = 3)
  expect_equal(docvars(dfm_uc, "rainette_uc_id"), c(1,2,2,4,5,6,6))
})

test_that("error if uc can't be computed", {
  expect_error(compute_uc(mini_dfm, min_uc_size = 5)) 
})

# stat_col

test_that("stat_col is ok", {
  expect_equal(stat_col("chi2"), rlang::sym("chi2"))
  expect_equal(stat_col("lr"), rlang::sym("G2"))
})


