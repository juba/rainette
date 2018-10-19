
fchisq_val <- function(tab1, tab2, row_sum, n) {
  tmp <- cbind(tab1, tab2)
  col_sum <- colSums(tmp)
  E <- outer(row_sum, col_sum, "*") / n
  sum((tmp - E)^2 / E)
}

# TODO
reinert_cluster <- function(dtm, k = 10) {
  dtm <- dfm_weight(dtm, scheme = "boolean")
  for (i in 1:k) {
    compute_partition(dtm)
  }
}


##' @export

compute_partition <- function(dtm, min_members = 5, cc_test = 0.3) {
  
  ## First step : CA partition
  
  cat("First step : clustering along first CA factor\n")
  
  ## Compute first factor of CA on DTM
  afc <- quanteda::textmodel_ca(dtm, nd = 1)
  ## Order documents by their first factor coordinates
  indices <- afc$rowcoord %>% 
    as.data.frame %>% 
    mutate(index = 1:n()) %>% 
    arrange(Dim1) %>% 
    pull(index)
  ## Transpose and convert DTM to ease computations
  tab <- dtm %>% 
    convert(to = "data.frame") %>% 
    select(-document) %>% 
    t %>% 
    as.data.frame
  ## Precompute rows sum et total
  row_sum <- rowSums(tab)
  total <- sum(tab)

  ## First iteration
  ## Compute first aggregate table
  tab1 <- rowSums(tab[,indices[1], drop = FALSE])
  tab2 <- rowSums(tab[,indices[-1]])
  ## Initialize chisquaredand index
  max_chisq <- fchisq_val(tab1, tab2, row_sum, total)
  max_index <- indices[1]
  
  ## For each index (ordered by first factor coordinates)
  for (index in indices[2:(length(indices) - 1)]) {
    ## Recompute aggregate table for both groups
    tab1 <- tab1 + tab[,index]
    tab2 <- tab2 - tab[,index]
    ## Compute chi squared of aggregate table and compare
    chisq <- fchisq_val(tab1, tab2, row_sum, total)
    if (chisq > max_chisq) {
      max_chisq <- chisq
      max_index <- index
    }
  }
  
  ## Second step : switching points
  
  cat("Second step : points switching\n")
  
  ## Group indices and tabs  
  group1 <- indices[1:which(indices == max_index)]
  group2 <- indices[(which(indices == max_index) + 1):length(indices)]
  tab1 <- rowSums(tab[,group1])
  tab2 <- rowSums(tab[,group2])
  switched <- 1
  iteration <- 0

  ## Run while points are switched, and groups size is ok
  while(switched > 0 && length(group1) >= min_members && length(group2) >= min_members) {
    
    switched <- 0
    iteration <- iteration + 1
    
    ## For each point
    for (index in indices) {
      ## Switch current element
      if (index %in% group1) {
        tab1_new <- tab1 - tab[,index]
        tab2_new <- tab2 + tab[,index]
      } else {
        tab1_new <- tab1 + tab[,index]
        tab2_new <- tab2 - tab[,index]
      }
      ## Compute new chi-squared
      chisq_new <- fchisq_val(tab1_new, tab2_new, row_sum, total)
      ## Compare
      if (chisq_new > chisq) {
        if (index %in% group1) {
          group1 <- group1[-which(group1 == index)] 
          group2 <- c(group2, index)
        } else {
          group1 <- c(group1, index)
          group2 <- group2[-which(group2 == index)] 
        }
        tab1 <- tab1_new
        tab2 <- tab2_new
        chisq <- chisq_new
        switched <- switched + 1
      }
    }
    
    cat(paste0("  Iteration ", iteration, " : ", switched," switched, chisq=", chisq, "\n"))
  }
  
  ## Third step : features elimination
  
  cat("Third step : features elimination\n")

  ## Total number of features in each group
  nfeat_group1 <- sum(tab1)
  nfeat_group2 <- sum(tab2)
  ## Observed frequency of features
  observed <- cbind(tab1, tab2)
  ## Expected frequency of features
  expected_prop <- (tab1 + tab2) / sum(tab1 + tab2)
  expected <- cbind(expected_prop * nfeat_group1, expected_prop * nfeat_group2)
  ## Chi2 and contingency coefficients for each feature
  feat_chisq <- rowSums((observed - expected)^2 / expected)
  feat_cc <- sqrt(feat_chisq / (feat_chisq + rowSums(observed)))
  
  ## Features selection
  cols1 <- character()
  cols2 <- character()
  
  for (i in seq_along(feat_cc)) {
    cc <- feat_cc[i]
    name <- names(feat_cc)[i]
    ## Keep feature if cc <= cc_test and frequency > 0
    if (cc <= cc_test && tab1[i] > 0) {
      cols1 <- c(cols1, name)
    }
    if (cc <= cc_test && tab2[i] > 0) {
      cols2 <- c(cols2, name)
    }
    ## If cc > cc_test, only keep feature in the group
    ## where observed frequency > expected frequency
    if (cc > cc_test) {
      if (tab1[i] > expected[i, 1]) {
        cols1 <- c(cols1, name)
      }
      if (tab2[i] > expected[i, 2]) {
        cols2 <- c(cols2, name)
      }
    }
  }
  
  return(list(group1 = group1, group2 = group2))
}
