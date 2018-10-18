
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



compute_partition <- function(dtm, min_members = 5) {
  
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
  ## Precompute rows sumet total
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
  
  return(list(group1 = group1, group2 = group2))
}
