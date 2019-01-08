
fchisq_val <- function(tab1, tab2, row_sum, n) {
  tmp <- cbind(tab1, tab2)
  col_sum <- colSums(tmp)
  E <- outer(row_sum, col_sum, "*") / n
  sum((tmp - E)^2 / E)
}


## uce merging into uc based on minimum uc size
## this function just returns the original dtm with a new
## `rainette_uc_index` docvar

compute_uc <- function(dtm, min_uc_size = 10) {
  ## Size of each uce
  terms_by_uce <- rowSums(dtm)
  if (any(terms_by_uce < min_uc_size)) {
    index <- 1
    uc_id <- docvars(dtm)$rainette_uce_id
    while (index < length(terms_by_uce)) {
      current_size <- terms_by_uce[index]
      grouping_index <- index
      ## While current uc size is smaller than min, regroup with following uce
      while(current_size < min_uc_size) {
        grouping_index <- grouping_index + 1
        if (grouping_index > length(terms_by_uce)) {
          stop("can't compute uc with respect to min_uc_size")
        }
        current_size <- current_size + terms_by_uce[grouping_index]
        uc_id[grouping_index] <- index
      }
      index <- grouping_index + 1
    }
    ## Add computed uc ids to docvars
    docvars(dtm)$rainette_uc_id <- uc_id
  }
  
  return(dtm)
}



##' @export

rainette <- function(dtm, k = 10, min_uc_size = 10, min_members = 5, cc_test = 0.3, tsj = 3,...) {
  
  if (any(dtm@x > 1)) {
    ## We don't use dfm_weight here because of https://github.com/quanteda/quanteda/issues/1545
    dtm@x <- as.numeric(dtm@x > 0)
  }
  
  ## Add id to documents
  docvars(dtm)$rainette_uce_id <- 1:nrow(dtm)
  
  ## Compute uc ids based on minimum size
  dtm <- compute_uc(dtm, min_uc_size = min_uc_size)
  ## Correspondance table between uce and uc
  corresp_uce_uc <- data.frame(uce = docvars(dtm)$rainette_uce_id, uc = docvars(dtm)$rainette_uc_id)
  ## Group dfm by uc
  dtm <- dfm_group(dtm, docvars(dtm)$rainette_uc_id)
  dtm <- dfm_weight(dtm, scheme = "boolean")
  
  ## Initialize results list with first dtm
  res <- list(list(tabs = list(dtm)))
  
  ## Display progress bar
  pb <- progress::progress_bar$new(total = k - 1,
    format = "  Clustering [:bar] :percent in :elapsed",
    clear = FALSE, show_after = 0)
  invisible(pb$tick(0))
  
  for (i in 1:(k - 1)) {

    ## Split the biggest group
    biggest_group <- which.max(purrr::map(res[[i]]$tabs, nrow))
    if (nrow(res[[i]]$tabs[[biggest_group]]) < min_members) {
      warning("\nNo more group bigger than min_members. Stopping after iteration ", i, ".")
      k <- i
      break
    }
    tab <- res[[i]]$tabs[[biggest_group]]
    ## textmodel_ca only works if nrow >= 3 and ncol >= 3
    if (nrow(tab) < 3 || ncol(tab) < 3) {
      warning("\nTab to be splitted is not big enough. Stopping after iteration ", i, ".")
      k <- i
      break
    }
    clusters <- split_tab(tab, cc_test = cc_test, tsj = tsj,...)
    
    ## Populate results
    res[[i + 1]] <- list()
    res[[i + 1]]$height <- clusters$height
    res[[i + 1]]$splitted <- c(biggest_group, biggest_group + 1)
    res[[i + 1]]$tabs <- append(res[[i]]$tabs[-biggest_group], 
                                clusters$tabs,
                                after = biggest_group - 1)
    res[[i + 1]]$groups <- append(res[[i]]$groups[-biggest_group], 
                                  clusters$groups,
                                  after = biggest_group - 1)
    pb$tick(1)
  }
  
  res <- res[-1]
  
  ## Compute the merge element of resulting hclust result
  groups <- 1:k
  merge <- matrix(nrow = 0, ncol = 2)
  for (i in (k-1):1) {
    split <- res[[i]]$splitted
    merge <- rbind(merge, -groups[split])
    groups <- groups[-split[2]]
    groups[split[1]] <- -(k-i)
  }
  
  ## Compute groups by uce at each k
  uce_groups <- list()
  for (i in 1:(k-1)) {
    group <- rep(NA, nrow(corresp_uce_uc))
    indices <- res[[i]]$groups
    for (group_index in seq_along(indices)) {
      group[corresp_uce_uc$uc %in% indices[[group_index]]] <- group_index
    }
    uce_groups[[i]] <- group
  }
  
  ## Get the final group element of resulting hclust result
  ## by uce
  group <- uce_groups[[k - 1]]

  ## Compute and return hclust-class result
  hres <- list(method = "reinert",
              call = match.call(),
              height = rev(purrr::map_dbl(res, ~.x$height)),
              order = 1:k,
              labels = as.character(1:k),
              merge = merge,
              group = group,
              uce_groups = uce_groups,
              # TODO remove or clean up results below
              corresp_uce_uc = corresp_uce_uc,
              res = res)
  
  class(hres) <- c("rainette", "hclust")
  hres
}

##' @export

cutree.rainette <- function(hres, k = NULL, h = NULL) {
  if (!is.null(h)) {
    stop("cutree.rainette only works with k argument")
  }
  # TODO
  cut <- stats::cutree(hres, k, h)
  unname(cut[as.character(hres$group)])
}

##' @export

split_tab <- function(dtm, cc_test = 0.3, tsj = 3, ...) {
  
  ## First step : CA partition

  ## Remove documents with zero terms
  rs <- rowSums(dtm) > 0
  dtm <- dtm[rs,]
    
  ## Compute first factor of CA on DTM
  afc <- quanteda::textmodel_ca(dtm, nd = 1, ...)
  ## Order documents by their first factor coordinates
  indices <- afc$rowcoord %>% 
    as.data.frame %>% 
    mutate(index = 1:n()) %>% 
    arrange(Dim1) %>% 
    pull(index)
  ## Transpose and convert DTM to ease computations
  tab <- dtm %>% 
    convert(to = "data.frame")
  if (sum(colnames(tab) == 'document') == 1) {
    tab <- tab %>% 
      select(-document) %>% 
      t %>% 
      as.data.frame
  } else {
    tab <- tab[, -1] %>% 
      t %>% 
      as.data.frame
  }
  ## Precompute rows sum et total
  row_sum <- rowSums(tab)
  total <- sum(tab)

  ## First iteration
  ## Compute first aggregate table
  tab1 <- rowSums(tab[,indices[1], drop = FALSE])
  tab2 <- rowSums(tab[,indices[-1]])
  ## Initialize chisquared and index
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
  
  ## Group indices and tabs  
  group1 <- indices[1:which(indices == max_index)]
  group2 <- indices[(which(indices == max_index) + 1):length(indices)]
  tab1 <- rowSums(tab[,group1, drop = FALSE])
  tab2 <- rowSums(tab[,group2, drop = FALSE])
  switched <- 1
  iteration <- 0

  ## Run while points are switched
  while(switched > 0) {
    
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
      ## chisq_new can be NaN if one of tab1 or tab2 is only zeros
      if (!is.nan(chisq_new) && chisq_new > chisq) {
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
  }
  
  ## Third step : features elimination
  
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
    if (cc <= cc_test && tab1[i] > tsj) {
      cols1 <- c(cols1, name)
    }
    if (cc <= cc_test && tab2[i] > tsj) {
      cols2 <- c(cols2, name)
    }
    ## If cc > cc_test, only keep feature in the group
    ## where observed frequency > expected frequency
    if (cc > cc_test) {
      if (tab1[i] > expected[i, 1] && tab1[i] > tsj) {
        cols1 <- c(cols1, name)
      }
      if (tab2[i] > expected[i, 2] && tab2[i] > tsj) {
        cols2 <- c(cols2, name)
      }
    }
  }
  
  return(list(groups = list(docvars(dtm)$rainette_uc_id[group1],
                            docvars(dtm)$rainette_uc_id[group2]), 
              tabs = list(dtm[group1, cols1], 
                          dtm[group2, cols2]), 
              height = chisq))
}
