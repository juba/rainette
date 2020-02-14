#' Corpus clustering based on the Reinert method - Simple clustering
#'
#' @param dtm quanteda dfm object of documents to cluster, usually the 
#'   result of [split_segments()]
#' @param k maximum number of clusters to compute
#' @param min_uc_size minimum number of forms by document
#' @param min_split_members don't try to split groups with fewer members
#' @param cc_test contingency coefficient value for feature selection
#' @param tsj minimum frequency value for feature selection
#' @param min_members deprecated, use `min_split_members` instead
#' @param ... parameters passed to [quanteda::textmodel_ca]
#'
#' @details
#' See the references for original articles on the method. Computations and results may differ
#' quite a bit, see the package vignettes for more details.
#'
#' @return
#' The result is a list of both class `hclust` and `rainette`. Besides the elements 
#' of an `hclust` object, two more results are available :
#' 
#' - `uce_groups` give the group of each document for each k
#' - `group` give the group of each document for the maximum value of k available
#'
#' @seealso [split_segments()], [rainette2()], [cutree_rainette()], [rainette_plot()], [rainette_explor()]
#'   
#' @references 
#' 
#' - Reinert M, Une méthode de classification descendante hiérarchique : application à l'analyse lexicale par contexte, Cahiers de l'analyse des données, Volume 8, Numéro 2, 1983. <http://www.numdam.org/item/?id=CAD_1983__8_2_187_0>
#' - Reinert M., Alceste une méthodologie d'analyse des données textuelles et une application: Aurelia De Gerard De Nerval, Bulletin de Méthodologie Sociologique, Volume 26, Numéro 1, 1990. <https://doi.org/10.1177/075910639002600103>
#'
#' @examples
#' \dontrun{
#' library(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' dtm <- dfm(corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
#' dtm <- dfm_wordstem(dtm, language = "english")
#' dtm <- dfm_trim(dtm, min_termfreq = 3)
#' res <- rainette(dtm, k = 3)
#' }
#' 
#' @export

rainette <- function(dtm, k = 10, min_uc_size = 10, min_split_members = 5, cc_test = 0.3, tsj = 3, min_members, ...) {
  
  ## Check for deprecated min_members argument
  if (!missing(min_members)) {
    warning("min_members is deprecated. Use min_split_members instead.")
    if (missing(min_split_members)) {
      min_split_members <- min_members
    }
  }
  
  if (any(dtm@x > 1)) {
    ## We don't use dfm_weight here because of https://github.com/quanteda/quanteda/issues/1545
    dtm@x <- as.numeric(dtm@x > 0)
  }
  
  ## Compute uc ids based on minimum size
  message("  Computing ucs from segments...")
  dtm <- compute_uc(dtm, min_uc_size = min_uc_size)
  ## Correspondance table between uce and uc
  corresp_uce_uc <- data.frame(uce = docvars(dtm)$rainette_uce_id, uc = docvars(dtm)$rainette_uc_id)
  ## Group dfm by uc
  dtm <- dfm_group(dtm, docvars(dtm)$rainette_uc_id)
  dtm <- dfm_weight(dtm, scheme = "boolean")
  
  message("  Clustering...")
  ## Initialize results list with first dtm
  res <- list(list(tabs = list(dtm)))
  
  ## Display progress bar
  progressr::with_progress({
    p <- progressr::progressor(along = seq_len(k - 1))
    
    for (i in 1:(k - 1)) {
      
      ## Split the biggest group
      biggest_group <- which.max(purrr::map(res[[i]]$tabs, nrow))
      if (nrow(res[[i]]$tabs[[biggest_group]]) < min_split_members) {
        message("! No more group bigger than min_split_members. Stopping after iteration ", i, ".")
        k <- i
        break
      }
      tab <- res[[i]]$tabs[[biggest_group]]
      ## textmodel_ca only works if nrow >= 3 and ncol >= 3
      if (nrow(tab) < 3 || ncol(tab) < 3) {
        message("! Tab to be splitted is not big enough. Stopping after iteration ", i, ".")
        k <- i
        break
      }
      clusters <- cluster_tab(tab, cc_test = cc_test, tsj = tsj,...)
      p()

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
    }
  })
  
  if (k == 1) {
    message("! No computed clusters. Returning NULL.")
    return(NULL)
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
  
  message("  Done.")

  ## Compute and return hclust-class result
  hres <- list(method = "reinert",
              call = match.call(),
              height = cumsum(rev(purrr::map_dbl(res, ~.x$height))),
              order = 1:k,
              labels = as.character(1:k),
              merge = merge,
              group = group,
              uce_groups = uce_groups)

  class(hres) <- c("rainette", "hclust")
  hres
}


#' return documents indices ordered by CA first axis coordinates
#'
#' @param dtm dmt on which to compute the CA and order documents
#' 
#' @details
#' Internal function, not to be used directly
#' 
#' @return ordered list of document indices
#' @importFrom Matrix tcrossprod

docs_order_by_ca <- function(dtm) {
  
  ## Compute first factor of CA on DTM
  ## Code taken from getAnywhere(textmodel_ca.dfm)
  P <- dtm / sum(dtm)
  rm <- rowSums(P)
  cm <- colSums(P)
  eP <- Matrix::tcrossprod(rm, cm)
  S <- (P - eP)/sqrt(eP)
  dec <- RSpectra::svds(S, 1)
  coord <- dec$u[, 1]/sqrt(rm)
  
  ## Order documents by their first factor coordinates
  indices <- (seq_along(coord))[order(coord)]
  
  return(indices)
}


#' Split a dtm in two according to the chi-square value between the two aggregated tables
#'
#' @param tab dtm to be split
#' @param indices documents indices ordered by CA first axis coordinate
#' 
#' @details
#' Internal function, not to be used directly
#'
#' @return a list with the index of the document where the split generates a maximum chi-square,
#' and the corresponding max chi-square value

split_tab_by_chisq <- function(tab, indices) {
  
  ## Precompute row sums et total
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
  
  return(list(max_index = max_index,
              max_chisq = max_chisq))
}



#' Switch documents between two groups to maximize chi-square value
#'
#' @param tab original dtm
#' @param indices documents indices orderes by first CA axis coordinates
#' @param max_index document index where the split is maximum, computed with [split_tab_by_chisq()]
#' @param max_chisq maximum chi-square value computed by [split_tab_by_chisq()]
#' 
#' @details
#' Internal function, not to be used directly
#'
#' @return a list of two vectors `indices1` and `indices2`, which contain 
#' the documents indices of each group after documents switching, and a `chisq` value,
#' the new corresponding chi-square value after switching

switch_docs_by_chisq <- function(tab, indices, max_index, max_chisq) {

  ## Group indices and tabs  
  group1 <- indices[1:which(indices == max_index)]
  group2 <- indices[(which(indices == max_index) + 1):length(indices)]
  tab1 <- rowSums(tab[,group1, drop = FALSE])
  tab2 <- rowSums(tab[,group2, drop = FALSE])
  row_sum <- rowSums(tab)
  total <- sum(tab)
  chisq <- max_chisq
  switched <- 1

  ## Run while points are switched
  while(switched > 0) {
    
    switched <- 0

    ## For each point
    for (index in indices) {
      index_1 <- index %in% group1
      ## Switch current element
      if (index_1) {
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
        if (index_1) {
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
  
  return(list(indices1 = group1, 
    indices2 = group2, 
    chisq = chisq))
}


#' Remove features from dtm of each group base don cc_test and tsj
#'
#' @param tab global dtm
#' @param indices1 indices of documents of group 1
#' @param indices2 indices of documents of group 2
#' @param cc_test maximum contingency coefficient value for the 
#' feature to be kept in both groups. 
#' @param tsj minimum feature frequency in the dtm
#' 
#' @details
#' Internal function, not to be used directly
#'
#' @return a list of two character vectors : `cols1` is the name of features to 
#' keep in group 1, `cols2` the name of features to keep in group 2

features_selection <- function(tab, indices1, indices2, cc_test = 0.3, tsj = 3) {
  
  ## features count for each group
  tab1 <- rowSums(tab[, indices1, drop = FALSE])
  tab2 <- rowSums(tab[, indices2, drop = FALSE])
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
  ## C contingency coefficient, sqrt(khi2 / (khi2 + N))
  feat_cc <- sqrt(feat_chisq / (feat_chisq + rowSums(observed)))

  ## Features selection
  cols1 <- character()
  cols2 <- character()
  
  for (i in seq_along(feat_cc)) {
    cc <- feat_cc[i]
    name <- names(feat_cc)[i]
    ## Keep feature if cc <= cc_test and frequency > 0
    if (cc <= cc_test && tab1[i] >= tsj) {
      cols1 <- c(cols1, name)
    }
    if (cc <= cc_test && tab2[i] >= tsj) {
      cols2 <- c(cols2, name)
    }
    ## If cc > cc_test, only keep feature in the group
    ## where observed frequency > expected frequency
    if (cc > cc_test) {
      if (tab1[i] > expected[i, 1] && tab1[i] >= tsj) {
        cols1 <- c(cols1, name)
      }
      if (tab2[i] > expected[i, 2] && tab2[i] >= tsj) {
        cols2 <- c(cols2, name)
      }
    }
  }
  
  return(list(cols1 = cols1, 
              cols2 = cols2))
}


#' Split a dtm into two clusters with reinert algorithm
#' 
#' @param dtm to be split, passed by `rainette`
#' @param cc_test maximum contingency coefficient value for the 
#' feature to be kept in both groups. 
#' @param tsj minimum feature frequency in the dtm
#' @param ... arguments passed to other methods
#' 
#' @details
#' Internal function, not to be used directly
#' 
#' @return
#' An object of class `hclust` and `rainette`


cluster_tab <- function(dtm, cc_test = 0.3, tsj = 3, ...) {
  
  ## First step : CA partition

  ## Remove documents with zero terms
  rs <- rowSums(dtm) > 0
  dtm <- dtm[rs,]
    
  indices <- docs_order_by_ca(dtm)
  
  ## Transpose and convert DTM to ease computations
  tab <- dtm %>% 
    convert(to = "data.frame")
  tab <- tab[, -1] %>% 
      t %>% 
      as.data.frame
 
  res <- split_tab_by_chisq(tab, indices)
  max_index <- res$max_index
  max_chisq <- res$max_chisq

  ## Second step : switching docs
  
  res <- switch_docs_by_chisq(tab, indices, max_index, max_chisq)
  indices1 <- res$indices1
  indices2 <- res$indices2
  chisq <- res$chisq
  
  ## Third step : features selection
  
  res <- features_selection(tab, indices1, indices2, cc_test, tsj)
  cols1 <- res$cols1
  cols2 <- res$cols2
  
  return(list(groups = list(docvars(dtm)$rainette_uc_id[indices1],
                            docvars(dtm)$rainette_uc_id[indices2]), 
              tabs = list(dtm[indices1, cols1], 
                          dtm[indices2, cols2]), 
              height = chisq))
}
