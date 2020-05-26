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
#'
#' @details
#' See the references for original articles on the method. Computations and results may differ
#' quite a bit, see the package vignettes for more details.
#' 
#' The dtm object is automatically converted to boolean.
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
#' \donttest{
#' require(quanteda)
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

rainette <- function(dtm, k = 10, min_uc_size = 10, min_split_members = 5, cc_test = 0.3, tsj = 3, min_members) {
  
  ## Check for deprecated min_members argument
  if (!missing(min_members)) {
    warning("min_members is deprecated. Use min_split_members instead.")
    if (missing(min_split_members)) {
      min_split_members <- min_members
    }
  }
  
  if (any(!(dtm@x %in% c(0, 1)))) {
    ## We don't use dfm_weight here because of https://github.com/quanteda/quanteda/issues/1545
    dtm@x <- as.numeric(dtm@x != 0)
  }
  
  ## Compute uc ids based on minimum size
  message("  Computing ucs from segments...")
  dtm <- compute_uc(dtm, min_uc_size = min_uc_size)
  ## Correspondance table between uce and uc
  corresp_uce_uc <- data.frame(
    uce = docvars(dtm, "rainette_uce_id"), 
    uc = docvars(dtm, "rainette_uc_id")
  )
  ## Group dfm by uc
  dtm <- dfm_group(dtm, docvars(dtm, "rainette_uc_id"))
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

      clusters <- cluster_tab(tab, cc_test = cc_test, tsj = tsj)
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
#' @param m dtm on which to compute the CA and order documents, converted to an integer matrix.
#' 
#' @details
#' Internal function, not to be used directly
#' 
#' @return ordered list of document indices

order_docs <- function(m) {
  
  ## Compute first factor of CA on DTM
  ## Code taken from getAnywhere(textmodel_ca.dfm)
  P <- m / sum(m)
  rm <- rowSums(P)
  cm <- colSums(P)
  eP <- tcrossprod(rm, cm)
  S <- (P - eP)/sqrt(eP)
  dec <- RSpectra::svds(S, 1)
  coord <- dec$u[, 1]/sqrt(rm)
  
  ## Order documents by their first factor coordinates
  indices <- (seq_along(coord))[order(coord)]
  
  return(indices)
}


#' Switch documents between two groups to maximize chi-square value
#'
#' @param m original dtm
#' @param indices documents indices orderes by first CA axis coordinates
#' @param max_index document index where the split is maximum
#' @param max_chisq maximum chi-square value 
#' 
#' @details
#' Internal function, not to be used directly
#'
#' @return a list of two vectors `indices1` and `indices2`, which contain 
#' the documents indices of each group after documents switching, and a `chisq` value,
#' the new corresponding chi-square value after switching

switch_docs <- function(m, indices, max_index, max_chisq) {

  ## Group indices and tabs  
  group1 <- indices[1:which(indices == max_index)]
  group2 <- indices[(which(indices == max_index) + 1):length(indices)]

  switched <- TRUE

  ## Run while points are switched
  while(switched) {

    switched <- FALSE
    
    tab1 <- m[group1, , drop = FALSE]
    tab2 <- m[group2, , drop = FALSE]

    chisq_values <- cpp_switch_docs(tab1, tab2)
    current_max <- max(chisq_values, na.rm = TRUE)
    
    if (current_max > max_chisq) {
      switched <- TRUE
      to_switch <- indices[which.max(chisq_values)]
      if (to_switch %in% group1) {
        group1 <- group1[group1 != to_switch]
        group2 <- c(group2, to_switch)
      } else {
        group2 <- group2[group2 != to_switch]
        group1 <- c(group1, to_switch)
      }
      max_chisq <- current_max
    }
  }
  
  return(list(indices1 = group1, 
    indices2 = group2, 
    chisq = max_chisq))
}


#' Remove features from dtm of each group base don cc_test and tsj
#'
#' @param m global dtm
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

select_features <- function(m, indices1, indices2, cc_test = 0.3, tsj = 3) {
  
  ## features count for each group
  tab1 <- colSums(m[indices1, , drop = FALSE])
  tab2 <- colSums(m[indices2, , drop = FALSE])
  ## Total number of features in each group
  nfeat_group1 <- sum(tab1)
  nfeat_group2 <- sum(tab2)
  ## Observed frequency of features
  observed <- rbind(tab1, tab2)
  ## Expected frequency of features
  expected_prop <- (tab1 + tab2) / sum(tab1 + tab2)
  expected <- rbind(expected_prop * nfeat_group1, expected_prop * nfeat_group2)
  ## Chi2 and contingency coefficients for each feature
  feat_chisq <- colSums((observed - expected)^2 / expected)
  ## C contingency coefficient, sqrt(khi2 / (khi2 + N))
  feat_cc <- sqrt(feat_chisq / (feat_chisq + colSums(observed)))

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
      if (tab1[i] > expected[1, i] && tab1[i] >= tsj) {
        cols1 <- c(cols1, name)
      }
      if (tab2[i] > expected[2, i] && tab2[i] >= tsj) {
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
#' 
#' @details
#' Internal function, not to be used directly
#' 
#' @return
#' An object of class `hclust` and `rainette`


cluster_tab <- function(dtm, cc_test = 0.3, tsj = 3) {

  ## Remove documents and features with zero occurrences
  dtm <- dtm[rowSums(dtm) > 0, colSums(dtm) > 0]
  ## Remove features with zero 
  m <- convert(dtm, to = "matrix")
  storage.mode(m) <- "integer"
  
  ## First step : CA partition

  indices <- order_docs(m)
  res <- cpp_split_tab(m, indices)
  max_index <- res$max_index
  max_chisq <- res$max_chisq

  ## Second step : switching docs
  
  res <- switch_docs(m, indices, max_index, max_chisq)
  indices1 <- res$indices1
  indices2 <- res$indices2
  chisq <- res$chisq

  ## Third step : features selection
  
  res <- select_features(m, indices1, indices2, cc_test, tsj)
  cols1 <- res$cols1
  cols2 <- res$cols2
  
  return(list(groups = list(docvars(dtm, "rainette_uc_id")[indices1],
                            docvars(dtm, "rainette_uc_id")[indices2]), 
              tabs = list(dtm[indices1, cols1], 
                          dtm[indices2, cols2]), 
              height = chisq))
}
