
## Compute signed Khi2 statistic from different sizes

compute_chi2 <- function(n_both, n1, n2, n_tot) {
  tab <- matrix(c(n_both, n1 - n_both, n2 - n_both, n_tot - n1 - n2 + n_both), nrow = 2)
  suppressWarnings(cs <- chisq.test(tab))
  res <- cs$statistic
  if (cs$expected[1,1] > n_both) {
    res <- -res
  }
  return(res)
}

#' Corpus clustering based on the Reinert method - Double clustering
#'
#' @param x either a quanteda dfm object or the result of `rainette`
#' @param y if `x` is a `rainette` result, this must be another `rainette` from same dfm
#'   but with different uc size.
#' @param max_k maximum number of clusters to compute
#' @param uc_size1 if `x` is a dfm, minimum uc size for first clustering
#' @param uc_size2 if `x` is a dfm, minimum uc size for second clustering
#' @param min_members minimum members of each cluster
#' @param ... if `x` is a dfm object, parameters passed to `rainette` for both 
#'   simple clusterings
#'
#' @details
#' You can pass a quanteda dfm as `x` object, the function then performs two simple
#' clustering with varying minimum uc size, and then proceed to find optimal partitions
#' based on the results of both clusterings.
#' 
#' If both clusterings have already been computed, you can pass them as `x` and `y` arguments
#' and the function will only look for optimal partitions.
#' 
#' For more details on optimal partitions search algorithm, please see package vignettes.
#' 
#' @return 
#' A tibble with optimal partitions found for each available value of `k` as rows, and the following
#' columns :
#' 
#' - `clusters` list of the crossed original clusters used in the partition
#' - `k` the number of clusters
#' - `chi2` sum of the chi2 value of each cluster
#' - `n` sum of the size of each cluster
#' - `groups` group membership of each document for this partition (`NA` if not assigned)
#'
#' @seealso `rainette`, `cutree_rainette2`, `rainette2_plot`, `rainette2_explor`
#'
#' @references 
#' Reinert M, Une méthode de classification descendante hiérarchique : application à l'analyse lexicale par contexte, Cahiers de l'analyse des données, Volume 8, Numéro 2, 1983. [http://www.numdam.org/item/?id=CAD_1983__8_2_187_0](http://www.numdam.org/item/?id=CAD_1983__8_2_187_0)
#' 
#' Reinert M., Alceste une méthodologie d'analyse des données textuelles et une application: Aurelia De Gerard De Nerval, Bulletin de Méthodologie Sociologique, Volume 26, Numéro 1, 1990. [https://doi.org/10.1177/075910639002600103](https://doi.org/10.1177/075910639002600103)
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' mini_corpus <- head(data_corpus_inaugural, n = 2)
#' mini_corpus <- split_segments(mini_corpus, 5)
#' dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
#' dtm <- dfm_wordstem(dtm, language = "english")
#' dtm <- dfm_trim(dtm, min_termfreq = 3)
#' 
#' res1 <- rainette(dtm, k = 5, min_uc_size = 2, min_members = 2)
#' res2 <- rainette(dtm, k = 5, min_uc_size = 3, min_members = 2)
#' 
#' res <- rainette2(res1, res2, min_members = 2)
#' rainette2_explor(res, dtm)
#' }


rainette2 <- function(x, y = NULL, max_k = 5, uc_size1 = 10, uc_size2 = 15, min_members = 10, ...) {

  ## If passed a dfm, compute both clustering
  if (inherits(x, "dfm")) {
    dtm <- x
    message("  Computing first clustering with uc_size1 = ", uc_size1)
    x <- rainette(dtm, k = max_k, min_uc_size = uc_size1, min_members = min_members,...)
    message("  Computing second clustering with uc_size2 = ", uc_size2)
    y <- rainette(dtm, k = max_k, min_uc_size = uc_size2, min_members = min_members, ...)
  }
  
  ## max_k should not be higher than the smallest k in both clustering
  max_k_res <- min(max(x$group, na.rm = TRUE), max(y$group, na.rm = TRUE))
  if (max_k_res < max_k) {
    message("! Setting max_k from ", max_k, " to ", max_k_res)
    max_k <- max_k_res
  }
  
  ## Progress bar
  message("  Searching for best partitions")
  pb_max <- max_k + 4
  pb <- progress::progress_bar$new(total = pb_max,
    format = "  [:bar] :percent in :elapsed",
    clear = FALSE, show_after = 0)
  invisible(pb$tick(0))
  
  ## Compute data frame of groups at each k for both clusterings
  groups1 <- purrr::imap_dfc(x$uce_groups, ~ paste(.y, .x, sep="."))
  colnames(groups1) <- 1:ncol(groups1)
  groups2 <- purrr::imap_dfc(y$uce_groups, ~ paste(.y, .x, sep="."))
  colnames(groups2) <- 1:ncol(groups2)
  ## Total number of documents
  n_tot <- nrow(groups1)

  ## Compute sizes and Khi2 of every crossing between classes
  ## of both clusterings (intersection classes)
  cross_classes <- purrr::imap_dfr(groups1, function(g1, i1) {
    purrr::imap_dfr(groups2, function(g2, i2) {
      df <- tibble(g1, g2)
      counts <- df %>% 
        count(g1, g2) %>% 
        tidyr::complete(g1, g2, fill = list(n = 0)) %>% 
        rename(n_both = n) %>% 
        right_join(df %>% count(g1), by = "g1") %>% 
        rename(n1 = n) %>%
        right_join(df %>% count(g2), by = "g2") %>%
        rename(n2 = n) %>%
        rowwise() %>%
        mutate(level1 = i1, level2 = i2,
               chi2 = compute_chi2(n_both, n1, n2, n_tot)) %>%
        ungroup()
    })
  })

  ## Return members of an intersection class
  compute_members <- function(level1, g1, level2, g2) {
    pmap(list(level1, g1, level2, g2), function(level1, g1, level2, g2) {
      which(groups1[[level1]] == g1 & groups2[[level2]] == g2)
    })
  }
  
  ## Filter intersection classes on size and Khi2 value, and add members
  valid <- cross_classes %>% 
    filter(chi2 > 3.84, n_both > min_members) %>%
    select(g1, g2, level1, level2, n_both, chi2) %>% 
    mutate(interclass = paste(g1, g2, sep = "x"),
           members = compute_members(level1, g1, level2, g2)) %>% 
    filter(!duplicated(members))
  
  if (nrow(valid) < 2) {
    stop("Not enough valid classes to continue. You may try a lower min_members value.")
  }
  
  ## Matrix of sizes of intersection classes intersections
  cross_inter <- matrix(1, nrow = nrow(valid), ncol = nrow(valid),
                        dimnames = list(valid$interclass, valid$interclass))
  for (i in 1:(nrow(valid)-1)) {
    for (j in (i+1):nrow(valid)) {
      cross_inter[i , j] <- length(intersect(valid$members[[i]], valid$members[[j]]))
    }
  }
  pb$tick(1)
  
  ## Compute 2-class partitions
  interclasses <- valid$interclass
  partitions <- list()
  k <- 2
  res <- lapply(interclasses, function(class) {
    classes_ok <- which(cross_inter[class, ] == 0)
    lapply(interclasses[classes_ok], function(x) c(class, x))
  })
  res <- res[lapply(res, length) > 0]
  partitions[[k]] <- do.call(c, res)
  pb$tick(1)


  ## Compute higher order partitions
  for (k in 3:max_k) {
    res <- lapply(partitions[[k-1]], function(partition) {
      classes_ok <- which(colSums(cross_inter[partition, ]) == 0)
      lapply(interclasses[classes_ok], function(x) c(partition, x))
    })
    res <- res[lapply(res, length) > 0]
    if (length(res) == 0) {
      break;
    }
    partitions[[k]] <- do.call(c, res)
    pb$tick(1)
  }
  
  
  ## Compute data frame of results 
  res <- imap_dfr(partitions, function(partitions, k) {
    if (is.null(partitions)) {
      return(NULL)
    }
    tibble(clusters = partitions, k = k) %>% 
      rowwise %>% 
      ## Compute size and sum of Khi2 for each partition
      mutate(chi2 = sum(valid$chi2[valid$interclass %in% clusters]),
             n = sum(valid$n_both[valid$interclass %in% clusters])) %>% 
      ungroup %>% 
      ## Filter partitions with max size or max chi2 for each k
      group_by(k) %>% 
      filter(n == max(n) | chi2 == max(chi2)) %>% 
      group_by(k, n) %>% 
      filter(chi2 == max(chi2)) %>% 
      group_by(k, chi2) %>% 
      filter(n == max(n))
  })
  
  ## Compute group memberships from a vector of clusters
  compute_groups <- function(clusters) {
    clusters <- unlist(clusters)
    groups <- rep(NA, n_tot)
    for (i in seq_along(clusters)) {
      members <- unlist(valid$members[valid$interclass == clusters[i]])
      groups[members] <- i
    }
    list(groups)
  }
  
  ## Add group membership for each clustering
  res <- res %>%
    rowwise %>% 
    mutate(groups = compute_groups(clusters)) %>% 
    ungroup
  class(res) <- c("rainette2", class(res))

  pb$update(1)

  res
}

