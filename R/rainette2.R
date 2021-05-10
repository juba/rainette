
## Compute signed Khi2 statistic from different sizes

compute_chi2 <- function(n_both, n1, n2, n_tot) {
  tab <- matrix(
    c(n_both, n1 - n_both, n2 - n_both, n_tot - n1 - n2 + n_both), 
    nrow = 2
  )
  suppressWarnings(cs <- stats::chisq.test(tab))
  res <- cs$statistic
  if (cs$expected[1, 1] > n_both) {
    res <- -res
  }
  return(res)
}

## Compute data frame of groups at each k for a clustering

get_groups <- function(res) {
  groups <- purrr::imap_dfc(res$uce_groups, ~ {
    v <- data.frame(paste(.y, .x, sep = "."), stringsAsFactors = FALSE)
    colnames(v) <- .y
    v
  })
  colnames(groups) <- seq_along(groups)
  return(groups)
}

## Compute size and chi2 for all combinations of two classes form two
## clustering results

classes_crosstab <- function(groups1, groups2, n_tot) {
  purrr::imap_dfr(groups1, function(g1, i1) {
    purrr::imap_dfr(groups2, function(g2, i2) {
      df <- dplyr::tibble(g1, g2)
      df %>%
        count(g1, g2) %>%
        tidyr::complete(g1, g2, fill = list(n = 0)) %>%
        rename(n_both = n) %>%
        right_join(df %>% count(g1), by = "g1") %>%
        rename(n1 = n) %>%
        right_join(df %>% count(g2), by = "g2") %>%
        rename(n2 = n) %>%
        mutate(level1 = i1, level2 = i2,
          chi2 = purrr::pmap_dbl(list(n_both, n1, n2, n_tot), compute_chi2)) %>%
        ungroup()
    })
  })
}

## Filter intersection classes on size and Khi2 value, and
## add their members

filter_crosstab <- function(tab, groups1, groups2, min_members, min_chi2) {

  ## Return members of an intersection class
  compute_members <- function(level1, g1, level2, g2) {
    purrr::pmap(list(level1, g1, level2, g2), function(level1, g1, level2, g2) {
      which(groups1[[level1]] == g1 & groups2[[level2]] == g2)
    })
  }

  ## Filter intersection classes on size and Khi2 value, and add members
  tab %>%
    filter(chi2 > min_chi2, n_both > min_members) %>%
    select(g1, g2, level1, level2, n_both, chi2) %>%
    mutate(interclass = paste(g1, g2, sep = "x"),
      members = compute_members(level1, g1, level2, g2)) %>%
    filter(!duplicated(members))

}

## Compute size of each pairs of intersection classes. Lower triangle
## and diagonal at 1 not to be selected as a partition afterward.

cross_sizes <- function(crosstab) {
  sizes <- matrix(1, nrow = nrow(crosstab), ncol = nrow(crosstab),
    dimnames = list(crosstab$interclass, crosstab$interclass))
  for (i in 1:(nrow(crosstab) - 1)) {
    for (j in (i + 1):nrow(crosstab)) {
      sizes[i , j] <- length(
        intersect(crosstab$members[[i]], crosstab$members[[j]])
      )
    }
  }
  sizes
}

## Compute next level partitions

next_partitions <- function(partitions, sizes) {

  k <- length(partitions)
  interclasses <- partitions[[1]]

  ## for each previous partition
  res <- lapply(partitions[[k]], function(partition) {
    size_inter <- sizes[partition, ]
    if (k > 1) {
      size_inter <- colSums(size_inter)
    }
    ## add new class if intersection is empty
    classes_ok <- which(size_inter == 0)
    lapply(interclasses[classes_ok], function(x) c(partition, x))
  })
  res <- res[lapply(res, length) > 0]

  if (length(res) == 0) {
    return(NULL)
  }

  do.call(c, res)
}

## From computed partitions, filter out the optimal ones and add group
## membership

get_optimal_partitions <- function(partitions, valid, n_tot) {

  ## Compute group memberships from a vector of clusters
  compute_groups <- function(clusters) {
    clusters <- unlist(clusters)
    groups <- rep(NA, n_tot)
    for (i in seq_along(clusters)) {
      members <- unlist(valid$members[valid$interclass == clusters[i]])
      groups[members] <- i
    }
    groups
  }

  ## Compute data frame of results
  res <- purrr::imap_dfr(partitions, function(partitions, k) {
    if (is.null(partitions)) {
      return(NULL)
    }
    dplyr::tibble(clusters = partitions, k = k + 1) %>%
      ## Compute size and sum of Khi2 for each partition
      mutate(
        chi2 = purrr::map_dbl(clusters, ~sum(valid$chi2[valid$interclass %in% .x])),
        n = purrr::map_dbl(clusters, ~sum(valid$n_both[valid$interclass %in% .x]))
      ) %>%
      ## Filter partitions with max size or max chi2 for each k
      group_by(k) %>%
      filter(n == max(n) | chi2 == max(chi2)) %>%
      group_by(k, n) %>%
      filter(chi2 == max(chi2)) %>%
      group_by(k, chi2) %>%
      filter(n == max(n))
  })

  ## Add group membership for each clustering
  res %>%
    mutate(groups = purrr::map(clusters, compute_groups)) %>%
    ungroup
}


#' Corpus clustering based on the Reinert method - Double clustering
#'
#' @param x either a quanteda dfm object or the result of [rainette()]
#' @param y if `x` is a [rainette()] result, this must be another [rainette()] 
#'   result from same dfm but with different uc size.
#' @param max_k maximum number of clusters to compute
#' @param uc_size1 if `x` is a dfm, minimum uc size for first clustering
#' @param uc_size2 if `x` is a dfm, minimum uc size for second clustering
#' @param min_members minimum members of each cluster
#' @param min_chi2 minimum chi2 for each cluster
#' @param ... if `x` is a dfm object, parameters passed to [rainette()] for both
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
#' @seealso [rainette()], [cutree_rainette2()], [rainette2_plot()], [rainette2_explor()]
#'
#' @references
#'
#' - Reinert M, Une méthode de classification descendante hiérarchique : application à l'analyse lexicale par contexte, Cahiers de l'analyse des données, Volume 8, Numéro 2, 1983. <http://www.numdam.org/item/?id=CAD_1983__8_2_187_0>
#' - Reinert M., Alceste une méthodologie d'analyse des données textuelles et une application: Aurelia De Gerard De Nerval, Bulletin de Méthodologie Sociologique, Volume 26, Numéro 1, 1990.  \doi{10.1177/075910639002600103}
#'
#' @export
#'
#' @examples
#' \donttest{
#' require(quanteda)
#' mini_corpus <- head(data_corpus_inaugural, n = 2)
#' mini_corpus <- split_segments(mini_corpus, 5)
#' dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
#' dtm <- dfm_wordstem(dtm, language = "english")
#' dtm <- dfm_trim(dtm, min_termfreq = 3)
#'
#' res1 <- rainette(dtm, k = 5, min_uc_size = 2, min_split_members = 2)
#' res2 <- rainette(dtm, k = 5, min_uc_size = 3, min_split_members = 2)
#'
#' res <- rainette2(res1, res2, min_members = 2)
#' }


rainette2 <- function(x, y = NULL, max_k = 5, uc_size1 = 10, uc_size2 = 15,
  min_members = 10, min_chi2 = 3.84, ...) {

  ## If passed a dfm, compute both clustering
  if (inherits(x, "dfm")) {
    dtm <- x
    message("  Computing first clustering with uc_size1 = ", uc_size1)
    x <- rainette::rainette(
      dtm, k = max_k, min_uc_size = uc_size1,
      min_split_members = min_members, ...
    )
    message("  Computing second clustering with uc_size2 = ", uc_size2)
    y <- rainette::rainette(
      dtm, k = max_k, min_uc_size = uc_size2,
      min_split_members = min_members, ...
    )
  }

  ## max_k should not be higher than the smallest k in both clustering
  max_k_res <- min(max(x$group, na.rm = TRUE), max(y$group, na.rm = TRUE))
  if (max_k_res < max_k) {
    message("! Setting max_k from ", max_k, " to ", max_k_res)
    max_k <- max_k_res
  }

  ## Progress bar
  message("  Searching for best partitions...")
  pb_max <- max_k + 4

  progressr::with_progress({
    p <- progressr::progressor(along = seq_len(pb_max))

    ## Compute data frame of groups at each k for both clusterings
    groups1 <- get_groups(x)
    groups2 <- get_groups(y)
    ## Total number of documents
    n_tot <- nrow(groups1)

    ## Compute sizes and chi2 of every crossing between classes
    ## of both clusterings (intersection classes)
    cross_classes <- classes_crosstab(groups1, groups2, n_tot)

    ## Filter intersection classes on size and Khi2 value, and add members
    valid <- filter_crosstab(
      cross_classes, groups1, groups2, min_members, min_chi2
    )

    if (nrow(valid) < 2) {
      stop("Not enough valid classes to continue. You may try a lower min_members value.")
    }

    ## Matrix of sizes of intersection classes crossing
    sizes <- cross_sizes(valid)
    p()

    ## Compute partitions
    partitions <- list()
    interclasses <- valid$interclass
    partitions[[1]] <- interclasses
    for (k in 2:max_k) {
      part <- next_partitions(partitions, sizes)
      if(!is.null(part)) {
        partitions[[k]] <- part
        p()
      } else {
        p()
        message("! No more partitions found, stopping at k=", k - 1)
        break;
      }
    }

    partitions[[1]] <- NULL

    ## Select opimal partitions and add group membership for each one
    res <- get_optimal_partitions(partitions, valid, n_tot)

  })

  message("  Done.")

  class(res) <- c("rainette2", class(res))

  res
}

