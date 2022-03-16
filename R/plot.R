## Generate a "terms bar plot", based on terms keyness for a group

keyness_barplot <- function(tab, range = NULL, title = "", title_color = "firebrick3",
                       stat_col = "chi2", n_terms = NULL, text_size = 10, top_margin = 0,
                       keyness_plot_xlab = NULL) {

  if (is.null(keyness_plot_xlab)) {
    keyness_plot_xlab <- stat_col
  }

  ## Column with statistic values
  if (!is.null(range)) {
    stat_max <- max(range)
  } else {
    stat_max <- max(tab[[stat_col]], na.rm = TRUE)
  }
  ## Plot
  g <- ggplot(data = tab, aes(x = stats::reorder(.data$feature, .data[[stat_col]]), y = abs(.data[[stat_col]]))) +
    geom_col(aes(fill = .data$sign), color = "white", width = 1) +
    geom_text(
      y = stat_max / 15,
      aes(label = stats::reorder(.data$feature, .data[[stat_col]])),
      hjust = 0, size = text_size / 2.5
    ) +
    coord_flip() +
    scale_fill_manual("", values = c("positive" = "#a1d8ff", "negative" = "#ff7d7e")) +
    guides(fill = "none") +
    labs(title = title, x = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = text_size, face = "bold", hjust = 0.5, colour = title_color),
      axis.title.x = element_text(size = text_size * 0.8),
      plot.margin = grid::unit(c(top_margin, 0.05, 0, 0), "npc"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = grDevices::rgb(.9, .9, .9, .2),
        colour = "transparent"))
  ## Fix x limits if necessary and remove horizontal axis values
  if (!is.null(range)) {
    g <- g + ggplot2::scale_y_continuous(keyness_plot_xlab, limits = range, breaks = NULL)
  } else {
    g <- g + ggplot2::scale_y_continuous(keyness_plot_xlab, breaks = NULL)
  }
  ## Adjust vertical scale if necessary
  if (nrow(tab) < n_terms) {
    limits <- levels(stats::reorder(tab$feature, tab[[stat_col]]))
    limits <- c(rep("", n_terms - length(limits)), limits)
    g <- g + ggplot2::scale_x_discrete(limits = limits, breaks = NULL)
  } else {
    g <- g + ggplot2::scale_x_discrete(breaks = NULL)
  }

  ## Align title element to the left to center it with hjust
  g <- ggplot2::ggplotGrob(g)
  g$layout$l[g$layout$name == "title"] <- 1
  g
}


## Generate a "terms wordcloud plot", based on terms keyness for a group

#' @import ggwordcloud

keyness_worcloud <- function(tab, range = NULL, title = "", title_color = "firebrick3",
  stat_col = "chi2", max_size = 15, top_margin = 0) {

  ## Plot
  g <- ggplot(data = tab) +
    #geom_hline(yintercept = 0, color = "grey70") +
    ggwordcloud::geom_text_wordcloud(
      aes(label = .data$feature, size = .data[[stat_col]]), color = title_color
    ) +
    labs(title = title) +
    scale_x_continuous(stat_col) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, colour = title_color),
      plot.margin = grid::unit(c(top_margin, 0.05, 0, 0), "npc"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = grDevices::rgb(.9, .9, .9, .3),
        colour = "transparent"))
  ## Fix x limits if necessary and remove horizontal axis values
  if (!is.null(range)) {
    g <- g + ggplot2::scale_size_area(limits = range, max_size = max_size)
  } else {
    g <- g + ggplot2::scale_size_area(max_size = max_size)
  }

  ## Align title element to the left to center it with hjust
  g <- ggplot2::ggplotGrob(g)
  g$layout$l[g$layout$name == "title"] <- 1
  g
}



## Returns a color palette or an individual group color depending on the number of groups

groups_colors <- function(k, i = NULL) {
  ## Groups colors
  ## This is the result of the following call, put manually here
  ## for compatibility with R 3.6
  ## col <- grDevices::palette.colors(n = 10, palette = "Tableau 10")
  col <- c(
    "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  )
  col <- rep_len(col, k)

  if (!is.null(i)) {
    return(col[i])
  } else {
    return(col)
  }

}


## Generate a list of terms plots from a list of keyness statistic tables

keyness_plots <- function(tabs, groups, type = "bar",
  free_scales = FALSE, stat_col = "chi2", n_terms, text_size, top_margin = 0,
  cluster_label = NULL, keyness_plot_xlab = NULL) {

  ## Frequency and proportion of each cluster
  clust_n <- table(groups)
  clust_prop <- round(clust_n / sum(clust_n) * 100, 1)
  k <- length(tabs)

  ## Min and max statistics to fix x axis in terms plots
  if (stat_col == "docprop") {
    range <- c(0, 1)
  } else {
    range <- NULL
    if (!free_scales) {
      max_stat <- max(purrr::map_dbl(tabs, function(tab) {
         v <- tab[[stat_col]]
         if (length(v) == 0) return(0)
         max(v)
      }))
      range <- c(0, max_stat)
    }
  }

  purrr::map(1:k, function(i) {
    if (k <= 6) {
      label <- ifelse(is.null(cluster_label), "Cluster", cluster_label)
      title <- paste0(label, " ", i, "\nn = ", clust_n[i], " (", clust_prop[i], "%)")
    } else if (k <= 8) {
      label <- ifelse(is.null(cluster_label), "Cluster", cluster_label)
      title <- paste0(label, " ", i, "\nn = ", clust_n[i])
    } else {
      label <- ifelse(is.null(cluster_label), "Cl.", cluster_label)
      title <- paste0(label, " ", i, "\nn = ", clust_n[i])
    }
    if (type == "bar") {
      if (is.null(text_size)) text_size <- 10
      keyness_barplot(tabs[[i]], range, title = title, title_color = groups_colors(k, i),
               stat_col = stat_col, n_terms, text_size = text_size, top_margin,
               keyness_plot_xlab = keyness_plot_xlab)
    } else {
      if (is.null(text_size)) text_size <- 15
      keyness_worcloud(tabs[[i]], range, title = title, title_color = groups_colors(k, i),
        stat_col = stat_col, max_size = text_size, top_margin)
    }
  })
}

# Generate the dendrogram plot for rainette_plot()

dendrogram_plot <- function(res, k, groups, text_size, show_na_title) {

  dend <- stats::as.dendrogram(res)
  max_k <- max(res$group, na.rm = TRUE)

  ## Cut the dendrogram if necessary
  if (k < max_k) {
    dend <- cut(dend, res$height[max_k - k])$upper
    ## Double conversion to "balance" the dendrogram
    dend <- stats::as.dendrogram(stats::as.hclust(dend))
    dend <- dendextend::set(dend, "labels", 1:k)
  }

  ## Style labels and branches
  dendextend::labels_colors(dend) <- groups_colors(k)
  dend <- dend %>%
    dendextend::color_branches(k = k, col = groups_colors(k)) %>%
    dendextend::set("branches_lwd", 0.4)

  ## Generate plot
  dend <- dendextend::as.ggdend(dend)
  margin <- ifelse(k >= 7, 0, 0.175 - k * 0.025)
  title_size <- ifelse(is.null(text_size), 10, text_size)
  g <- ggplot(dend, nodes = FALSE) +
    scale_y_continuous(breaks = NULL) +
    theme(plot.margin = grid::unit(c(0.05, margin, 0, margin), "npc"),
          plot.title = element_text(hjust = 0.5, size = title_size))

  ## Add NA number and proportion as title
  if (show_na_title) {
    ## Compute number of NA
    na_n <- sum(is.na(groups))
    na_prop <- round(na_n / length(groups) * 100, 1)
    g <- g +
        ggtitle(paste0("NA : ", na_n, " (", na_prop, "%)"))
  }

  g
}


#' Generate a clustering description plot from a rainette result
#'
#' @param res result object of a `rainette` clustering
#' @param dtm the dfm object used to compute the clustering
#' @param k number of groups. If NULL, use the biggest number possible
#' @param type type of term plots : barplot or wordcloud
#' @param n_terms number of terms to display in keyness plots
#' @param free_scales if TRUE, all the keyness plots will have the same scale
#' @param measure statistics to compute
#' @param show_negative if TRUE, show negative keyness features
#' @param text_size font size for barplots, max word size for wordclouds
#' @param show_na_title if TRUE, show number of NA as plot title
#' @param cluster_label define a specific term for clusters identification in keyness plots.
#'   Default is "Cluster" or "Cl." depending on the number of groups.
#' @param keyness_plot_xlab define a specific x label for keyness plots.
#'
#' @seealso [quanteda.textstats::textstat_keyness()], [rainette_explor()], [rainette_stats()]
#'
#' @return
#' A gtable object.
#'
#' @export
#' @examples
#' \donttest{
#' require(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' tok <- tokens(corpus, remove_punct = TRUE)
#' tok <- tokens_remove(tok, stopwords("en"))
#' dtm <- dfm(tok, tolower = TRUE)
#' dtm <- dfm_trim(dtm, min_docfreq = 3)
#' res <- rainette(dtm, k = 3, min_segment_size = 15)
#' rainette_plot(res, dtm)
#' }
#'
#' @import ggplot2
#' @import dendextend

rainette_plot <- function(res, dtm, k = NULL,
                          type = c("bar", "cloud"), n_terms = 15,
                          free_scales = FALSE,
                          measure = c("chi2", "lr", "frequency", "docprop"),
                          show_negative = FALSE,
                          text_size = NULL,
                          show_na_title = TRUE,
                          cluster_label = NULL,
                          keyness_plot_xlab = NULL) {

  if (!inherits(res, "rainette"))
    stop("res must be a rainette result object")

  type <- match.arg(type)
  measure <- match.arg(measure)
  stat_col <- stat_col(measure)
  if (type == "cloud") {
    warning("wordcloud plots will soon be deprecated. Please use type = \"bar\" instead.")
    show_negative <- FALSE
  }

  ## Maximum number of clusters
  max_k <- max(res$group, na.rm = TRUE)

  ## Get groups
  if (is.null(k)) {
    groups <- res$group
    k <- max_k
  } else {
    if (k < 2 || k > max_k) stop("k must be between 2 and ", max_k)
    groups <- rainette::cutree_rainette(res, k)
  }

  ## Keyness statistics
  tabs <- rainette::rainette_stats(groups, dtm, measure, n_terms, show_negative)

  ## Graph layout
  lay <- matrix(
    c(rep(1, k),
    rep(2:(k + 1), 2)),
    nrow = 3, ncol = k, byrow = TRUE
  )
  plots <- list()

  ## Dendrogram
  plots[[1]] <- dendrogram_plot(res, k, groups, text_size, show_na_title)

  ## Add terms plots
  plots <- c(
    plots,
    keyness_plots(
      tabs, groups, type, free_scales,
      stat_col, n_terms, text_size,
      cluster_label = cluster_label,
      keyness_plot_xlab = keyness_plot_xlab
    )
  )

  ## Generate grid
  gridExtra::grid.arrange(grobs = plots, layout_matrix = lay)

}

## Generate barplot of cluster sizes

frequency_barplot <- function(groups, k, text_size) {

  # Compute cluster sizes and number of NA
  freq <- data.frame(table(groups, exclude = NULL))
  n_na <- sum(is.na(groups))
  title <- paste0("Clusters size\n(NA = ", n_na, ")")
  colnames(freq) <- c("Group", "n")

  # Generate barplot
  g <- ggplot(freq) +
    geom_col(aes(x = .data$Group, y = .data$n, fill = .data$Group)) +
    scale_fill_manual(values = c(groups_colors(k)), na.value = "grey20") +
    guides(fill = "none") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.margin = grid::unit(c(0.05, 0.05, 0, 0), "npc"),
      axis.title.x = element_text(size = text_size * 0.8),
      axis.title.y = element_text(size = text_size * 0.8))
}

#' Generate a clustering description plot from a rainette2 result
#'
#' @param res result object of a `rainette2` clustering
#' @param dtm the dfm object used to compute the clustering
#' @param k number of groups. If NULL, use the biggest number possible
#' @param criterion criterion to use to choose the best partition. `chi2` means
#'    the partition with the maximum sum of chi2, `n` the partition with the
#'    maximum size.
#' @param complete_groups if TRUE, documents with NA cluster are reaffected by
#'    k-means clustering initialised with current groups centers.
#' @param type type of term plots : barplot or wordcloud
#' @param n_terms number of terms to display in keyness plots
#' @param free_scales if TRUE, all the keyness plots will have the same scale
#' @param measure statistics to compute
#' @param show_negative if TRUE, show negative keyness features
#' @param text_size font size for barplots, max word size for wordclouds
#'
#' @return
#' A gtable object.
#'
#' @seealso [quanteda.textstats::textstat_keyness()], [rainette2_explor()], [rainette2_complete_groups()]
#'
#' @export
#'
#' @import ggplot2

rainette2_plot <- function(res, dtm, k = NULL, criterion = c("chi2", "n"),
  complete_groups = FALSE,
  type = c("bar", "cloud"), n_terms = 15,
  free_scales = FALSE, measure = c("chi2", "lr", "frequency", "docprop"),
  show_negative = FALSE,
  text_size = 10) {

  if (!inherits(res, "rainette2"))
    stop("res must be a rainette2 result object")

  type <- match.arg(type)
  measure <- match.arg(measure)
  criterion <- match.arg(criterion)
  stat_col <- stat_col(measure)

  if (type == "cloud") {
    warning("wordcloud plots will soon be deprecated. Please use type = \"bar\" instead.")
    show_negative <- FALSE
  }

  ## Stop if not full and criterion == "n"
  if ((is.null(attr(res, "full")) || !attr(res, "full")) && criterion != "chi2") {
    stop("if rainette2 has been computed with full=FALSE, only 'chi2' criterion is available")
  }

  ## Maximum number of clusters
  max_k <- max(res$k, na.rm = TRUE)

  ## Get groups
  if (is.null(k) || k < 2 || k > max_k) stop("k must be between 2 and ", max_k)
  groups <- rainette::cutree_rainette2(res, k, criterion)
  if (complete_groups) {
    groups <- rainette::rainette2_complete_groups(dtm, groups)
  }

  ## Keyness statistics
  tabs <- rainette::rainette_stats(groups, dtm, measure, n_terms, show_negative)

  plots <- list()

  ## Barplot of clusters size
  plots[[1]] <- frequency_barplot(groups, k, text_size)

  ## Add terms plots
  plots <- c(
    plots,
    keyness_plots(
      tabs, groups, type, free_scales,
      stat_col, n_terms, text_size, top_margin = 0.05
    )
  )

  ## Graph layout
  nrow <- ifelse(k > 5, 2, 1)
  index <- 1:(k + 1)
  if (k > 5 && k %% 2 == 0) {
    index <- c(index, NA)
  }
  lay <- matrix(index, nrow = nrow, byrow = TRUE)

  ## Generate grid
  gridExtra::grid.arrange(grobs = plots, layout_matrix = lay)

}
