## Generate a "terms plot", ie terms keyness for a group

terms_plot <- function(tab, xlim = NULL, title = "", title_color = "firebrick3", stat_col = "chi2") {
  
  ## Column with statistic values
  stat_col <- rlang::sym(stat_col)
  ## Plot
  g <- ggplot(data = tab, aes(x = stats::reorder(feature, !!stat_col), y = !!stat_col, fill = sign)) + 
    geom_col(width = .7) + 
    geom_hline(yintercept = 0, color = "grey70") +
    coord_flip() + 
    scale_fill_manual("", guide = FALSE, 
      values = c("positive" = "#377eb8", "negative" = "#e41a1c")) +
    labs(title = title, x = "") +
    theme_minimal() + 
    theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5, colour = title_color),
      axis.title.x = element_text(size = 7),
      axis.text.x = element_text(size = 7),
      plot.margin = grid::unit(c(0,0.05,0,0), "npc"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = grDevices::rgb(.9,.9,.9,.3), 
        colour = "transparent"))
  ## Fix x limits if necessary
  if (!is.null(xlim)) {
    g <- g + scale_y_continuous(limits = xlim)
  }
  
  ## Align title element to the left to center it with hjust
  g <- ggplotGrob(g)
  g$layout$l[g$layout$name == "title"] <- 1
  g
}




#' Generate a clustering description plot from a rainette result
#'
#' @param res result object of a `rainette` clustering
#' @param dtm the dfm object used to compute the clustering
#' @param k number of groups. If NULL, use the biggest number possible.
#' @param n_terms number of terms to display in keyness plots
#' @param free_x if TRUE, all the keyness plots will have the same x axis
#' @param measure statistics to compute
#'
#' @seealso `quanteda::textstat_keyness`
#'
#' @export
#' @examples 
#' \dontrun{
#' library(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' dtm <- dfm(corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
#' dtm <- dfm_trim(dtm, min_termfreq = 3)
#' res <- rainette(dtm, k = 3)
#' rainette_plot(dtm, res)
#' }
#' 
#' @import ggplot2
#' @import dendextend

rainette_plot <- function(res, dtm, k = NULL, n_terms = 15, free_x = FALSE, measure = c("chi2", "lr")) {
  
  measure <- match.arg(measure)
  stat_col <- switch(measure,
    "chi2" = "chi2",
    "lr" = "G2"
  )
  stat_col <- rlang::sym(stat_col)
  
  if (is.null(k)) {
    groups <- res$group
    k <- max(groups)
  } else {
    groups <- cutree.rainette(res, k)
  }
  
  ## Compute and filter keyness statistics
  tabs <- purrr::map(sort(unique(groups)), function(group) {
    quanteda::textstat_keyness(dtm, groups == group, measure = measure) %>% 
      arrange(desc(abs(!!stat_col))) %>% 
      filter(p < 0.05) %>% 
      slice(1:n_terms) %>% 
      mutate(sign = if_else(!!stat_col > 0, "positive", "negative"),
             sign = factor(sign, levels = c("positive", "negative")))
  })
  
  ## Min and max statistics to fix x axis in terms plots
  xlim <- NULL
  if (!free_x) {
    min_stat <- min(purrr::map_dbl(tabs, ~ min(.x %>% pull(!!stat_col))))
    max_stat <- max(purrr::map_dbl(tabs, ~ max(.x %>% pull(!!stat_col))))
    xlim <- c(min_stat, max_stat)
  }
  ## Frequency and proportion of each cluster
  clust_n <- table(groups)
  clust_prop <- round(clust_n / sum(clust_n) * 100, 1)
  ## Graph layout
  lay <- matrix(c(rep(1, k), rep(2:(k+1), 2)), nrow = 3, ncol = k, byrow = TRUE)
  plots <- list()
  
  ## Groups colors
  if (k <=9) {
    groups_colors <- RColorBrewer::brewer.pal(k, "Set1")
  } else if (k <= 12) {
    groups_colors <- RColorBrewer::brewer.pal(k, "Paired")
  } else {
    groups_colors <- rep("firebrick3", k)
  }
  
  ## Add dendrogram
  dend <- as.dendrogram(res)
  labels_colors(dend) <- groups_colors
  dend <- dend %>% 
    color_branches(k = k, col = groups_colors) %>% 
    set("branches_lwd", 0.5)
  dend <- as.ggdend(dend)
  g <- ggplot(dend) + scale_y_continuous(breaks = NULL) +
      theme(plot.margin = grid::unit(c(0.01,0.05,0.01,0.05), "npc"))
  plots[[1]] <- g
  
  ## Add terms plots
  for (i in 1:k) {
    title <- paste0("n = ", clust_n[i], "\n", clust_prop[i], "%")
    plots[[i+1]] <- terms_plot(tabs[[i]], xlim, 
      title = title, title_color = groups_colors[i], stat_col = stat_col)
  }
  ## Generate grid
  gridExtra::grid.arrange(grobs = plots, layout_matrix = lay)
  
}
