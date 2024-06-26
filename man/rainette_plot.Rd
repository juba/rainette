% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{rainette_plot}
\alias{rainette_plot}
\title{Generate a clustering description plot from a rainette result}
\usage{
rainette_plot(
  res,
  dtm,
  k = NULL,
  type = c("bar", "cloud"),
  n_terms = 15,
  free_scales = FALSE,
  measure = c("chi2", "lr", "frequency", "docprop"),
  show_negative = FALSE,
  text_size = NULL,
  show_na_title = TRUE,
  cluster_label = NULL,
  keyness_plot_xlab = NULL,
  colors = NULL
)
}
\arguments{
\item{res}{result object of a \code{rainette} clustering}

\item{dtm}{the dfm object used to compute the clustering}

\item{k}{number of groups. If NULL, use the biggest number possible}

\item{type}{type of term plots : barplot or wordcloud}

\item{n_terms}{number of terms to display in keyness plots}

\item{free_scales}{if TRUE, all the keyness plots will have the same scale}

\item{measure}{statistics to compute}

\item{show_negative}{if TRUE, show negative keyness features}

\item{text_size}{font size for barplots, max word size for wordclouds}

\item{show_na_title}{if TRUE, show number of NA as plot title}

\item{cluster_label}{define a specific term for clusters identification in keyness plots.
Default is "Cluster" or "Cl." depending on the number of groups. If a vector of length > 1,
define the cluster labels manually.}

\item{keyness_plot_xlab}{define a specific x label for keyness plots.}

\item{colors}{vector of custom colors for cluster titles and branches (in the order of the clusters)}
}
\value{
A gtable object.
}
\description{
Generate a clustering description plot from a rainette result
}
\examples{
\donttest{
require(quanteda)
corpus <- data_corpus_inaugural
corpus <- head(corpus, n = 10)
corpus <- split_segments(corpus)
tok <- tokens(corpus, remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("en"))
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 3)
res <- rainette(dtm, k = 3, min_segment_size = 15)
rainette_plot(res, dtm)
rainette_plot(
  res,
  dtm,
  cluster_label = c("Assets", "Future", "Values"),
  colors = c("red", "slateblue", "forestgreen")
)
}

}
\seealso{
\code{\link[quanteda.textstats:textstat_keyness]{quanteda.textstats::textstat_keyness()}}, \code{\link[=rainette_explor]{rainette_explor()}}, \code{\link[=rainette_stats]{rainette_stats()}}
}
