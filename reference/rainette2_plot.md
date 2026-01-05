# Generate a clustering description plot from a rainette2 result

Generate a clustering description plot from a rainette2 result

## Usage

``` r
rainette2_plot(
  res,
  dtm,
  k = NULL,
  criterion = c("chi2", "n"),
  complete_groups = FALSE,
  type = c("bar", "cloud"),
  n_terms = 15,
  free_scales = FALSE,
  measure = c("chi2", "lr", "frequency", "docprop"),
  show_negative = FALSE,
  text_size = 10
)
```

## Arguments

- res:

  result object of a `rainette2` clustering

- dtm:

  the dfm object used to compute the clustering

- k:

  number of groups. If NULL, use the biggest number possible

- criterion:

  criterion to use to choose the best partition. `chi2` means the
  partition with the maximum sum of chi2, `n` the partition with the
  maximum size.

- complete_groups:

  if TRUE, documents with NA cluster are reaffected by k-means
  clustering initialised with current groups centers.

- type:

  type of term plots : barplot or wordcloud

- n_terms:

  number of terms to display in keyness plots

- free_scales:

  if TRUE, all the keyness plots will have the same scale

- measure:

  statistics to compute

- show_negative:

  if TRUE, show negative keyness features

- text_size:

  font size for barplots, max word size for wordclouds

## Value

A gtable object.

## See also

[`quanteda.textstats::textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html),
[`rainette2_explor()`](https://juba.github.io/rainette/reference/rainette2_explor.md),
[`rainette2_complete_groups()`](https://juba.github.io/rainette/reference/rainette2_complete_groups.md)
