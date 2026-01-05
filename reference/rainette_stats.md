# Generate cluster keyness statistics from a rainette result

Generate cluster keyness statistics from a rainette result

## Usage

``` r
rainette_stats(
  groups,
  dtm,
  measure = c("chi2", "lr", "frequency", "docprop"),
  n_terms = 15,
  show_negative = TRUE,
  max_p = 0.05
)
```

## Arguments

- groups:

  groups membership computed by `cutree_rainette` or `cutree_rainette2`

- dtm:

  the dfm object used to compute the clustering

- measure:

  statistics to compute

- n_terms:

  number of terms to display in keyness plots

- show_negative:

  if TRUE, show negative keyness features

- max_p:

  maximum keyness statistic p-value

## Value

A list with, for each group, a data.frame of keyness statistics for the
most specific n_terms features.

## See also

[`quanteda.textstats::textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html),
[`rainette_explor()`](https://juba.github.io/rainette/reference/rainette_explor.md),
[`rainette_plot()`](https://juba.github.io/rainette/reference/rainette_plot.md)

## Examples

``` r
# \donttest{
require(quanteda)
corpus <- data_corpus_inaugural
corpus <- head(corpus, n = 10)
corpus <- split_segments(corpus)
#>   Splitting...
#>   Done.
tok <- tokens(corpus, remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("en"))
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 3)
res <- rainette(dtm, k = 3, min_segment_size = 15)
#>   Merging segments to comply with min_segment_size...
#>   Clustering...
#>   Done.
groups <- cutree_rainette(res, k = 3)
rainette_stats(groups, dtm)
#> [[1]]
#> # A tibble: 15 × 6
#>    feature     chi2           p n_target n_reference sign    
#>    <chr>      <dbl>       <dbl>    <dbl>       <dbl> <fct>   
#>  1 commerce    28.2 0.000000107       23           1 positive
#>  2 states      26.3 0.000000290       49          18 positive
#>  3 force       25.4 0.000000475       23           2 positive
#>  4 revenue     21.9 0.00000294        16           0 positive
#>  5 made        18.1 0.0000214         26           7 positive
#>  6 shall      -16.5 0.0000479          4          35 negative
#>  7 good       -15.1 0.000102           1          24 negative
#>  8 powers      14.6 0.000134          20           5 positive
#>  9 within      13.7 0.000215          16           3 positive
#> 10 confidence -13.6 0.000222           1          22 negative
#> 11 defense     13.3 0.000261          12           1 positive
#> 12 naval       12.7 0.000358          11           0 positive
#> 13 united      11.7 0.000614          32          16 positive
#> 14 fellow     -11.5 0.000692           3          25 negative
#> 15 war         11.4 0.000729          34          18 positive
#> 
#> [[2]]
#> # A tibble: 15 × 6
#>    feature     chi2        p n_target n_reference sign    
#>    <chr>      <dbl>    <dbl>    <dbl>       <dbl> <fct>   
#>  1 shall       40.9 1.57e-10       28          11 positive
#>  2 confidence  37.0 1.18e- 9       19           4 positive
#>  3 high        21.6 3.39e- 6       14           5 positive
#>  4 fellow      20.4 6.29e- 6       18          10 positive
#>  5 duties      18.3 1.88e- 5       17          10 positive
#>  6 station     17.5 2.94e- 5        9           1 positive
#>  7 future      14.5 1.43e- 4       11           4 positive
#>  8 good        14.3 1.56e- 4       15          10 positive
#>  9 fervent     13.0 3.12e- 4        6           0 positive
#> 10 indulgence  13.0 3.12e- 4        6           0 positive
#> 11 presence    13.0 3.12e- 4        6           0 positive
#> 12 states     -12.7 3.60e- 4        5          62 negative
#> 13 conscious   10.3 1.34e- 3        5           0 positive
#> 14 derive      10.3 1.34e- 3        5           0 positive
#> 15 endeavor    10.3 1.34e- 3        5           0 positive
#> 
#> [[3]]
#> # A tibble: 15 × 6
#>    feature      chi2          p n_target n_reference sign    
#>    <chr>       <dbl>      <dbl>    <dbl>       <dbl> <fct>   
#>  1 can         22.2  0.00000250       29          17 positive
#>  2 happiness   20.4  0.00000617       15           4 positive
#>  3 let         17.9  0.0000232        11           1 positive
#>  4 truth       14.7  0.000125          8           0 positive
#>  5 upon        14.4  0.000149         17           9 positive
#>  6 man         13.6  0.000223          9           1 positive
#>  7 government  13.3  0.000271         45          48 positive
#>  8 order       12.7  0.000372         11           3 positive
#>  9 republican  11.5  0.000690          8           1 positive
#> 10 force      -11.3  0.000778          0          25 negative
#> 11 love        11.0  0.000909          9           2 positive
#> 12 made        -9.67 0.00188           2          31 negative
#> 13 freedom      8.89 0.00286           9           3 positive
#> 14 choice       8.13 0.00436           5           0 positive
#> 15 false        8.13 0.00436           5           0 positive
#> 
# }
```
