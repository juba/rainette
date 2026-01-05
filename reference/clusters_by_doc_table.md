# Returns the number of segment of each cluster for each source document

Returns the number of segment of each cluster for each source document

## Usage

``` r
clusters_by_doc_table(obj, clust_var = NULL, doc_id = NULL, prop = FALSE)
```

## Arguments

- obj:

  a corpus, tokens or dtm object

- clust_var:

  name of the docvar with the clusters

- doc_id:

  docvar identifying the source document

- prop:

  if TRUE, returns the percentage of each cluster by document

## Details

This function is only useful for previously segmented corpus. If
`doc_id` is NULL and there is a `sement_source` docvar, it will be used
instead.

## See also

[`docs_by_cluster_table()`](https://juba.github.io/rainette/reference/docs_by_cluster_table.md)

## Examples

``` r
# \donttest{
require(quanteda)
#> Loading required package: quanteda
#> Package version: 4.3.1
#> Unicode version: 15.1
#> ICU version: 74.2
#> Parallel computing: disabled
#> See https://quanteda.io for tutorials and examples.
corpus <- data_corpus_inaugural
corpus <- head(corpus, n = 10)
corpus <- split_segments(corpus)
#>   Splitting...
#>   Done.
tok <- tokens(corpus, remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("en"))
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 2)
res <- rainette(dtm, k = 3, min_segment_size = 15)
#>   Merging segments to comply with min_segment_size...
#>   Clustering...
#>   Done.
corpus$cluster <- cutree(res, k = 3)
clusters_by_doc_table(corpus, clust_var = "cluster", prop = TRUE)
#> # A tibble: 10 × 4
#>    doc_id          clust_1 clust_2 clust_3
#>    <chr>             <dbl>   <dbl>   <dbl>
#>  1 1789-Washington   29.7     56.8    13.5
#>  2 1793-Washington  100        0       0  
#>  3 1797-Adams        13.6     64.4    22.0
#>  4 1801-Jefferson     9.09    72.7    18.2
#>  5 1805-Jefferson    10.5     52.6    36.8
#>  6 1809-Madison      25       32.1    42.9
#>  7 1813-Madison      21.2     33.3    45.5
#>  8 1817-Monroe       18.2     22.7    59.1
#>  9 1821-Monroe       14.8     13.9    71.3
#> 10 1825-Adams        19.5     31.2    49.4
# }
```
