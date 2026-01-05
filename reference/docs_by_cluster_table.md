# Returns, for each cluster, the number of source documents with at least n segments of this cluster

Returns, for each cluster, the number of source documents with at least
n segments of this cluster

## Usage

``` r
docs_by_cluster_table(obj, clust_var = NULL, doc_id = NULL, threshold = 1)
```

## Arguments

- obj:

  a corpus, tokens or dtm object

- clust_var:

  name of the docvar with the clusters

- doc_id:

  docvar identifying the source document

- threshold:

  the minimal number of segments of a given cluster that a document must
  include to be counted

## Details

This function is only useful for previously segmented corpus. If
`doc_id` is NULL and there is a `sement_source` docvar, it will be used
instead.

## See also

[`clusters_by_doc_table()`](https://juba.github.io/rainette/reference/clusters_by_doc_table.md)

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
dtm <- dfm_trim(dtm, min_docfreq = 2)
res <- rainette(dtm, k = 3, min_segment_size = 15)
#>   Merging segments to comply with min_segment_size...
#>   Clustering...
#>   Done.
corpus$cluster <- cutree(res, k = 3)
docs_by_cluster_table(corpus, clust_var = "cluster")
#> # A tibble: 3 × 3
#>   cluster     n   `%`
#>   <chr>   <int> <dbl>
#> 1 clust_1    10   100
#> 2 clust_2     9    90
#> 3 clust_3     9    90
# }
```
