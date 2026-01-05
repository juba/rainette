# Corpus clustering based on the Reinert method - Double clustering

Corpus clustering based on the Reinert method - Double clustering

## Usage

``` r
rainette2(
  x,
  y = NULL,
  max_k = 5,
  min_segment_size1 = 10,
  min_segment_size2 = 15,
  doc_id = NULL,
  min_members = 10,
  min_chi2 = 3.84,
  parallel = FALSE,
  full = TRUE,
  uc_size1,
  uc_size2,
  ...
)
```

## Arguments

- x:

  either a quanteda dfm object or the result of
  [`rainette()`](https://juba.github.io/rainette/reference/rainette.md)

- y:

  if `x` is a
  [`rainette()`](https://juba.github.io/rainette/reference/rainette.md)
  result, this must be another
  [`rainette()`](https://juba.github.io/rainette/reference/rainette.md)
  result from same dfm but with different uc size.

- max_k:

  maximum number of clusters to compute

- min_segment_size1:

  if `x` is a dfm, minimum uc size for first clustering

- min_segment_size2:

  if `x` is a dfm, minimum uc size for second clustering

- doc_id:

  character name of a dtm docvar which identifies source documents.

- min_members:

  minimum members of each cluster

- min_chi2:

  minimum chi2 for each cluster

- parallel:

  if TRUE, use
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html) to
  compute partitions (won't work on Windows, uses more RAM)

- full:

  if TRUE, all crossed groups are kept to compute optimal partitions,
  otherwise only the most mutually associated groups are kept.

- uc_size1:

  deprecated, use min_segment_size1 instead

- uc_size2:

  deprecated, use min_segment_size2 instead

- ...:

  if `x` is a dfm object, parameters passed to
  [`rainette()`](https://juba.github.io/rainette/reference/rainette.md)
  for both simple clusterings

## Value

A tibble with optimal partitions found for each available value of `k`
as rows, and the following columns :

- `clusters` list of the crossed original clusters used in the partition

- `k` the number of clusters

- `chi2` sum of the chi2 value of each cluster

- `n` sum of the size of each cluster

- `groups` group membership of each document for this partition (`NA` if
  not assigned)

## Details

You can pass a quanteda dfm as `x` object, the function then performs
two simple clustering with varying minimum uc size, and then proceed to
find optimal partitions based on the results of both clusterings.

If both clusterings have already been computed, you can pass them as `x`
and `y` arguments and the function will only look for optimal
partitions.

`doc_id` must be provided unless the corpus comes from `split_segments`,
in this case `segment_source` is used by default.

If `full = FALSE`, computation may be much faster, but the chi2
criterion will be the only one available for best partition detection,
and the result may not be optimal.

For more details on optimal partitions search algorithm, please see
package vignettes.

## References

- Reinert M, Une méthode de classification descendante hiérarchique :
  application à l'analyse lexicale par contexte, Cahiers de l'analyse
  des données, Volume 8, Numéro 2, 1983.
  <http://www.numdam.org/item/?id=CAD_1983__8_2_187_0>

- Reinert M., Alceste une méthodologie d'analyse des données textuelles
  et une application: Aurelia De Gerard De Nerval, Bulletin de
  Méthodologie Sociologique, Volume 26, Numéro 1, 1990.
  [doi:10.1177/075910639002600103](https://doi.org/10.1177/075910639002600103)

## See also

[`rainette()`](https://juba.github.io/rainette/reference/rainette.md),
[`cutree_rainette2()`](https://juba.github.io/rainette/reference/cutree_rainette2.md),
[`rainette2_plot()`](https://juba.github.io/rainette/reference/rainette2_plot.md),
[`rainette2_explor()`](https://juba.github.io/rainette/reference/rainette2_explor.md)

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

res1 <- rainette(dtm, k = 5, min_segment_size = 10)
#>   Merging segments to comply with min_segment_size...
#>   Clustering...
#>   Done.
res2 <- rainette(dtm, k = 5, min_segment_size = 15)
#>   Merging segments to comply with min_segment_size...
#>   Clustering...
#>   Done.

res <- rainette2(res1, res2, max_k = 4)
#>   Searching for best partitions...
#>   Computing size 2 partitions...
#>   Computing size 3 partitions...
#>   Computing size 4 partitions...
#>   Selecting best partitions...
#>   Done.
# }
```
