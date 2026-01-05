# Corpus clustering based on the Reinert method - Simple clustering

Corpus clustering based on the Reinert method - Simple clustering

## Usage

``` r
rainette(
  dtm,
  k = 10,
  min_segment_size = 0,
  doc_id = NULL,
  min_split_members = 5,
  cc_test = 0.3,
  tsj = 3,
  min_members,
  min_uc_size
)
```

## Arguments

- dtm:

  quanteda dfm object of documents to cluster, usually the result of
  [`split_segments()`](https://juba.github.io/rainette/reference/split_segments.md)

- k:

  maximum number of clusters to compute

- min_segment_size:

  minimum number of forms by document

- doc_id:

  character name of a dtm docvar which identifies source documents.

- min_split_members:

  don't try to split groups with fewer members

- cc_test:

  contingency coefficient value for feature selection

- tsj:

  minimum frequency value for feature selection

- min_members:

  deprecated, use `min_split_members` instead

- min_uc_size:

  deprecated, use `min_segment_size` instead

## Value

The result is a list of both class `hclust` and `rainette`. Besides the
elements of an `hclust` object, two more results are available :

- `uce_groups` give the group of each document for each k

- `group` give the group of each document for the maximum value of k
  available

## Details

See the references for original articles on the method. Computations and
results may differ quite a bit, see the package vignettes for more
details.

The dtm object is automatically converted to boolean.

If `min_segment_size > 0` then `doc_id` must be provided unless the
corpus comes from `split_segments`, in this case `segment_source` is
used by default.

## References

- Reinert M, Une méthode de classification descendante hiérarchique :
  application à l'analyse lexicale par contexte, Cahiers de l'analyse
  des données, Volume 8, Numéro 2, 1983.
  <https://www.numdam.org/item/?id=CAD_1983__8_2_187_0>

- Reinert M., Alceste une méthodologie d'analyse des données textuelles
  et une application: Aurelia De Gerard De Nerval, Bulletin de
  Méthodologie Sociologique, Volume 26, Numéro 1, 1990.
  [doi:10.1177/075910639002600103](https://doi.org/10.1177/075910639002600103)

## See also

[`split_segments()`](https://juba.github.io/rainette/reference/split_segments.md),
[`rainette2()`](https://juba.github.io/rainette/reference/rainette2.md),
[`cutree_rainette()`](https://juba.github.io/rainette/reference/cutree_rainette.md),
[`rainette_plot()`](https://juba.github.io/rainette/reference/rainette_plot.md),
[`rainette_explor()`](https://juba.github.io/rainette/reference/rainette_explor.md)

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
# }
```
