# Shiny gadget for rainette clustering exploration

Shiny gadget for rainette clustering exploration

## Usage

``` r
rainette_explor(res, dtm = NULL, corpus_src = NULL)
```

## Arguments

- res:

  result object of a `rainette` clustering

- dtm:

  the dfm object used to compute the clustering

- corpus_src:

  the quanteda corpus object used to compute the dtm

## Value

No return value, called for side effects.

## See also

`rainette_plot`

## Examples

``` r
if (FALSE) { # \dontrun{
require(quanteda)
corpus <- data_corpus_inaugural
corpus <- head(corpus, n = 10)
corpus <- split_segments(corpus)
tok <- tokens(corpus, remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("en"))
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 3)
res <- rainette(dtm, k = 3, min_segment_size = 15)
rainette_explor(res, dtm, corpus)
} # }
```
