# Split a dtm into two clusters with reinert algorithm

Split a dtm into two clusters with reinert algorithm

## Usage

``` r
cluster_tab(dtm, cc_test = 0.3, tsj = 3)
```

## Arguments

- dtm:

  to be split, passed by `rainette`

- cc_test:

  maximum contingency coefficient value for the feature to be kept in
  both groups.

- tsj:

  minimum feature frequency in the dtm

## Value

An object of class `hclust` and `rainette`

## Details

Internal function, not to be used directly
