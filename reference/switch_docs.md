# Switch documents between two groups to maximize chi-square value

Switch documents between two groups to maximize chi-square value

## Usage

``` r
switch_docs(m, indices, max_index, max_chisq)
```

## Arguments

- m:

  original dtm

- indices:

  documents indices orderes by first CA axis coordinates

- max_index:

  document index where the split is maximum

- max_chisq:

  maximum chi-square value

## Value

a list of two vectors `indices1` and `indices2`, which contain the
documents indices of each group after documents switching, and a `chisq`
value, the new corresponding chi-square value after switching

## Details

Internal function, not to be used directly
