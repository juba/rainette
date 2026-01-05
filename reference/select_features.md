# Remove features from dtm of each group base don cc_test and tsj

Remove features from dtm of each group base don cc_test and tsj

## Usage

``` r
select_features(m, indices1, indices2, cc_test = 0.3, tsj = 3)
```

## Arguments

- m:

  global dtm

- indices1:

  indices of documents of group 1

- indices2:

  indices of documents of group 2

- cc_test:

  maximum contingency coefficient value for the feature to be kept in
  both groups.

- tsj:

  minimum feature frequency in the dtm

## Value

a list of two character vectors : `cols1` is the name of features to
keep in group 1, `cols2` the name of features to keep in group 2

## Details

Internal function, not to be used directly
