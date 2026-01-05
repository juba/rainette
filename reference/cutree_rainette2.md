# Cut a rainette2 result object into groups of documents

Cut a rainette2 result object into groups of documents

## Usage

``` r
cutree_rainette2(res, k, criterion = c("chi2", "n"), ...)
```

## Arguments

- res:

  the `rainette2` result object to be cut

- k:

  the desired number of clusters

- criterion:

  criterion to use to choose the best partition. `chi2` means the
  partition with the maximum sum of chi2, `n` the partition with the
  maximum size.

- ...:

  arguments passed to other methods

## Value

A vector with group membership.

## See also

[`rainette2_complete_groups()`](https://juba.github.io/rainette/reference/rainette2_complete_groups.md)
