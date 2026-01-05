# Complete groups membership with knn classification

Starting with groups membership computed from a `rainette2` clustering,
every document not assigned to a cluster is reassigned using a k-nearest
neighbour classification.

## Usage

``` r
rainette2_complete_groups(dfm, groups, k = 1, ...)
```

## Arguments

- dfm:

  dfm object used for `rainette2` clustering.

- groups:

  group membership computed by `cutree` on `rainette2` result.

- k:

  number of neighbours considered.

- ...:

  other arguments passed to
  [`FNN::knn`](https://rdrr.io/pkg/FNN/man/knn.html).

## Value

Completed group membership vector.

## See also

[`cutree_rainette2()`](https://juba.github.io/rainette/reference/cutree_rainette2.md),
[`FNN::knn()`](https://rdrr.io/pkg/FNN/man/knn.html)
