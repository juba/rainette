# Cut a tree into groups

Cut a tree into groups

## Usage

``` r
cutree(tree, ...)
```

## Arguments

- tree:

  the hclust tree object to be cut

- ...:

  arguments passed to other methods

## Value

A vector with group membership.

## Details

If `tree` is of class `rainette`, invokes
[`cutree_rainette()`](https://juba.github.io/rainette/reference/cutree_rainette.md).
Otherwise, just run
[`stats::cutree()`](https://rdrr.io/r/stats/cutree.html).
