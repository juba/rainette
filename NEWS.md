# rainette 0.3.0

## New features

Rework of the double classification computation :

- New `full` argument to `rainette2()`. If `TRUE` (default, same behavior as previous versions), all crossings between groups of the two single classifications are taken into account to find the best partition. If `FALSE`, only the crossings with maximal associations are kept.
- New `parallel` argument to `rainette2()` to compute partitions with `mclapply` (`FALSE` by default, won't work on Windows, uses more RAM)
- Global optimization and speed up of `rainette2()` computations, with added progress bars to better estimate long runs

## Documentation

- Improved french vignette "description des algorithmes"
- New english vignette "algorithms description"
- Reworked french and english introduction vignettes

## Deprecated features

- The wordcloud plots will be deprecated in a near future. A warning has been added to `rainette_plot()` and `rainette2_plot()` if they are called with `type = "cloud"`.

## Other

- Add `show_na_title`, `cluster_label` and `keyness_plot_xlab` arguments to `rainette_plot()` to customize graphics output
- Fix warnings in Font Awesome icon names

# rainette 0.2.1

- Add option to show merged segments in document browser
- Fix warning and error in `rainette_explor` and `rainette2_explor` when a cluster dfm is empty
- Fix error when the dfm contains empty string as feature

# rainette 0.2.0

## Important and breaking changes

- `min_uc_size`, `uc_size1` and `uc_size2` arguments to `rainette` and `rainette2` have been renamed to `min_segment_size`, `min_segment_size1` and `min_segment_size2`.
- The default value of `min_segment_size` in `rainette` is now `0`, which means that no merging is done between segments by default. Results could then be different from previous package versions when `min_uc_size` was not specified.
- Important bugfix : merging of segments based on `min_segment_size` was not handled correctly in the previous versions regarding the segment sources, as segments from different documents could be merged together. This should now be fixed.

## New features

- A new graphical interface to browse cluster documents has been added to `rainette_explor` and `rainette2_explor`.
- New function `clusters_by_doc_table` which gives the number of segments of each cluster for each document.
- New function `docs_by_cluster_table` which gives, for each cluster, the number of documents with at least one segment in this cluster.
- `split_segments` should now be about 4 times faster.
- Terms frequencies and documents proportions statistics have been added to the explor interfaces.

## Other

- When `rainette` is called with `min_segment_size` > 0, a `doc_id` argument must be given which is the name of a `dtm` docvar identifying the segments source. If the corpus has been produced by `split_segments`, the added `segment_source` docvar is used by default.
- Color palette for clusters changed to "Tableau 10".
- Negative keyness values are not shown by default anymore in `rainette_explor`, `rainette2_explor`, `rainette_plot` and `rainette2_plot`.
- Wordcloud plots have been removed from explor interfaces.
- A warning is displayed when `min_split_members` < 3.
- If `rainette_explor` is called on a `rainette2` results object, `rainette2_explor` is launched automatically.

# rainette 0.1.3

- Parallel computing in split_segments has been removed
- Fix potential name conflicts in `rainette_explor`
- Compatibility with quanteda v3 (thanks @kbenoit)

# rainette 0.1.2

- Fix bug due to factor comparison in `rainette2`

# rainette 0.1.1

- Compatibility with dplyr 1.0
- Fix bug in p threshold in `rainette_stats`

# rainette 0.1.0

- First version
