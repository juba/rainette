# Changelog

## rainette 0.2.0

### Important and breaking changes

- The default value of `min_uc_size` in `rainette` is now 0, which means that no merging is done between segments by default. Results could be different from previous versions when `min_uc_size` was not specified.
- Merging of segments based on `min_uc_size` was not handled correctly in the previous versions regarding the segment sources : segments from different documents could be merged together. This should now be fixed.

### Other

- When `rainette` is called with `min_uc_size` > 0, a `doc_id` argument must be given which is the name of a `dtm` docvar identifying the segments source. If the corpus has been produced by `split_segments`, the added `segment_source` docvar is used by default.
- Add warning when `min_split_members` < 3

## rainette 0.1.3

- Parallel computing in split_segments has been removed
- Fix potential name conflicts in rainette_explor
- Compatibility with quanteda v3 (thanks @kbenoit)

## rainette 0.1.2

- Fix bug due to factor comparison in rainette2

## rainette 0.1.1

- Compatibility with dplyr 1.0
- Fix bug in p threshold in rainette_stats()

## rainette 0.1

- First version
