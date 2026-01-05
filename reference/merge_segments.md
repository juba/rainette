# Merges segments according to minimum segment size

`rainette_uc_index` docvar

## Usage

``` r
merge_segments(dtm, min_segment_size = 10, doc_id = NULL)
```

## Arguments

- dtm:

  dtm of segments

- min_segment_size:

  minimum number of forms by segment

- doc_id:

  character name of a dtm docvar which identifies source documents.

## Value

the original dtm with a new `rainette_uc_id` docvar.

## Details

If `min_segment_size == 0`, no segments are merged together. If
`min_segment_size > 0` then `doc_id` must be provided unless the corpus
comes from `split_segments`, in this case `segment_source` is used by
default.
