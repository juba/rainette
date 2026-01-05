# Split a character string or corpus into segments

Split a character string or corpus into segments, taking into account
punctuation where possible

## Usage

``` r
split_segments(obj, segment_size = 40, segment_size_window = NULL)

# S3 method for class 'character'
split_segments(obj, segment_size = 40, segment_size_window = NULL)

# S3 method for class 'Corpus'
split_segments(obj, segment_size = 40, segment_size_window = NULL)

# S3 method for class 'corpus'
split_segments(obj, segment_size = 40, segment_size_window = NULL)

# S3 method for class 'tokens'
split_segments(obj, segment_size = 40, segment_size_window = NULL)
```

## Arguments

- obj:

  character string, quanteda or tm corpus object

- segment_size:

  segment size (in words)

- segment_size_window:

  window around segment size to look for best splitting point

## Value

If obj is a tm or quanteda corpus object, the result is a quanteda
corpus.

## Examples

``` r
# \donttest{
require(quanteda)
split_segments(data_corpus_inaugural)
#>   Splitting...
#>   Done.
#> Corpus consisting of 3,658 documents and 5 docvars.
#> 1789-Washington_1 :
#> "Fellow-Citizens of the Senate and of the House of Representa..."
#> 
#> 1789-Washington_2 :
#> "On the one hand, I was summoned by my Country, whose voice I..."
#> 
#> 1789-Washington_3 :
#> "as the asylum of my declining years - a retreat which was re..."
#> 
#> 1789-Washington_4 :
#> "On the other hand, the magnitude and difficulty of the trust..."
#> 
#> 1789-Washington_5 :
#> "could not but overwhelm with despondence one who (inheriting..."
#> 
#> 1789-Washington_6 :
#> "In this conflict of emotions all I dare aver is that it has ..."
#> 
#> [ reached max_ndoc ... 3,652 more documents ]
# }
```
