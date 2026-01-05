# Import a corpus in Iramuteq format

Import a corpus in Iramuteq format

## Usage

``` r
import_corpus_iramuteq(f, id_var = NULL, thematics = c("remove", "split"), ...)
```

## Arguments

- f:

  a file name or a connection

- id_var:

  name of metadata variable to be used as documents id

- thematics:

  if "remove", thematics lines are removed. If "split", texts as
  splitted at each thematic, and metadata duplicated accordingly

- ...:

  arguments passed to [`file`](https://rdrr.io/r/base/connections.html)
  if `f` is a file name.

## Value

A quanteda corpus object. Note that metadata variables in docvars are
all imported as characters.

## Details

A description of the Iramuteq corpus format can be found here :
<http://www.iramuteq.org/documentation/html/2-2-2-les-regles-de-formatages>
