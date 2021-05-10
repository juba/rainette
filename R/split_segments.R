##' Split a character string or corpus into segments
##'
##' Split a character string or corpus into segments, taking into account punctuation where possible
##'
##' @param obj character string, quanteda or tm corpus object
##' @param segment_size segment size (in words)
##' @param segment_size_window window around segment size to look for best splitting point
##'
##' @return
##' If obj is a tm or quanteda corpus object, the result is a quanteda corpus.
##' @export
##'
##' @examples
##' \donttest{
##' require(quanteda)
##' split_segments(data_corpus_inaugural)
##' }

split_segments <- function(
  obj, segment_size = 40, segment_size_window = NULL
) {
  UseMethod("split_segments")
}


##' @rdname split_segments
##' @aliases split_segments.character
##'
##' @export
##' @import dplyr
##' @import quanteda
##' @importFrom purrr map_chr


split_segments.character <- function(
  obj, segment_size = 40, segment_size_window = NULL
) {

  text <- obj

  if (!(inherits(text, "character") && length(text) == 1)) {
    stop("text must be a character vector of size 1")
  }

  ## Default segment_size_window
  if (is.null(segment_size_window)) {
    segment_size_window <- 0.4 * segment_size
  }

  ## Tokenize into words
  words <- as.character(quanteda::tokens(text, what = "fastestword"))

  ## If string is shorter than segment_size, returns it
  if (length(words) <= segment_size) {
    return(obj)
  }

  ## Compute "weight" for each word
  last_char <- stringr::str_sub(words, -1)
  weights <- dplyr::case_when(
    last_char %in% c(".", "?", "!", "\u2026") ~ 6,
    last_char == ":" ~ 5,
    last_char == ";" ~ 4,
    last_char == "," ~ 1,
    TRUE ~ 0.01
  )

  slice_indices <- 1:(segment_size + segment_size_window)
  last_index <- 1
  split_indices <- 1
  stop_index <- length(words) - (segment_size + segment_size_window) + 1
  while(last_index < stop_index) {
    indices <- last_index + slice_indices - 1
    tmpw <- weights[indices]
    split_index <- which.max(tmpw / (abs(slice_indices - segment_size) + 1))
    split_indices <- append(split_indices, split_index)
    last_index <- last_index + split_index
  }
  split_indices <- append(split_indices, length(words) - sum(split_indices) + 1)
  split_indices <- cumsum(split_indices)

  segments <- purrr::map_chr(seq_len(length(split_indices) - 1), ~{
    w <- words[split_indices[.x]:(split_indices[.x + 1] - 1)]
    paste0(w, collapse = " ")
  })

  segments

}



##' @rdname split_segments
##' @aliases split_segments.Corpus
##' @export


split_segments.Corpus <- function(
  obj, segment_size = 40, segment_size_window = NULL
) {

  corpus <- obj

  if (!inherits(corpus, "Corpus")) stop("corpus must be of class Corpus")

  corpus <- quanteda::corpus(corpus)
  split_segments(corpus)

}


##' @rdname split_segments
##' @aliases split_segments.corpus
##' @export
##' @importFrom purrr map_int


split_segments.corpus <- function(
  obj, segment_size = 40, segment_size_window = NULL
) {

  corpus <- obj

  if (!inherits(corpus, "corpus")) stop("corpus must be of class corpus")

  quanteda::docvars(corpus, "segment_source") <- quanteda::docnames(corpus)

  message("  Splitting...")

  progressr::with_progress({
    p <- progressr::progressor(along = as.character(corpus))

    texts <- lapply(
      as.character(corpus),
      function(text) {
        p()
        split_segments(text, segment_size, segment_size_window)
      }
    )
  })

  new_corpus <- docvars(corpus) %>%
    dplyr::mutate(text = texts) %>%
    tidyr::unnest(text) %>%
    dplyr::group_by(segment_source) %>%
    dplyr::mutate(segment_id = paste0(segment_source, "_", 1:dplyr::n())) %>%
    quanteda::corpus(
      docid_field = "segment_id",
      text_field = "text"
    )

  message("  Done.")

  new_corpus

}

