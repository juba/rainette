##' Split a character string or corpus into segments, taking into account punctuation where possible
##'
##' @param obj character string or corpus object
##' @param ... arguments passed to other methods
##'
##' @return
##' If obj is a tm or quanteda corpus object, the result is a quanteda corpus.
##' If obj is a character string, the result is a tibble.
##' @export
##'
##' @examples
##' \dontrun{
##' require(quanteda)
##' split_segments(data_corpus_inaugural)
##' }

split_segments <- function(obj, ...) {
  UseMethod("split_segments")
}


##' @rdname split_segments
##' @param text character string
##' @param segment_size segment size (in words)
##' @param segment_size_window window around segment size to look for best splitting point
##' @return
##' A tibble with the computed segments
##' @aliases split_segments.character
##' @export
##' @import dplyr
##' @import quanteda
##' @import tibble
##' @import purrr


split_segments.character <- function(text, segment_size = 40, segment_size_window = 15, ...) {

  if (!(inherits(text, "character") && length(text) == 1)) stop("text must be a character vector of size 1")

  ## Tokenize into words
  words <- as.character(quanteda::tokens(text, what = "word"))

  ## Compute "weight" for each word
  words_tbl <- tibble(word = words) %>%
    mutate(weight = case_when(
      word %in% c(".", "?", "!", "…") ~ 6,
      word == ":" ~ 5,
      word == ";" ~ 4,
      word == "," ~ 1,
      TRUE ~ 0.01
    ))

  split_indices <- 1

  ## Loop over words vector
  while (sum(split_indices) <= nrow(words_tbl) - (segment_size * 1.25)) {
    last_index <- sum(split_indices)
    ## Filter words from start to segment size + window, and compute
    ## distance and ratio weight / distance
    tmp_tbl <- words_tbl %>%
      slice(last_index:(last_index + segment_size + segment_size_window)) %>%
      mutate(rank = 1:n(),
             distance = abs(rank - segment_size) + 1,
             ratio = weight / distance)

    ## Split at maximum ratio
    split <- which.max(tmp_tbl$ratio)
    split_indices <- append(split_indices, split)
  }

  ## Recompute split indices
  split_indices <- cumsum(split_indices)

  ## Recompute segments by splitting and pasting
  segments <- purrr::map2_dfr(head(split_indices, -1),
                              head(lead(split_indices), -1) - 1,
                              ~ tibble(segment = paste0(words[.x:.y], collapse = " ")))

  return(segments)
}


##' @rdname split_segments
##' @param corpus a tm corpus object
##' @param segment_size segment size (in words)
##' @param segment_size_window window around segment size to look for best splitting point
##' @aliases explor.Corpus
##' @export


split_segments.Corpus <- function(corpus, segment_size = 40, segment_size_window = 15, ...) {
  
  if (!inherits(corpus, "Corpus")) stop("corpus must be of class Corpus")
  
  corpus <- quanteda::corpus(corpus)
  split_segements(corpus)
  
}


##' @rdname split_segments
##' @param corpus a quanteda corpus object
##' @param segment_size segment size (in words)
##' @param segment_size_window window around segment size to look for best splitting point
##' @aliases explor.corpus
##' @export
##' @import purrr
##' @import dplyr
##' @import tidyr
##' @import tibble
##' @import progress


split_segments.corpus <- function(corpus, segment_size = 40, segment_size_window = 15, ...) {
  
  if (!inherits(corpus, "corpus")) stop("corpus must be of class corpus")
  
  corpus$documents$segment_source <- rownames(docvars(corpus))
  
  if (interactive()) {
    pb <- progress::progress_bar$new(total = ndoc(corpus),
                                     format = "  Splitting [:bar] :percent in :elapsed",
                                     clear = FALSE)
    pb$tick(0)
    corpus$documents$texts <- purrr::map(corpus$documents$texts, 
                                         function(text) {
                                           pb$tick()
                                           split_segments(text)
                                         })
  } else {
    corpus$documents$texts <- purrr::map(corpus$documents$texts, split_segments)
  }
  
  
  corpus$documents <- corpus$documents %>%
    tidyr::unnest(texts) %>%
    dplyr::rename(texts = segment) %>% 
    group_by(segment_source) %>% 
    mutate(segment_id = paste0(segment_source, "_", 1:n())) %>% 
    as.data.frame %>% 
    tibble::column_to_rownames("segment_id")
  
  corpus
  
}
  
