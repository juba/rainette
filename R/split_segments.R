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
##' If obj is a character string, the result is a tibble.
##' @export
##'
##' @examples
##' \dontrun{
##' require(quanteda)
##' split_segments(data_corpus_inaugural)
##' }

split_segments <- function(obj, segment_size = 40, segment_size_window = NULL) {
  UseMethod("split_segments")
}


##' @rdname split_segments
##' @aliases split_segments.character
##' 
##' @export
##' @import dplyr
##' @import quanteda
##' @importFrom tibble tibble
##' @importFrom purrr map_chr


split_segments.character <- function(obj, segment_size = 40, segment_size_window = NULL) {
  
  text <- obj
  
  if (!(inherits(text, "character") && length(text) == 1)) stop("text must be a character vector of size 1")
  
  ## Default segment_size_window
  if (is.null(segment_size_window)) {
    segment_size_window <- 0.4 * segment_size
  }
  
  ## Tokenize into words
  words <- as.character(quanteda::tokens(text, what = "fastestword"))
  
  ## If string is shorter than segment_size, returns it
  if (length(words) <= segment_size) {
    return(tibble::tibble(segment = obj))
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
      
  tibble::tibble(segment = segments)
      
}



##' @rdname split_segments
##' @aliases split_segments.Corpus
##' @export


split_segments.Corpus <- function(obj, segment_size = 40, segment_size_window = NULL) {
  
  corpus <- obj
  
  if (!inherits(corpus, "Corpus")) stop("corpus must be of class Corpus")
  
  corpus <- quanteda::corpus(corpus)
  split_segments(corpus)
  
}


##' @rdname split_segments
##' @aliases split_segments.corpus
##' @export
##' @importFrom purrr map_int 
##' @importFrom tibble column_to_rownames


split_segments.corpus <- function(obj, segment_size = 40, segment_size_window = NULL) {
  
  corpus <- obj
  
  if (!inherits(corpus, "corpus")) stop("corpus must be of class corpus")
  
  corpus$documents$segment_source <- rownames(docvars(corpus))
  
  corpus_length <- sum(purrr::map_int(texts(obj), nchar))
  use_multicore <- corpus_length > 10000000
  
  if (use_multicore) {
    message("  Splitting in parallel (please wait while R sessions start)...")    
  } else {
    message("  Splitting...")
  }
  
  progressr::with_progress({
    p <- progressr::progressor(along = corpus$documents$texts)

    if (use_multicore) {
      options(future.supportsMulticore.unstable = "quiet")
      future::plan(future::multiprocess)

      corpus$documents$texts <- future.apply::future_lapply(
        corpus$documents$texts, 
        function(text) {
          p()
          split_segments(text, segment_size, segment_size_window)
        }
      )
    } else {
      corpus$documents$texts <- lapply(
        corpus$documents$texts, 
        function(text) {
          p()
          split_segments(text, segment_size, segment_size_window)
        }
      )
    }
  })
  
  corpus$documents <- corpus$documents %>%
    tidyr::unnest(texts) %>%
    dplyr::rename(texts = segment) %>% 
    group_by(segment_source) %>% 
    mutate(segment_id = paste0(segment_source, "_", 1:n())) %>% 
    as.data.frame %>% 
    tibble::column_to_rownames("segment_id")
  
  corpus
  
}
  
