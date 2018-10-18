#' Split a character string into segments, taking into account punctuation where possible
#'
#' @param text character string
#' @param segment_size segment size in words
#' @param segment_size_window window around segment size to look for best splitting point
#'
#' @return
#' A tibble with the computed segments
#' @export
#'
#' @examples

split_segments <- function(text, segment_size = 40, segment_size_window = 15) {

  ## Tokenize into words
  words <- as.character(quanteda::tokens(text, what="word"))

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
  while(sum(split_indices) <= nrow(words_tbl) - (segment_size * 1.25)) {
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
