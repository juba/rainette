## Extract texts from a list of documents in Iramuteq format
## `thematics` indicate if potential thematics have to been removed

extract_texts <- function(docs, thematics) {
  purrr::map_chr(docs, function(text) {
    ## Remove metadata
    text <- stringr::str_replace_all(text, "^\\*\\w+_.*?\n", "")
    ## Remove thematics
    if (thematics == "remove") {
      text <- stringr::str_replace_all(text, "(^|\n)-\\*[^ ].*?\n", "\\1")
    }
    text
  })
}

## Extract metadata from a list of documents in Iramuteq format
## and return them as a data.frame

extract_metadata <- function(docs) {
  meta <- purrr::map_dfr(docs, function(text) {
    first_line <- stringr::str_extract(text, "^.*?\n")
    vars_line <- stringr::str_extract_all(first_line, "\\*(.*?)[ \n$]")

    # No metadata for this doc
    if (identical(vars_line[[1]], NA_character_)) {
      return(data.frame(rainette_nometa = NA))
    }

    res <- purrr::map(vars_line, function(vars) {
      vars <- stringr::str_replace(vars, "^\\*", "")
      vars <- stringr::str_trim(vars)
      vars <- stringr::str_split_fixed(vars, "_", n = 2)
      values <- as.list(vars[, 2])
      names(values) <- vars[, 1]
      values
    })
    purrr::flatten_dfr(res)
  })
  meta[["rainette_nometa"]] <- NULL
  meta
}


#' Import a corpus in Iramuteq format
#'
#' @param f a file name or a connection
#' @param id_var name of metadata variable to be used as documents id
#' @param thematics if "remove", thematics lines are removed. If "split", texts as splitted at each thematic, and metadata duplicated accordingly
#' @param ... arguments passed to \code{\link[base:connections]{file}} if `f` is a file name.
#'
#' @details
#' A description of the Iramuteq corpus format can be found here : \url{http://www.iramuteq.org/documentation/html/2-2-2-les-regles-de-formatages}
#'
#' @return
#' A quanteda corpus object. Note that metadata variables in docvars are all imported as characters.
#'
#' @export
#' @import stringr
#' @importFrom rlang sym

import_corpus_iramuteq <- function(f, id_var = NULL, thematics = c("remove", "split"), ...) {

  thematics <- match.arg(thematics)

  ## Open as a filename or a connection
  close_con <- FALSE
  if (is.character(f)) {
    close_con <- TRUE
    f <- file(f, ...)
  }
  if (!inherits(f, "connection")) {
    stop("f must be either a connection or a file path")
  }

  ## Read text
  lines <- readLines(f)
  if (close_con) {
    close(f)
  }
  lines <- lines[lines != ""]
  text <- paste(lines, collapse = "\n")

  ## Detect if corpus has metadata
  has_metadata <- stringr::str_detect(text, "(^|\n)\\*\\*\\*\\* +\\*\\w+_")
  ## Detect if corpus has thematics
  has_thematic <- stringr::str_detect(text, "(^|\n)-\\*[^ ]")

  ## Split into documents
  docs <- stringr::str_split(text, "(^|\n)\\*\\*\\*\\*(\n| +)", simplify = TRUE)
  docs <- docs[docs != ""]

  ## Extract texts
  texts <- extract_texts(docs, thematics)
  ## Extract metadata
  if (has_metadata) {
    metadata <- extract_metadata(docs)
  }

  ## Split by thematics
  if (has_thematic && thematics == "split") {
    thems <- purrr::map(stringr::str_match_all(docs, "(^|\n)-\\*(.+)\n"), ~.x[, 3])
    thems <- purrr::flatten_chr(thems)
    texts <- stringr::str_split(texts, "(^|\n)-\\*(.+)\n")
    texts <- purrr::imap_dfr(texts, function(text, i) {
      data.frame(id = i, text = text[text != ""], stringsAsFactors = FALSE)
    })
    ## Duplicate metadata and add thematics variable
    if (has_metadata) {
      metadata <- metadata[texts$id, ]
      metadata <- data.frame(metadata, thematics = thems, stringsAsFactors = FALSE)
      ## Generate id by thematic if necessary
      if (!is.null(id_var)) {
        metadata <- metadata %>%
          dplyr::group_by(.data[[id_var]]) %>%
          dplyr::mutate(rainette_split_id = paste(.data[[id_var]], 1:n(), sep = "_"))
      }
      id_var <- "rainette_split_id"
    } else {
      metadata <- data.frame(thematics = thems, stringsAsFactors = FALSE)
      has_metadata <- TRUE
    }
    texts <- texts$text
  }

  ## Build corpus
  corpus <- data.frame(text = texts, stringsAsFactors = FALSE)
  if (has_metadata) {
    corpus <- data.frame(metadata, corpus, check.names = FALSE)
  }
  if (!is.null(id_var)) {
    rownames(corpus) <- corpus[[id_var]]
    ## Remove computed id by thematic if present
    corpus[["rainette_split_id"]] <- NULL
  }

  quanteda::corpus(corpus, text_field = "text")

}
