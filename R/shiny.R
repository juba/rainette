## CSS and modules for Shiny gadgets

## CSS for shiny gadgets

rainette_explor_css <- function() {
  "
#main {
  padding: 0 1em;
}
#docs {
  height: 100%;
  max-height: 100%;
  overflow-y: hidden;
  padding-left: 2em;
}
#docs_sample {
  height: 100%;
  max-height: 100%;
  overflow-y: scroll;
  padding-right: 2em;
}
#docs_sample_intro {
  color: #369;
  background-color: #F0F0F0;
  border-radius: 5px;
  padding: .6em 1em;
  margin-bottom: 20px;
}
#docs_sample hr {
  margin-top: 12px;
  margin-bottom: 12px;
}
#side {
  background-color: #EEEEEE;
  padding: 2em 3em;
  font-size: 12px;
}
.docname {
  font-size: 80%;
  color: #69B;
  margin: 0 0 .3em 0;
}
.doc {
  font-size: 100%;
  max-width: 50em;
  border-left: 3px solid #9BE;
  margin: 0;
  padding: .3em 1em .2em 1em;
}
.doc .highlight {
  background-color: #FF0;
}

/* Syntax highlighting */
span.hl.str { color: #d14;}
span.hl.kwa { color: #099;}
span.hl.num { color: #099;}
span.hl.kwd { color: #333; font-weight: bold;}
span.hl.com { color: #888; font-style: italic;}
"
}



## Cluster documents sample UI

docs_sample_ui <- function(id, res) {

    show_merged_segments <- !inherits(res, "rainette2") &&
        !is.null(res$call$min_segment_size) && res$call$min_segment_size > 1

    ns <- shiny::NS(id)
    fillRow(
        flex = c(1, 3),
        fillCol(
            flex = c(10, 1),
            id = "side",
            div(
                uiOutput(ns("group_ui")),
                numericInput(
                    ns("ndoc"), "Documents displayed",
                    value = 100, min = 1
                ),
                checkboxInput(ns("random_sample"), "Random sample", value = FALSE),
                numericInput(
                    ns("nchar"), "Maximum text length",
                    value = 1000, min = 10
                ),
                textInput(
                    ns("filter_term"), "Filter by term",
                    value = ""
                ),
                if (show_merged_segments) {
                    checkboxInput(ns("show_merged"), "Show merged segments", value = FALSE)
                }
            )
        ),
        fillCol(
            flex = c(10, 1),
            id = "docs",
            fillCol(
                flex = c(NA, 1),
                div(id = "docs_sample_intro",
                    htmlOutput(ns("docs_sample_intro"))
                ),
                div(id = "docs_sample",
                    htmlOutput(ns("docs_sample"))
                )
            )
        )
    )
}


## Cluster documents sample Server

docs_sample_server <- function(id, res, corpus_src, current_k) {
    moduleServer(
        id,
        function(input, output, session) {

            ## Cluster selection slider
            output$group_ui <- renderUI({
                ns <- session$ns
                selectInput(ns("cluster"),
                    label = "Cluster",
                    selected = 1,
                    choices = seq_len(current_k())
                )
            })

            ## Current clusters
            groups <- reactive({
                rainette::cutree(res, k = current_k())
            })

            ## Corpus from wanted cluster
            corpus_cluster <- reactive({
                if (is.null(corpus_src)) {
                    return(NULL)
                }
                ## Select only from wanted cluster
                if (!is.null(input$show_merged) && input$show_merged) {
                    corpus_src$group <- groups()
                    corpus_src$doc_name <- quanteda::docnames(corpus_src)
                    corpus_src$uc_id <- res$corresp_uce_uc$uc
                    docvars(corpus_src) <- docvars(corpus_src) %>%
                        group_by(.data$uc_id) %>%
                        mutate(doc_name = paste(.data$doc_name, collapse = " | ")) %>%
                        ungroup()
                    result <- quanteda::corpus_group(corpus_src, groups = corpus_src$doc_name)
                    sel <- quanteda::docvars(result, "group") == input$cluster &
                        !is.na(quanteda::docvars(result, "group"))
                    result <- result[sel]
                } else {
                    sel <- groups() == input$cluster & !is.na(groups())
                    result <- corpus_src[sel]
                }
                result
            })

            ## Regex for filter term input
            filter_regex <- reactive({
                stringr::regex(
                    shiny::req(input$filter_term),
                    ignore_case = TRUE, multiline = TRUE
                )
            })

            ## Corpus from cluster filtered by term
            corpus_filtered <- reactive({
                if (is.null(corpus_src)) {
                    return(NULL)
                }
                result <- corpus_cluster()
                ## Filter by terms
                filter_term <- stringr::str_trim(input$filter_term)
                if (!is.null(filter_term) && filter_term != "") {
                    keep <- stringr::str_detect(
                        as.character(corpus_cluster()),
                        filter_regex()
                    )
                    result <- result[keep]
                }
                result
            })

            highlighter <- reactive({
                filter_term <- stringr::str_trim(input$filter_term)
                if (!is.null(filter_term) && filter_term != "") {
                    fun <- function(txt) {
                        stringr::str_replace_all(
                            txt,
                            filter_regex(),
                            "<span class='highlight'>\\0</span>"
                        )
                    }
                } else {
                    fun <- I
                }
                fun
            })

            ## Sample cluster documents introducation phrase
            output$docs_sample_intro <- renderUI({
                if (is.null(corpus_src)) {
                    return(
                        HTML("<p>Can't display documents : <tt>corpus_src</tt> is null.</p><p>Please rerun <tt>rainette_explor</tt> with your quanteda corpus object as third parameter : something like <tt>rainette_explor(res, dtm, corpus)</tt>.</p>")
                    )
                }

                nb_docs_cluster <- quanteda::ndoc(corpus_filtered())
                out <- paste0(
                    "Displayed : <strong>",
                    min(input$ndoc, nb_docs_cluster),
                    "</strong>"
                )
                if (quanteda::ndoc(corpus_cluster()) != quanteda::ndoc(corpus_filtered())) {
                    out <- paste0(
                        out,
                        " - Filtered documents : <strong>",
                        quanteda::ndoc(corpus_filtered()),
                        "</strong>"
                    )
                }
                out <- paste0(
                    out,
                    " - Cluster size : <strong>",
                    quanteda::ndoc(corpus_cluster()),
                    "</strong>."
                )
                shiny::HTML(out)
            })

            ## Sample cluster documents
            output$docs_sample <- renderUI({
                if (is.null(corpus_src)) {
                    return(NULL)
                }

                ## Sample docs
                size <- min(quanteda::ndoc(corpus_filtered()), input$ndoc)
                if (input$random_sample) {
                    corp <- quanteda::corpus_sample(corpus_filtered(), size = size)
                } else {
                    corp <- corpus_filtered()[seq_len(size)]
                }

                ## Truncate texts
                txt <- as.character(corp)
                txt <- ifelse(
                    nchar(txt) <= input$nchar,
                    txt,
                    paste(stringr::str_sub(txt, 1, input$nchar), "(...)")
                )

                ## Highlight texts
                txt <- highlighter()(txt)

                ## Generate output
                out <- paste(
                    "<div class='doc'>",
                    "<div class='docname'>",
                    quanteda::docnames(corp),
                    "</div>",
                    stringr::str_replace_all(txt, "\n", "<br>"),
                    "</div>",
                    "<hr>",
                    collapse = "\n"
                )
                shiny::HTML(out)
            })
        }
    )
}