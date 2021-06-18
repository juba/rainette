## Cluster documents sample UI

docs_sample_ui <- function(id) {
    ns <- shiny::NS(id)
    fillRow(
        flex = c(1, 3),
        fillCol(
            flex = c(10, 1),
            id = "side",
            div(
                uiOutput(ns("group_ui")),
                numericInput(
                    ns("ndoc"), "Maximum number of documents",
                    value = 10, min = 1
                ),
                checkboxInput(ns("random_sample"), "Random sample", value = FALSE),
                numericInput(
                    ns("nchar"), "Maximum length",
                    value = 1000, min = 10
                ),
                textInput(
                    ns("filter_term"), "Filter by term",
                    value = ""
                )
            )
        ),
        fillCol(
            flex = c(10, 1),
            id = "docs",
            div(
                htmlOutput(ns("docs_sample_intro")),
                htmlOutput(ns("docs_sample"))
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
                rainette::cutree_rainette(res, k = current_k())
            })

            ## Corpus from wanted cluster
            corpus_cluster <- reactive({
                if (is.null(corpus_src)) {
                    return(NULL)
                }
                ## Select only from wanted cluster
                sel <- groups() == input$cluster & !is.na(groups())
                corpus_src[sel]
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
                    "</strong>.</p>"
                )
                htmltools::HTML(out)
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
                    "<div class='doc'><span class='name'>",
                    quanteda::docnames(corp),
                    "</span><br />",
                    txt, "</div>",
                    collapse = "\n"
                )
                htmltools::HTML(out)
            })
        }
    )
}