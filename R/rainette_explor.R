rainette_explor_css <- function() {
  "
#main, #docs {
  padding: 0 1em;
}
#docs {
  height: 100%;
  max-height: 100%;
  overflow-y: scroll;
}
#side {
  background-color: #EEEEEE;
  padding: 2em 3em;
}
.doc {
  font-size: 90%;
  background-color: #F6F6F6;
  border-radius: 10px;
  margin-bottom: 1em;
  padding: 1em;
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


#' Shiny gadget for rainette clustering exploration
#'
#' @param res result object of a `rainette` clustering
#' @param dtm the dfm object used to compute the clustering
#'
#' @seealso `rainette_plot`
#'
#' @return
#' No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' library(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' dtm <- dfm(corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
#' dtm <- dfm_trim(dtm, min_termfreq = 3)
#' res <- rainette(dtm, k = 3)
#' rainette_explor(dtm, res)
#' }
#'
#' @export
#'
#' @import shiny
#' @import miniUI

rainette_explor <- function(res, dtm = NULL, corpus_src = NULL) {

  ## Error if no dtm
  if (is.null(dtm)) {
    stop("rainette_explor must be called with a result object and its associated dtm.")
  }

  ## If res is a rainette2 result, launch rainette2_explor
  if (inherits(res, "rainette2")) {
    rainette::rainette2_explor(res, dtm)
    return()
  }

  res_name <- deparse(substitute(res))
  dtm_name <- deparse(substitute(dtm))
  max_n_groups <- max(res$group, na.rm = TRUE)

  ui <- miniPage(
    gadgetTitleBar("Clustering exploration", left = NULL),
    tags$head(tags$style(rainette_explor_css())),
    miniTabstripPanel(
      miniTabPanel(
        "Summary", icon = shiny::icon("bar-chart"),
        miniContentPanel(
          fillRow(
            flex = c(1, 3),
            fillCol(
              flex = c(10, 1),
              id = "side",
              div(
                sliderInput("k",
                  label = "Number of clusters",
                  value = max_n_groups,
                  min = 2, max = max_n_groups, step = 1
                ),
                selectInput("measure", "Statistics",
                  choices = c(
                    "Chi-squared" = "chi2",
                    "Likelihood ratio" = "lr"
                  )
                ),
                selectInput("type", "Plot type",
                  choices = c(
                    "Barplot" = "bar",
                    "Word cloud" = "cloud"
                  )
                ),
                numericInput("n_terms",
                  label = "Max number of terms to display",
                  value = 20, min = 5, max = 30, step = 1
                ),
                checkboxInput("same_scales", label = "Force same scales", value = TRUE),
                conditionalPanel(
                  "input.type == 'bar'",
                  checkboxInput("show_negative", label = "Show negative values", value = FALSE),
                  sliderInput("text_size",
                    label = "Text size",
                    value = 13, min = 6, max = 20, step = 1
                  )
                ),
                conditionalPanel(
                  "input.type == 'cloud'",
                  sliderInput("max_size",
                    label = "Max text size",
                    value = 15, min = 6, max = 30, step = 1
                  )
                )
              ),
              actionButton("get_r_code",
                class = "btn-success",
                icon = icon("code"),
                label = gettext("Get R code")
              )
            ),
            fillCol(
              id = "main",
              plotOutput("rainette_plot", height = "100%")
            )
          )
        )
      ),
      miniTabPanel(
        "Cluster documents", icon = shiny::icon("file-text"),
        miniContentPanel(
          fillRow(
            flex = c(1, 3),
            fillCol(
              flex = c(10, 1),
              id = "side",
              div(
                uiOutput("group_ui"),
                numericInput(
                  "ndoc", "Maximum number of documents",
                  value = 10, min = 1
                ),
                numericInput(
                  "nchar", "Maximum number of characters",
                  value = 1000, min = 10
                ),
                textInput(
                  "filter_term", "Filter by term",
                  value = ""
                )
              )
            ),
           fillCol(
              flex = c(10, 1),
              id = "docs",
              div(
                htmlOutput("docs_sample_intro"),
                htmlOutput("docs_sample")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    plot_code <- reactive({
      if (input$type == "bar") {
        code <- paste0(
          "rainette_plot(", res_name, ",", dtm_name, ", k = ", input$k,
          ", type = \"bar\"",
          ", n_terms = ", input$n_terms,
          ", free_scales = ", !input$same_scales,
          ", measure = \"", input$measure, "\"",
          ", show_negative = \"", input$show_negative, "\"",
          ", text_size = ", input$text_size, ")"
        )
      }
      if (input$type == "cloud") {
        code <- paste0(
          "rainette_plot(", res_name, ",", dtm_name, ", k = ", input$k,
          ", type = \"cloud\"",
          ", n_terms = ", input$n_terms,
          ", free_scales = ", !input$same_scales,
          ", measure = \"", input$measure, "\"",
          ", text_size = ", input$max_size, ")"
        )
      }
      code
    })

    cutree_code <- reactive({
      paste0(
        "cutree_rainette(", res_name,
        ", k = ", input$k, ")"
      )
    })

    generate_code <- reactive({
      code <- "## Clustering description plot\n"
      code <- paste0(code, plot_code())
      code <- paste0(code, "\n## Groups\n")
      code <- paste0(code, cutree_code())
      code <- formatR::tidy_source(
        text = code,
        width.cutoff = 75,
        output = FALSE
      )$text.tidy
      code
    })

    output$rainette_plot <- renderPlot({
      eval(parse(text = plot_code()), envir = .GlobalEnv)
    })

    ## Code export modal dialog
    observeEvent(input$get_r_code, {
      code <- generate_code()
      showModal(modalDialog(
        title = gettext("Export R code"), size = "l",
        HTML(paste0(
          "Code to generate the current plot and compute groups :",
          "<pre><code>",
          paste(highr::hi_html(code), collapse = "\n"),
          "</code></pre>"
        )),
        easyClose = TRUE
      ))
    })

    ## Cluster selection slider
    output$group_ui <- renderUI({
      sliderInput("cluster",
        label = "Cluster",
        value = 1,
        min = 1, max = input$k, step = 1
      )
    })

    ## Current clusters
    groups <- reactive({
      rainette::cutree_rainette(res, k = input$k)
    })

    ## Corpus from wanted cluster
    corpus_cluster <- reactive({
      if (is.null(corpus_src)) return(NULL)
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
      if (is.null(corpus_src)) return(NULL)
      result <- corpus_cluster()
      ## Filter by terms
      filter_term <- stringr::str_trim(input$filter_term)
      if (!is.null(filter_term) && filter_term != "") {
        keep <- stringr::str_detect(
          as.character(corpus_cluster()),
          filter_regex()
        )
        result <- result[keep]
        texts <- stringr::str_replace_all(
          as.character(result),
          filter_regex(),
          "<span class='highlight'>\\0</span>"
        )
        result[] <- texts
      }
      result
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
        "<p>Cluster <strong>",
        input$cluster,
        "</strong> of <strong>",
        input$k,
        "</strong> / Displayed : <strong>",
        min(input$ndoc, nb_docs_cluster),
        "</strong>"
      )
      if (quanteda::ndoc(corpus_cluster()) != quanteda::ndoc(corpus_filtered())) {
        out <- paste0(out,
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
      if (is.null(corpus_src)) return(NULL)

      ## Sample docs
      corp <- quanteda::corpus_sample(
        corpus_filtered(),
        size = min(quanteda::ndoc(corpus_filtered()), input$ndoc)
      )

      ## Truncate texts
      txt <- as.character(corp)
      txt <- ifelse(
        nchar(txt) <= input$nchar,
        txt,
        paste(stringr::str_sub(txt, 1, input$nchar), "(...)")
      )

      ## Generate output
      out <- paste(
        "<div class='doc'><strong>",
        quanteda::docnames(corp),
        "</strong><br />",
        txt, "</div>",
        collapse = "\n"
      )
      htmltools::HTML(out)
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ## Generate code
      code <- generate_code()
      out <- paste0(
        "\n-------- Start exported code --------\n\n",
        paste(code, collapse = "\n"),
        "\n\n--------- End exported code ---------\n"
      )
      cat(out)
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    ui, server,
    viewer = shiny::dialogViewer("Clusters exploration", width = 1500, height = 1000)
  )
}