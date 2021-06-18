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
.doc .name {
  font-weight: bold;
  color: #777;
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
          docs_sample_ui("rainette1")
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

    current_k <- reactive({input$k})
    docs_sample_server("rainette1", res, corpus_src, current_k)

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


