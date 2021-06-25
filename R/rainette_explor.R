#' Shiny gadget for rainette clustering exploration
#'
#' @param res result object of a `rainette` clustering
#' @param dtm the dfm object used to compute the clustering
#' @param corpus_src the quanteda corpus object used to compute the dtm
#'
#' @seealso `rainette_plot`
#'
#' @return
#' No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' require(quanteda)
#' corpus <- data_corpus_inaugural
#' corpus <- head(corpus, n = 10)
#' corpus <- split_segments(corpus)
#' tok <- tokens(corpus, remove_punct = TRUE)
#' tok <- tokens_remove(tok, stopwords("en"))
#' dtm <- dfm(tok, tolower = TRUE)
#' dtm <- dfm_trim(dtm, min_docfreq = 3)
#' res <- rainette(dtm, k = 3, min_segment_size = 15)
#' rainette_explor(res, dtm, corpus)
#' }
#'
#' @export
#'
#' @import shiny
#' @import miniUI

rainette_explor <- function(res, dtm = NULL, corpus_src = NULL) {

  ## Check for res, dtm and corpus_src values and consistency
  if (is.null(dtm)) {
    stop("rainette_explor must be called with a result object and its associated dtm.")
  }
  if (!is.null(corpus_src) && ndoc(corpus_src) != ndoc(dtm)) {
      stop("corpus_src and dtm must have the same number of documents.")
  }

  ## If res is a rainette2 result, launch rainette2_explor
  if (inherits(res, "rainette2")) {
    rainette::rainette2_explor(res, dtm, corpus_src)
    return()
  }

  if (length(res$group) != ndoc(dtm)) {
    stop("res and dtm must have the same number of documents.")
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
                    "Keyness - Chi-squared" = "chi2",
                    "Keyness - Likelihood ratio" = "lr",
                    "Frequency - Terms" = "frequency",
                    "Frequency - Documents proportion" = "docprop"
                  )
                ),
                numericInput("n_terms",
                  label = "Number of terms to display",
                  value = 20, min = 5, max = 30, step = 1
                ),
                conditionalPanel(
                  "input.measure != 'docprop'",
                  checkboxInput("same_scales", label = "Force same scales", value = TRUE)
                ),
                checkboxInput("show_negative", label = "Show negative values", value = FALSE),
                sliderInput("text_size",
                  label = "Text size",
                  value = 12, min = 6, max = 20, step = 1
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
      code <- paste0(
          "rainette_plot(", res_name, ",", dtm_name, ", k = ", input$k,
          ", type = \"bar\"",
          ", n_terms = ", input$n_terms,
          ", free_scales = ", !input$same_scales,
          ", measure = \"", input$measure, "\"",
          ", show_negative = \"", input$show_negative, "\"",
          ", text_size = ", input$text_size, ")"
      )
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

    current_k <- shiny::reactive({input$k})
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
    ui, server
  )
}


