#' Shiny gadget for rainette2 clustering exploration
#'
#' @param res result object of a `rainette2` clustering
#' @param dtm the dfm object used to compute the clustering
#' @param corpus_src the quanteda corpus object used to compute the dtm
#'
#' @seealso [rainette2_plot()]
#'
#' @return
#' No return value, called for side effects.
#'
#' @export
#'
#' @import shiny
#' @import miniUI

rainette2_explor <- function(res, dtm = NULL, corpus_src = NULL) {

  ## Error if no dtm
  if (is.null(dtm)) {
    stop("rainette2_explor must be called with a result object and its associated dtm.")
  }

  ## Stop if res comes from rainette instead of rainette2
  if (inherits(res, "rainette")) {
    stop("trying to run rainette2_explor on a rainette result object.")
  }

  ## If rainette2 has been called with full=FALSE, only chi2 criterion is available
  criterion_choices <- c(
    "Sum of chi-squared" = "chi2"
  )
  if (!is.null(attr(res, "full")) && attr(res, "full")) {
    criterion_choices <- c(
      criterion_choices,
      "Sum of sizes" = "n"
    )
  }
  res_name <- deparse(substitute(res))
  dtm_name <- deparse(substitute(dtm))
  max_n_groups <- max(res$k, na.rm = TRUE)


  ui <- miniPage(
    gadgetTitleBar("Clustering exploration", left = NULL),
    tags$head(tags$style(rainette_explor_css())),
    miniTabstripPanel(
      miniTabPanel(
        "Summary",
        icon = shiny::icon("chart-bar"),
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
                selectInput("criterion", "Partition criterion",
                  choices = criterion_choices
                ),
                checkboxInput("complete_km", label = "Complete with k-nearest neighbours", value = FALSE),
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
              plotOutput("rainette2_plot", height = "100%")
            )
          )
        )
      ),
          miniTabPanel(
        "Cluster documents", icon = shiny::icon("file-alt"),
        miniContentPanel(
          docs_sample_ui("rainette2", res)
        )
      )
    )
  )

  server <- function(input, output, session) {

    plot_code <- reactive({
      code <- paste0("rainette2_plot(\n  ", res_name, ", ", dtm_name, ", k = ", input$k,
        ",\n  criterion = \"", input$criterion, "\"",
        ",\n  n_terms = ", input$n_terms,
        ",\n  free_scales = ", !input$same_scales,
        ",\n  measure = \"", input$measure, "\"",
        ",\n  show_negative = ", input$show_negative,
        ifelse(input$complete_km,
          paste0(",\n  complete_groups = \"", input$complete_km, "\""),
          ""
        ),
        ifelse(input$text_size != "10",
          paste0(",\n  text_size = ", input$text_size),
          ""
        ),
        "\n)"
      )
      code
    })

    cutree_code <- reactive({
      out <- ""
      if (input$complete_km) {
        out <- "group <- "
      }
      out <- paste0(out, "cutree_rainette2(", res_name,
        ", k = ", input$k,
        ", criterion = \"", input$criterion, "\"",
        ")")
      if (input$complete_km) {
        out <- paste0(
          out, "\n",
          "rainette2_complete_groups(", dtm_name, ", groups)"
        )
      }
      out
    })

    generate_code <- reactive({
      code <- "## Clustering description plot\n"
      code <- paste0(code, plot_code())
      code <- paste0(code, "\n## Groups\n")
      code <- paste0(code, cutree_code())
      code
    })

    output$rainette2_plot <- renderPlot({
      eval(parse(text = plot_code()))
    })

    ## Code export modal dialog
    observeEvent(input$get_r_code, {
      code <- generate_code()
      showModal(modalDialog(
        title = gettext("Export R code"), size = "l",
        HTML(paste0("Code to generate the current plot and compute groups :",
          "<pre><code>",
          paste(highr::hi_html(code), collapse = "\n"),
          "</code></pre>")),
        easyClose = TRUE))
    })

    current_k <- shiny::reactive(input$k)
    docs_sample_server("rainette2", res, corpus_src, current_k)

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ## Generate code
      code <- generate_code()
      out <- paste0("\n-------- Start exported code --------\n\n",
                    paste(code, collapse = "\n"),
                    "\n\n--------- End exported code ---------\n")
      cat(out)
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server)

}