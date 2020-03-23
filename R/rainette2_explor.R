#' Shiny gadget for rainette2 clustering exploration
#' 
#' @param res result object of a `rainette2` clustering
#' @param dtm the dfm object used to compute the clustering
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

rainette2_explor <- function(res, dtm) {
  
  res_name <- deparse(substitute(res))
  dtm_name <- deparse(substitute(dtm))
  max_n_groups <- max(res$k, na.rm = TRUE)
  
  
  ui <- miniPage(
    gadgetTitleBar("Clustering exploration", left = NULL),
    tags$head(tags$style(rainette_explor_css())),
    fillRow(
      flex = c(1,3),
      fillCol(
        flex = c(10, 1),
        id = "side", 
        div(
          sliderInput("k", label = "Number of clusters", 
            value = max_n_groups,
            min = 2, max = max_n_groups, step = 1),
          selectInput("criterion", "Partition criterion", 
            choices = c("Partition sum of chi-squared" = "chi2",
              "Partition sum of sizes" = "n")),
          checkboxInput("complete_km", label = "Complete with k-nearest neighbours", value = FALSE),
          selectInput("measure", "Statistics", 
            choices = c("Chi-squared" = "chi2",
                        "Likelihood ratio" = "lr")),
          selectInput("type", "Plot type",
            choices = c("Barplot" = "bar",
                        "Word cloud" = "cloud")),
          numericInput("n_terms", label = "Max number of terms to display",
            value = 20, min = 5, max = 30, step = 1),
          checkboxInput("same_scales", label = "Force same scales", value = TRUE),
          conditionalPanel("input.type == 'bar'",
            checkboxInput("show_negative", label = "Show negative values", value = TRUE),
            sliderInput("text_size", label = "Text size", 
              value = 13, min = 6, max = 20, step = 1)
          ),
          conditionalPanel("input.type == 'cloud'",
            sliderInput("max_size", label = "Max text size", 
              value = 15, min = 6, max = 30, step = 1)
          )
        ),
        actionButton("get_r_code",
          class = "btn-success",
          icon = icon("code"),
          label = gettext("Get R code"))
      ),
      fillCol(id = "main",
        plotOutput("rainette2_plot", height = "100%")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    plot_code <- reactive({
      if (input$type == "bar") {
        code <- paste0("rainette2_plot(", res_name, ",", dtm_name,", k = ", input$k,
          ", criterion = \"", input$criterion, "\"",
          ", complete_groups = \"", input$complete_km, "\"",          
          ", type = \"bar\"",
          ", n_terms = ", input$n_terms, 
          ", free_scales = ", !input$same_scales, 
          ", measure = \"", input$measure, "\"",
          ", show_negative = \"", input$show_negative, "\"",
          ", text_size = ", input$text_size, ")")
      }
      if (input$type == "cloud") {
        code <- paste0("rainette2_plot(", res_name, ",", dtm_name,", k = ", input$k,
          ", criterion = \"", input$criterion, "\"",
          ", complete_groups = \"", input$complete_km, "\"",
          ", type = \"cloud\"",
          ", n_terms = ", input$n_terms, 
          ", free_scales = ", !input$same_scales, 
          ", measure = \"", input$measure, "\"",
          ", text_size = ", input$max_size, ")")
      }
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
      code <- formatR::tidy_source(text = code, 
        width.cutoff = 75, 
        output = FALSE)$text.tidy
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
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ## Generate code
      code <- generate_code()
      out <- paste0("\n-------- Start exported code --------\n\n",
                    paste(code, collapse = "\n"),
                    "\n\n--------- End exported code ---------\n")
      cat(out)
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer("Clusters exploration", width = 1500, height = 1000))
  
}