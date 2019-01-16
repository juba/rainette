rainette_explor_css <- function() {
"
#main {
  padding: 1em;
}
#side {
  background-color: #EEEEEE;
  padding: 2em 3em;
}"
}


#'
#' @export
#' 
#' @import shiny
#' @import miniUI
#' @import shinythemes

rainette_explor <- function(res, dtm) {
  
  max_n_groups <- max(res$group)
  
  ui <- miniPage(
    #gadgetTitleBar("Clustering exploration"),
    tags$head(tags$style(rainette_explor_css())),
    fillRow(
      flex = c(1,3),
      fillCol(id = "side", 
        div(
        sliderInput("k", label = "Number of clusters", 
          value = max_n_groups,
          min = 2, max = max_n_groups, step = 1),
        selectInput("measure", "Statistics", 
          choices = c("Chi-squared" = "chi2",
                      "Likelihood ratio" = "lr")),
        numericInput("n_terms", label = "Max number of terms to display",
          value = 20, min = 5, max = 30, step = 1),
        checkboxInput("free_x", label = "Free x axis", value = FALSE),
        sliderInput("font_size", label = "Font size", 
          value = 13, min = 6, max = 20, step = 1)
        ),
        div(id = "rcode",
          "R code"
          
        )
      ),
      fillCol(id = "main",
        plotOutput("rainette_plot", height = "100%")
      )
    )
  )
  
  server <- function(input, output, session) {
    output$rainette_plot <- renderPlot({
      rainette_plot(res, dtm, k = input$k, 
        n_terms = input$n_terms, 
        free_x = input$free_x, 
        measure = input$measure, 
        font_size = input$font_size)
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer("Clustering exploration", width = 1500, height = 1000))
}