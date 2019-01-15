rainette_explor_css <- function() {
  "#rainette_plot {
    height:100vh !important;
  }"
}


#'
#' @export
#' 
#' @import shiny
#' @import shinythemes

rainette_explor <- function(res, dtm) {
  
  max_n_groups <- max(res$group)
  
  library(shiny)
  
  ui <- fluidPage(
    theme = shinythemes::shinytheme("cosmo"),
    sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(rainette_explor_css())),
        width = 3,
        numericInput("k", label = "Number of clusters", 
          value = max_n_groups,
          min = 2, max = max_n_groups, step = 1),
        selectInput("measure", "Statistics", 
          choices = c("Chi-squared" = "chi2",
                      "Likelihood ratio" = "lr")),
        numericInput("n_terms", label = "Max number of terms to display",
          value = 15, min = 5, max = 30, step = 1),
        checkboxInput("free_x", label = "Free x axis", value = FALSE)
      ),
      mainPanel(
        plotOutput("rainette_plot", height = "100%")
      )
    )
  )
  
  server <- function(input, output, session) {
    output$rainette_plot <- renderPlot({
      rainette_plot(res, dtm, k = input$k, input$n_terms, input$free_x, input$measure)
    })
  }
  
  shinyApp(ui, server)
}