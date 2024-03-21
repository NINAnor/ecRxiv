
library(shiny)
library(DT)

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML("
        /* CSS for setting background color */
        body {
          background-color: #f2f2f2; /* Set your desired background color */
        }
      ")
    )
  ),
  shiny::navbarPage(
    title = "Ecosystem Condition Indicators",
    shiny::tabPanel("Overview"),
    shiny::tabPanel("Find indicator",
                    DT::DTOutput("indicatorTable")),
    shiny::tabPanel("Documentation",
                    uiOutput("documentation")),
    shiny::tabPanel("Contribute"),
    shiny::tabPanel("Contact"),
  )
)

server <- function(input, output) {
  output$indicatorTable <- DT::renderDT(
    data.frame(Ecosystem = "Skog og fjell",
               Egenskap = "Primærproduksjon",
               ECT = "Structural state characteristic",
               Contact = "Anders Kolstad")
  )
  
  output$documentation <- renderUI({
    selected_row <- input$indicatorTable_rows_selected
    if (length(selected_row) == 0) {
      return(NULL)
    } else {
      selected_row <- data.frame(Ecosystem = "Skog og fjell",
                                 HTML_Content = "<h1>Ecosystem Documentation</h1><p>This is the documentation for Skog og fjell.</p>")
      HTML(selected_row$HTML_Content)
    }
  })
}

shiny::shinyApp(ui = ui, server = server)