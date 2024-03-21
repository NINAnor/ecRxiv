library(shiny)
library(DT)

# Sample data 
data <- data.frame(
  Ecosystem = c("Skog og fjell", "Other Ecosystem"),
  Egenskap  = c("Primærproduksjon", "Other"),
  ECT       =c("Structural state characteristic", "Other"),
  Contact   =c("Anders Kolstad","Anders Kolstad"),
  HTML_File = c("C:/Users/matthew.grainger/Documents/Projects_in_development/ECindicators/Skog og fjell.html",
                "C:/Users/matthew.grainger/Documents/Projects_in_development/ECindicators/other_ecosystem.html")
)

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
    data |> 
      dplyr::select(!HTML_File),
    selection = "single"
  )
  
  
  output$documentation <- renderUI({
    selected_row <- input$indicatorTable_rows_selected
    if (length(selected_row) == 0) {
      return(NULL)
    } else {
      selected_ecosystem <- data$Ecosystem[selected_row]
      html_file_path <- data$HTML_File[data$Ecosystem == selected_ecosystem]
      print(html_file_path)
      if (!file.exists(html_file_path)) {
        return(shiny::tags$p("No documentation available for the selected ecosystem."))
        } else {
          shiny::includeHTML(html_file_path)
        }
      }
  })
}


shiny::shinyApp(ui = ui, server = server)

