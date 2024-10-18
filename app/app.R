library(shiny)
library(DT)
# here::here()
# source create_metadata functions
source(here::here("R/Create_metadata.R"))
# data

data <- readRDS(here::here("data/App_data.RDS"))

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
    shiny::tabPanel(
      "Overview",
      includeMarkdown("overview.md")
    ),
    shiny::tabPanel(
      "Find indicator",
      p("Select an indicator from the list and then move to the Documentation page to see the associated documentation"),
      DT::DTOutput("indicatorTable")
    ),
    shiny::tabPanel(
      "Documentation",
      uiOutput("documentation")
    ),
    shiny::tabPanel(
      "Contribute",
      includeMarkdown("contribute.md")
    ),
    shiny::tabPanel(
      "Contact",
      includeMarkdown("contact.md")
    ),
  )
)

server <- function(input, output) {
  shiny::addResourcePath("html_files", here::here("indicators"))

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
      selected_ecosystem <- data$ID[selected_row]
      html_file_path <- data$HTML_File[data$ID == selected_ecosystem]
      # print(html_file_path)
      if (!file.exists(html_file_path)) {
        return(shiny::tags$p("No documentation available for the selected ecosystem."))
      } else {
        shiny::includeHTML(html_file_path)
      }
    }
  })
}


shiny::shinyApp(ui = ui, server = server)

