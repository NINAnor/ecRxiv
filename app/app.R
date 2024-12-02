library(shiny)
library(DT)
# here::here()
# source global
source("global.R")
# data

data <- App_data

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"), # Include your CSS file here
    tags$style(
      shiny::HTML("
        .navbar {
          display: flex !important;
          flex-wrap: nowrap !important;
          justify-content: space-between !important;
          align-items: center !important;
          background-color: #;
        }
        .navbar-nav {
          display: flex !important;
          flex-direction: row !important;
          padding-left: 0 !important;
          color: white !important;
        }
        .navbar-nav > li {
          margin-right: 15px !important;
        }
        .navbar-nav > li > a {
          display: block !important;
          padding: 10px 15px !important;
        }
        .navbar-header {
          flex: 0 0 auto !important;
        }
        .navbar-collapse {
          flex-grow: 1 !important;
        }
      ")
    )
  ),
  navbarPage(
    title = "Ecosystem Condition Indicators",
    position = "static-top", # Ensures it stays at the top
    tabsetPanel(
    shiny::tabPanel("Overview", includeMarkdown("overview.md")),
    shiny::tabPanel("Find indicator", DT::DTOutput("indicatorTable")),
    shiny::tabPanel("Documentation", htmlOutput("documentation")),
    shiny::tabPanel("Contribute", includeMarkdown("contribute.md")),
    shiny::tabPanel("Contact", includeMarkdown("contact.md"))
  ))
)

server <- function(input, output) {
  shiny::addResourcePath("indicators", here::here("indicators"))

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
      selected_ID <- data$ID[selected_row]
      html_file_path <- data$HTML_File[data$ID == selected_ID]
      html_file_path2 <- paste0("indicators/", selected_ID, "/R/", selected_ID, ".html")
      # print(html_file_path)
      if (!file.exists(html_file_path)) {
        return(shiny::tags$p("No documentation available for the selected ecosystem."))
      } else {
        div(#class="documentation-content",
            tags$iframe(src= html_file_path2, width=1200, height=800)
            )
      }
    }
  })
}


shiny::shinyApp(ui = ui, server = server)

