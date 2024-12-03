library(shiny)
library(DT)
source("global.R")


data <- App_data

ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="www/favicon.png")),
  navbarPage(
    title = div(img(src='_ecrxiv_logo_hovedlogo.png',
                    style="margin-top: -14px;
                               padding-right:10px;
                               padding-bottom:10px",
                    height = 60)),
    position = "static-top", # Ensures it stays at the top
    
    shiny::tabPanel("Overview", includeMarkdown("overview.md")),
    shiny::tabPanel("Find indicator", DT::DTOutput("indicatorTable")),
    shiny::tabPanel("Documentation", htmlOutput("documentation")),
    navbarMenu('More',
      shiny::tabPanel("Contribute", includeMarkdown("contribute.md")),
      shiny::tabPanel("Contact", includeMarkdown("contact.md"))
    )
  )
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
            tags$iframe(src= html_file_path2, style='width:100vw;height:100vh;')
            )
      }
    }
  })
}


shiny::shinyApp(ui = ui, server = server)

