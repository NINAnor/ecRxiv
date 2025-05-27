library(shiny)
library(DT)
library(bslib)
source("global.R")

link_gh <- tags$a(
  shiny::icon("github"), "GitHub",
  href = "https://github.com/NINAnor/ecRxiv",
  target = "_blank"
)

data <- App_data
#tags$head(tags$link(rel="shortcut icon", href="www/favicon.png")),

ui <- page_navbar(
  id='nav',
  theme = bs_theme(version = 5, bootswatch = "minty") |>
    bslib::bs_add_rules(
      rules = "
                    .navbar.navbar-default {
                        background-color: $primary !important;
                    }
                    "
    ),
  bg = "#6c6c6c",
    title = div(img(src='_ecrxiv_logo_hovedlogo.png',
                    style="margin-top: 14px;
                               padding-right:10px;
                               padding-bottom:0px",
                    height = 60)),
    position = "static-top", # Ensures it stays at the top
    nav_panel("Start page",
        uiOutput('startpage')),
    nav_panel("Find indicator",
      DT::DTOutput("indicatorTable")
        ),
    nav_panel("Documentation",
      htmlOutput("documentation")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_gh)
    )

)

server <- function(input, output, session) {
  shiny::addResourcePath("indicators", here::here("indicators"))
  
  output$indicatorTable <- DT::renderDT(
    data |>
      dplyr::select(!c(HTML_File, Variable, url)) ,
    selection = "single",
    filter = "top"
  )
  
  # Automatically switch to documentation tab when a row is selected
  observeEvent(input$indicatorTable_rows_selected, {
    if (length(input$indicatorTable_rows_selected) > 0) {
      updateNavbarPage(session, inputId = "nav", selected = "Documentation")
    }
  })
  
  output$documentation <- renderUI({
    selected_row <- input$indicatorTable_rows_selected
    if (length(selected_row) == 0) {
      return(NULL)
    } else {
      selected_ID <- data$ID[selected_row]
      html_file_path <- data$HTML_File[data$ID == selected_ID]
      html_file_path2 <- paste0("indicators/", selected_ID, "/R/", selected_ID, ".html")
      if (!file.exists(html_file_path)) {
        return(shiny::tags$p("No documentation available for the selected ecosystem."))
      } else {
        div(
          tags$iframe(src = html_file_path2, style = 'width:100vw;height:100vh;')
        )
      }
    }
  })
  
  output$startpage <- renderUI({
    layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(includeMarkdown("overview.md")),
      bslib::card(includeMarkdown("HowToUse.md")),
      bslib::card(includeMarkdown("contribute.md")),
      bslib::card(includeMarkdown("contact.md"))
    )
  })
}


shiny::shinyApp(ui = ui, server = server)

