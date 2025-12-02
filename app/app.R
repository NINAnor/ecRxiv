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
  window_title = "ecRxiv",
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
    title = div(
      img(
        src = "_ecrxiv_logo_hovedlogo.png",
        style = "margin-top:14px; padding-right:10px; padding-bottom:0px;",
        height = 60
      )),
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
      dplyr::select(!c(html_file_rel, url,html_file_abs, file)) ,
    
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
    
    if (length(selected_row) == 0) return(NULL)
    
    # 1. Get selected indicator ID
    selected_ID <- data$indicator_id[selected_row]
    
    # 2. Get the relative html path from metadata
    html_rel <- data$html_file_rel[data$indicator_id == selected_ID]
    
    # 3. Convert to full path to check existence
    html_full <- here::here(html_rel)
    
    if (!file.exists(html_full)) {
      return(tags$p("No documentation available for the selected indicator."))
    }
    
    # 4. Show iframe using the RELATIVE path (Shiny serves it via addResourcePath)
    tags$iframe(
      src = html_rel,
      style = "width:100%; height:90vh; border:none;"
    )
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
