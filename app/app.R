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

ui <- navbarPage(

  id = "nav",
  windowTitle = "ecRxiv",
  bg = "#FDE6D2",
  # --- Logo + title ---
  title = div(
    tags$img(
      src = "_ecrxiv_logo_hovedlogo.png",
      style = "margin-top: 14px; padding-right:10px; padding-bottom:0px",
      height = 60
    )
  ),

  # --- Theme (Bootstrap 5 Minty) ---
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ) |>
    bs_add_rules(
      rules = "
        .navbar {
          background-color: #6C6C6C !important;
        }

         /* Tab text */
        .navbar-nav .nav-link {
          color: #000000 !important;
        }

        /* Active tab */
        .navbar-nav .nav-link.active {
          color: #000000 !important;
          font-weight: 600;
        }

        /* Hover */
        .navbar-nav .nav-link:hover {
          color: #000000 !important;
        }
      "
    ),

  # --- Background colour for the whole navbar ---
  inverse = TRUE,     # keeps text light on dark bg
  collapsible = TRUE,

  # ======================
  # NAV PANELS
  # ======================

  tabPanel(
    "Start page",
    uiOutput("startpage")
  ),

  tabPanel(
    "Find indicator",
    DTOutput("indicatorTable")
  ),

  tabPanel(
    "Documentation",
    htmlOutput("documentation")
  ),

  # ======================
  # RIGHT-SIDE MENU
  # ======================
  nav_spacer(),
  navbarMenu(
    "Links",
    bslib::nav_item(link_gh)
  )
)


server <- function(input, output, session) {
  shiny::addResourcePath("indicators", file.path(app_dir, "indicators"))
  output$indicatorTable <- DT::renderDT(
    data |>
      dplyr::select(!c(html_file_rel, url,html_file_abs, file)),
    selection = "single",
    filter = "top",
    options = list(
      pageLength = 30,
      lengthMenu = c(10,20,30, 50, 100),
      columnDefs = list(list(
        targets = "_all",
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}"
        )
      ))),
    class = "display"
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

    selected_ID <- data$indicator_id[selected_row]
    html_rel <- data$html_file_rel[data$indicator_id == selected_ID]

    html_full <- normalizePath(file.path(app_dir, html_rel), mustWork = FALSE)

    if (!file.exists(html_full)) {
      return(tags$p("No documentation available for the selected indicator."))
    }

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
