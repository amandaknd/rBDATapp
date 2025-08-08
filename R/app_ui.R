#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage(
      title = div(
        tags$a(
          href = "https://www.fva-bw.de/startseite",
          target = "_blank",
          tags$img(
            src = "www/logo_fva.svg",
            height = "50px",
            style = "position: absolute; top: 50%; left: 8px; transform: translateY(-50%);"
          )
        )
      ),
      id = "tabs",
      
      # Panels
      nav_spacer(),
      tabPanel("Data", mod_input_data_ui("input_data_1")),
      tabPanel("Charts", mod_histogram_ui("histogram_1")),
      tabPanel("Table", mod_table_ui("table_1")),
      tabPanel("Notation", mod_notation_ui("notation_1")),
      
      # Icons
      nav_spacer(),
      nav_item(
        style = "zoom: 1.5;",
        div(
          tags$a(
            shiny::icon("instagram"),
            href = "https://www.instagram.com/fva_bw", target = "_blank"
          ),
          tags$a(
            shiny::icon("youtube"),
            href = "https://www.youtube.com/channel/UCxyaUkQwqig2zL_JG9OotCg", target = "_blank"
          ),
          tags$a(
            shiny::icon("linkedin"),
            href = "https://www.linkedin.com/company/fva-bw", target = "_blank"
          )
        )
      ),
      footer = tagList(
        tags$div(
          class = "footer-container",
          conditionalPanel(
            condition = "input.tabs != 'Karte'",
            tags$footer(
              div("For information and help: Christian.Vonderach@forst.bwl.de"),
              div(HTML("Copyright © 2025"))
            )
          )
        )
      ),

      # Design
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "Minty",
        primary = "#006E60",
        secondary = "#D9C6B0",
        tertiary = "#9D9D9D",
        success = "#6CBC83",
        info = "#17a2b8",
        warning = "#F9A51B",
        danger = "#C50F50",
        brand = "inst/app/www/_brand.yml"
      ),
      lang = "de",
      navbar_options = navbar_options(collapsible = TRUE, underline = TRUE)
    )
  )#end taglist
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rBDATapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
