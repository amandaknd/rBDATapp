#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # calling the translator sent as a golem option
  #i18n <- golem::get_golem_options(which = "translator")
  #i18n$set_translation_language("en")


  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      navbarPage(title = "Inventory data", windowTitle = "rBDATapp",
                 footer = tagList(includeHTML(app_sys("app/www/include_footer.html"))),
                 tabPanel("Data", mod_input_data_ui("input_data_1")),
                 tabPanel("Charts", mod_histogram_ui("histogram_1")),
                 tabPanel("Table", mod_table_ui("table_1")),
                 tabPanel("Notation", mod_notation_ui("notation_1"))
                 #navbarMenu("Language",
                 #            radioButtons(inputId = 'selected_language',
                #              "",
                #              choices = i18n$get_languages(),
                #              inline = TRUE,
                #              selected = i18n$get_key_translation()))
      )#end navbar page


    )#end fluid page
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
