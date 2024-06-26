#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # calling the translator sent as a golem option
  #i18n <- golem::get_golem_options(which = "translator")
  #i18n$set_translation_language("en")

  # keep track of language object as a reactive
  #i18n_r <- reactive({
  #  i18n #<- golem::get_golem_options(which = "translator")
  #})

  #observeEvent(input[["selected_language"]], {
  #  shiny.i18n::update_lang(input[["selected_language"]], session)
  #  i18n_r()$set_translation_language(input[["selected_language"]])
  #})


  # Your application server logic
  data_input <- mod_input_data_server("input_data_1")
  df <- mod_histogram_server("histogram_1", data_input)
  mod_table_server("table_1", df, data_input)
  mod_notation_server("notation_1")
}
