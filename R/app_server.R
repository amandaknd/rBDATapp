#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  

  # Your application server logic
  selected_box <- reactiveVal()
  #mod_tabs_server("tabs_1")
  data_input <- mod_input_data_server("input_data_1", session)
  df <- mod_histogram_server("histogram_1", data_input, selected_box, session)
  mod_table_server("table_1", df, data_input, selected_box)
  mod_notation_server("notation_1")
}
