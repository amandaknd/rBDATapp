#' tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(id = "header_tabs",
               #title = "Nutrient export", windowTitle = "NUTS.APP",
               #footer = tagList(includeHTML(app_sys("app/www/include_footer.html"))),
               tabPanel("Data", mod_input_data_ui("input_data_1")),
               tabPanel("Charts", mod_histogram_ui("histogram_1")),
               tabPanel("Table", mod_table_ui("table_1")),
               tabPanel("Notation", mod_notation_ui("notation_1"))
    )#end navbar page
  )
}
    
#' tabs Server Functions
#'
#' @noRd 
mod_tabs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tabs_ui("tabs_1")
    
## To be copied in the server
# mod_tabs_server("tabs_1")
