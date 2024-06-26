#' notation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_notation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
        strong("Table Notation"),
        h6("Vfm: Volume over bark"),
        h6("Efm: Volume under bark"),
        h6("X: non-usable wood at stem foot (X-Holz)"),
        h6("Sth: stem wood"),
        h6("Ab: upper part of stem wood, second length after transport cut"),
        h6("Ind: industrial wood"),
        h6("nvDh: non-usable coarse wood"),
        h6("EV: sum of volumes X, Sth, Ab, Ind, nvDh and FixN"),
        h6("FixN: volume of the fix segment"),
        h6("Biomass: above ground biomass for a given tree"),

        strong("Components notation"),
        h6("agb: total aboveground biomass"),
        h6("stw: stump wood"),
        h6("stb: stump bark"),
        h6("sw: solid wood with diameter above 7cm over bark"),
        h6("sb: bark of component sw"),
        h6("fwb: fine wood incl. bark"),
        h6("ndl: needles")
    )

  )#end taglist
}

#' notation Server Functions
#'
#' @noRd
mod_notation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_notation_ui("notation_1")

## To be copied in the server
# mod_notation_server("notation_1")
