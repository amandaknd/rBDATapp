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
        tableOutput(ns("def_table")),

        strong("Components notation"),
        tableOutput(ns("comp_table"))
        
    )

  )#end taglist
}

#' notation Server Functions
#'
#' @noRd
mod_notation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$def_table <- renderTable({
      names <- c("Vfm", "Efm", "X", "Sth", "Ab", "Ind", "nvDh", "EV", "FixN", "Biomass")
      def <- c("Volume over bark", "Volume under bark", "Non-usable wood at stem foot (X-Holz)",
               "Stem wood", "Upper part of stem wood, second length after transport cut",
               "Industrial wood", "Non-usable coarse wood",
               "Sum of volumes X, Sth, Ab, Ind, nvDh and FixN", "Volume of the fix segment",
               "Above ground biomass for a given tree")
      
      x <- as.data.frame(cbind("Notation"= names, "Definition"= def))
      x
    })
    
    output$comp_table <- renderTable({
      names <- c("agb", "stw", "stb", "sw", "sb", "fwb", "ndl")
      def <- c("Total aboveground biomass", "Stump wood", "Stump bark",
               "Solid wood with diameter above 7cm over bark", "Bark of component sw",
               "Fine wood incl. bark", "Needles")
      
      x <- as.data.frame(cbind("Notation"= names, "Definition"= def))
      x
    })

  })
}

## To be copied in the UI
# mod_notation_ui("notation_1")

## To be copied in the server
# mod_notation_server("notation_1")
