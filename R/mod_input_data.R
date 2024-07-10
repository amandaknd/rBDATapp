#' input_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_data_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      h2(strong(("Tree Volume and Biomass estimation by rBDAT"))),
      h4("Use the rBDAT package to get segment volume over or under bark for a tree given by species, Dbh, tree height and possibly an upper diameter. Assortment volume and aboveground biomass is calculated for further given specified parameters."),
      tags$a(href="https://cran.r-project.org/web/packages/rBDAT/vignettes/rbdat.html","rBDAT documentation")
    ),#end fluid row
    br(),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          fileInput(ns('target_upload'), 'Select csv/txt file to upload',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      '.csv'
                    )),

          radioButtons(ns("colseparator"),"Column separator:", choices = c(";",",",":"), selected=";",inline=TRUE),
          radioButtons(ns("decseparator"),"Decimal separator:",choices = c(".",","), selected=",",inline=TRUE),
          h6(strong("Required variables for file upload:")),
          h6("spp: numeric, 1 <= spp <= 36"),
          h6("D1: numeric, first measured diameter [cm], usually at 1.3m"),
          h6("H: numeric, tree height [m]"),
          h6(strong("Optional variables:")),
          h6("H1: numeric, measuring height of D1; defaults to zero, i.e. 1.3m"),
          h6("D2: numeric, second/upper diameter"),
          h6("H2: numeric, measuring height of second/upper diameter")
        ),#end sidebar panel
        mainPanel(
          sidebarLayout(
            sidebarPanel(strong("Preview of the uploaded data:"),
                         tableOutput(ns("prev_table"))),
            mainPanel()
          )# end 2nd sidebarLayout
        )#end main panel
      )#end sidebar layout

    )#end fluid row


  )#end taglist
}

#' input_data Server Functions
#'
#' @noRd
mod_input_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_input <- reactive({
      inFile <- input$target_upload
      if(is.null(inFile)){
        #return(NULL)
        number <- 15
        df <- data.frame(spp = rep(c(15,1), number), D1 = seq(20, 50, length.out = number),
                         H = seq(15, 40, length.out = number) )
      }else{
        df <- read.csv(inFile$datapath, header = TRUE, sep = input$colseparator, dec= input$decseparator)
      }
      list(df=df, sep = input$colseparator, dec= input$decseparator, name = input$target_upload$name)

    })
    
    output$prev_table <- renderTable({head(data_input()$df)})

    return(reactive(data_input()))


  })

}

## To be copied in the UI
# mod_input_data_ui("input_data_1")

## To be copied in the server
# mod_input_data_server("input_data_1")
