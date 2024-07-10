#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(tableOutput(ns("table")),
             width = 12,
             style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
    ),#end fluid row
    br(),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(ns("Columns"),"Select variables to download",
                             choices = c("Standing stock" = "stock",
                               "Assortment" = "assort",
                               "Aboveground-biomass" = "agb",
                               "Component biomass" = "comp") ),
          br(),
          strong("Download results"),
          downloadButton(ns("downloadData"), "Download")
        ),#end sidebar panel
        mainPanel(
          tableOutput(ns("download_table")),
          style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
        )#end main panel

      )#end sidebar layout

    )#end fluid row

  )
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, df, data_input, selected_radiob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- renderTable({df()$df})#end table
    
    observeEvent(input$Columns,{
      if("comp" %in% input$Columns){
        selected_radiob("Components")
      }
    })

    #table for download
    selected_columns <- reactive({
      data <- df()$df
      fixN <- df()$fixN

      if(length(input$Columns)==0){
        return(NULL)
      }else if("comp" %in% input$Columns && ncol(data)<(17+fixN)){
        showNotification("Change in Biomass component estimates", type = "warning")
      }else {
        columns <- numeric(0)

        if("stock" %in% input$Columns){
          columns <- 2
        }
        if("assort" %in% input$Columns){
          columns <- c(columns, 3:(8+fixN+1) )
        }
        if("agb" %in% input$Columns){
          columns <- c(columns, (10+fixN) )
        }
        if("comp" %in% input$Columns){#  && (input$components=="Components" || input$components=="Komponenten")){
          columns <- c(columns, (11+fixN):ncol(data) )
        }

        df <- data[,columns, drop =F]
      }
      return(df)

    })


    output$download_table <- renderTable({ selected_columns() })

    output$downloadData <- downloadHandler(
      filename = function(){
        flnm <- data_input()
        flnm <- flnm$name
        flnm <- strsplit(flnm, split="\\.")[[1]][1]
        paste0(flnm, "_extended.csv")
      },
      content = function(file) {
        df <- selected_columns()
        write.table(df, file, row.names = FALSE, sep = data_input()$sep, dec = data_input()$dec)
      }
    )

  })
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
