#' histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr %>%
mod_histogram_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(3, numericInput(ns("lx"), "lx:", 0)),
            column(3, numericInput(ns("Zsh"), "Zsh:", 0)),
            column(3, numericInput(ns("Zab"), "Zab:", 0)),
            column(3, numericInput(ns("Sokz"), "Sokz:", 1)),
            column(3, numericInput(ns("Az"), "Az:", 0)),
            column(3, numericInput(ns("fixN"), "fixN:", 0)),
            column(3, numericInput(ns("fixZ"), "fixZ:", 0)),
            column(3, numericInput(ns("fixL"), "fixL:", 0)),
            column(3, numericInput(ns("fixA"), "fixA:", 0)),
            column(3, numericInput(ns("fixR"), "fixR:", 0)),
            column(3, numericInput(ns("Hsh"), "Hsh:", 0)),
            radioButtons(ns("components"),"Biomass component estimates:",choices = c("AGB","Components"), selected="AGB",inline=TRUE)
          )
        ),#end sidebar panel
        mainPanel(
          tabsetPanel(
            tabPanel("Histogram", plotOutput(outputId = ns("histPlot") ),
                     radioButtons(ns("yhat"),"Show results for:", choices = c("Vfm","Efm"), selected="Vfm",inline=TRUE)),
            tabPanel("Taper curve", plotOutput(outputId = ns("taperPlot") ),
                     actionButton(ns("backBtn"), label = icon("arrow-left")),
                     actionButton(ns("nextBtn"), label = icon("arrow-right")))

          )
        )#end main panel
      )#end sidebar layout
    )#end fluid row

  )#end taglist
}

#' histogram Server Functions
#'
#' @noRd
mod_histogram_server <- function(id, data_input){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    require(TapeS)
    library(dplyr)

    data_upload <- reactive({
      if(!is.null(data_input()$df)){
        df <- data_input()
        df <- df$df
        sort <- list(lX = input$lx, Sokz = input$Sokz, Az = input$Az,
                     fixN = input$fixN, fixZ = input$fixZ, fixL = input$fixL,
                     fixA = input$fixA, fixR = input$fixR, Hsh = input$Hsh)

        # Vfm and Efm
        tree <- rBDAT::buildTree(tree = df, check = "assortment", vars = sort)
        tree$Hsh <- ifelse(tree$spp<15, 0, tree$Hsh)
        assortment <- rBDAT::getAssortment(tree, value = "merge")
        assortment2 <- rBDAT::getAssortment(tree, value = "Vol")
        df <- assortment2

        #Get volumes for fixN
        if(input$fixN !=0){
          fix <- rBDAT::getAssortment(tree, value = "Fix")
          fix <- fix[, c("tree", "Sort", "Vol")]
          wide_fix <- fix %>%
            tidyr::pivot_wider(names_from=Sort, values_from = Vol)

          df <- merge(df, wide_fix, by= "tree")
        }

        df$Efm <- df$Vfm - df$EV
        df$Biomass <- rBDAT::getBiomass(tree)

        if(input$components=="Components" | input$components=="Komponenten"){
          tpr <- TapeS::bdat_as_tprtrees(tree)
          cmp <- TapeS::tprBiomass(tpr, component = "all")
          df <- cbind(df,cmp)
        }

        return(list("df"=df, "tree"=tree, "assort"=assortment))

      }

    })

    output$histPlot <- renderPlot({
      data <- data_upload()
      df <- data$df
      if(!is.null(df)){
        hist(df[, input$yhat], main=input$yhat, freq=TRUE, xlab = "Volume")
      }
    })# end histogram

    #TAPER PLOT
    currentPlotIndex <- reactiveVal(1)
    observeEvent(input$backBtn, {currentPlotIndex(max(1, currentPlotIndex()-1))})
    observeEvent(input$nextBtn, {currentPlotIndex(min(nrow(data_upload()$df), currentPlotIndex()+1))})
    generatePlot <- function(data, assort, index, n){
      if(anyNA(assort[((index-1)*n+1):(index*n),]) == F ){
        plot(data[index,], assort=assort[((index-1)*n+1):(index*n),])
      }else{
        showNotification("no volume table values for given stem dimensions (H, D1)", type = "warning")
      }
    }

    output$taperPlot <- renderPlot({
      data <- data_upload()
      tree <- data$tree
      assort <- data$assort
      n <- 5+input$fixN
      ranges <- nrow(tree)

      if(!is.null(tree) && currentPlotIndex() <= ranges){
        generatePlot(tree, assort, currentPlotIndex(), n)
      }
    })

    #return(reactive(data_upload()$df))
    return(reactive(list("fixN"=input$fixN,
                         "df"=data_upload()$df)))

  })#module server
}

## To be copied in the UI
# mod_histogram_ui("histogram_1")

## To be copied in the server
# mod_histogram_server("histogram_1")
