library(deSolve)                # v 1.21
library(ggplot2)                # v 3.1.1
library(shiny)                  # v 1.3.2
library(dplyr)                  # v 0.8.99.9002
library(tidyr)                  # v 0.8.3
library(cowplot)                # v 0.9.4
library(grid)
library(shinydashboard)         # v 0.7.1
library(shinyWidgets)           # v 0.5.3
library(png)

# R version 3.5.3

## Load ODE equations, plotting functions, and tabs  -------------------------------------------------

source("odes.R")
source("plotting.R")

source("tab_about.R")
source("tab_original.R")


## Define shiny app --------------------------------------------------------------------------

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(
        title = "Vaccine nationalism and the dynamics of SARS-CoV-2",
        titleWidth = 520
    ),
    dashboardSidebar(
        width = 240,
        tags$style(type = 'text/css',".badge{min-width: 140px; font-size: 15px;}"),
        sidebarMenu(id = "tabs",
                    menuItem("About", tabName = "about", icon = icon("info-circle")),
                    menuItem(tabName = "original", text = HTML('<b style="color:#EF4A4A;">Interactive plots</b>'), 
                             icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        
        # Style edits
        tags$head( 
            tags$style(HTML(".main-sidebar{ font-size: 17px;} 
                            .skin-blue .left-side, .skin-blue .wrapper {background-color: #ecf0f5;}")) 
        ),
        tags$style(
            HTML('.box{font-size: 17px; text-align: left; color: #000000;}')
        ),
        tags$head(
            tags$style(type ='text/css', 
                       ".nav-tabs {font-size: 17px} 
                        .irs-grid-text {font-size: 12px;} .irs-max {font-size: 12px;} .irs-min {font-size: 12px;}
                        .gobutton .bttn-danger{background-color: #EF4A4A;}") 
        ), 
        tags$script("
        Shiny.addCustomMessageHandler('plotVaccine', function(value) {
        Shiny.setInputValue('plotVaccine', value);
        });
        "),
        tabItems(
            tab_about,
            tab_original 
        )
    )
)

server <- function(input, output, session) {
    session$sendCustomMessage("plotVaccine", 'TRUE')
  
    observeEvent(input$go1, {
        session$sendCustomMessage("plotVaccine", 'TRUE')
        
        if (input$switch != "no"){
            session$sendCustomMessage("plotVaccine", 'FALSE')
        } 
    })
    
    newres <- eventReactive(c(input$go1), {  
        
        withProgress(message = "Simulating model..", value = 1.0, {             
            
            time <- seq(from = 1, to = 5 * 52, by = 1)
            
            R0.listOrig <- read.table(paste0(input$climate1, ".csv"), header = T, sep = ",")
            
            R0.list.a <- 2.3 * R0.listOrig$hku1/(mean(R0.listOrig$hku1))
            R0.list1 <- rep(R0.list.a, length = length(time))
            
            R0.listOrig <- read.table(paste0(input$climate2, ".csv"), header = T, sep = ",")
            
            R0.list.a <- 2.3 * R0.listOrig$hku1/(mean(R0.listOrig$hku1))
            R0.list2 <- rep(R0.list.a, length = length(time))
            
            plots.doses.switch(time, R0.list1, R0.list2, input)
        })
    }, ignoreNULL = FALSE)
    
        output$plot12 <- renderPlot({
            plots <- newres()
        
            plots[[1]]
        })
    
        output$plot12b <- renderPlot({        
            plots <- newres()
            
            plots[[3]]
        })
        
        output$plot13 <- renderPlot({
        plots <- newres()
        
        plots[[2]]
    })
}


## Run shiny app ---------------------------------------------------------------------------------

shinyApp(ui, server)

