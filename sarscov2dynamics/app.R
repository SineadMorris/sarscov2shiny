library(deSolve)                # v 1.21
library(ggplot2)                # v 3.1.1
library(shiny)                  # v 1.3.2
library(dplyr)                  # v 0.8.99.9002
library(tidyr)                  # v 0.8.3
library(cowplot)                # v 0.9.4
library(grid)
library(shinydashboard)         # v 0.7.1
library(shinyWidgets)           # v 0.5.3

# R version 3.5.3

## Load ODE equations, plotting functions, and tabs  -------------------------------------------------

source("odes.R")
source("plotting.R")

source("tab_about.R")
source("tab_original.R")
source("tab_refusal.R")
source("tab_clinical.R")

## Define shiny app --------------------------------------------------------------------------

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(
        title = "Dynamics of SARS-CoV-2 over the next five years",
        titleWidth = 500
    ),
    dashboardSidebar(
        width = 240,
        tags$style(type = 'text/css',".badge{min-width: 140px; font-size: 15px;}"),
        sidebarMenu(id = "tabs",
            menuItem("About", tabName = "about", icon = icon("info-circle")),
            menuItem(text = HTML('<b style="color:#EF4A4A;">Interactive plots</b>'), icon = icon("chart-line"), startExpanded = FALSE,
                     menuSubItem(tabName = "original", text = "No heterogeneity"), #, badgeColor = "red"
                     menuSubItem(tabName = "clinical", text = "Intrinsic heterogeneity"),
                     menuSubItem(tabName = "refusal", text = "Heterogeneity & vaccine refusal")
            )
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
                        .gobutton .bttn-danger{background-color: #EF4A4A;}") ##F04747  #F54B4B #E94949
        ), 
        tabItems(
            tab_about,
            tab_original,
            tab_refusal,
            tab_clinical
            )
        )
)

server <- function(input, output) {
 
    newres <- eventReactive(c(input$go1, input$go2, input$go3), {
        
        withProgress(message = "Simulating model..", value = 1.0, {             
            
            time <- seq(from = 1, to = 5 * 52, by = 1)
            
            R0.listOrig <- read.table(paste0(input$climate, ".csv"), header = T, sep = ",")
            R0.list <- rep(R0.listOrig$hku1, length = length(time))
            
            plots.soc.dis.seasonality.vax.hesitancy(time = time, R0.list, input)
        })
    }, ignoreNULL = FALSE)
    
    output$plot11 <- output$plot21 <- output$plot31 <- renderPlot({
        
        plots <- newres()
        
        plots[[1]]
    })
    
    output$plot12 <- output$plot22 <- output$plot32 <- renderPlot({
        
        plots <- newres()
        
        plots[[2]]
    })
    
    output$plot13 <- output$plot23 <- output$plot33 <- renderPlot({
        
        plots <- newres()
        
        plots[[3]]
    })
}


## Run shiny app ---------------------------------------------------------------------------------

shinyApp(ui, server)

