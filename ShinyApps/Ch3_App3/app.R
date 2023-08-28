#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(secr)
library(tidyverse)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    shinyWidgets::setSliderColor("#97CBA9", 1),
    chooseSliderSkin("Flat", 1),
    sliderInput("density",
                "Density (number of locations per hectare)",
                value = 25,
                min = 0,
                max = 200,
                width = "35%"),
    align = "center",
    style = "padding-left:26px;"
  ), 
  fluidRow(
    plotOutput("constantDensityPlot"),
    align="center"
  )
)

server <- function(input,output){
  
  output$constantDensityPlot <- renderPlot({
    set.seed(1908) # set seed
    temppop <- sim.popn(D = input$density, expand.grid(x = c(0, 100), y = c(0, 100)), buffer = 50)
    
    ggplot(temppop, aes(x = x, y = y)) +
      geom_point() +
      labs(
        x = "\nX",
        y = "Y"
      ) +
      coord_fixed() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#c1e0cb"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      ) +
      scale_x_continuous(n.breaks = 15) +
      scale_y_continuous(n.breaks = 10)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)