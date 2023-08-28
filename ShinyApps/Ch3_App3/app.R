# Chapter 3 Shiny App 3 ------------------------------------------------------------------------------------

# Overview: 
# The app provides an interactive visualization to explore how varying the density 
# (number of locations per hectare) impacts the distribution of simulated points within a defined space.

# Key Features:
# Density Slider: Allows users to adjust the density of simulated points in a given space. 
# The density is specified as the number of locations per hectare, and the user can select values ranging from 0 to 200.
#  Interactive Visualization: Displays a scatter plot of simulated points. 
# The distribution of these points changes based on the selected density value.

# required libraries
library(shiny)
library(plotly)
library(secr)
library(tidyverse)
library(shinyWidgets)

# UI ------------------------------------------------------------------------------------------------------------

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

# SERVER ---------------------------------------------------------------------------------------------------------

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