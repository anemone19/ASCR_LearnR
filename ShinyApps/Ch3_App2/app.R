# Chapter 3 Shiny App 3 -------------------------------------------------------------------------------------

# Overview
# The app provides an interactive visualization of a detector array (e.g., microphones) and a surrounding 
# buffer zone. This buffer zone is adjustable, allowing users to visualize how it changes in response to 
# varying distances.

# Key Features:

# Adjustable Buffer Zone: The user can adjust the size of the buffer zone around the detector array using a slider.
# Interactive Visualization: The visualization updates in real-time, reflecting changes to the buffer zone's size.
# Distinct Markers: The detectors are represented using custom icons (microphones), 
# while the buffer zone is depicted using points.

# Required libraries 

library(shiny)
library(tidyverse)
library(secr)
library(ggforce)
library(ggalt)
library(proxy)
library(acre)
library(shinyWidgets)
library(shinydashboard)
library(ggimage)

# Setup ----------------------------------------------------------------------------------------------------

micro_image <- "images/micro.png"

det_array <- make.grid(
  nx = 3, ny = 3, spacing = 50, detector =
    "proximity"
)

trapdf <- data.frame(det_array) # dataframe for plotting


# UI ---------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    shinyWidgets::setSliderColor("#97CBA9", 1),
    chooseSliderSkin("Flat", 1),
    sliderInput("buffer",
                "Buffer distance",
                min = 1,
                max = 200,
                value = 100,
                width = "35%"
    ),
    align = "center",
    style = "padding-left:25px;"
  ), 
  fluidRow(
    plotOutput("maskBufferPlot"),
    align="center"
  )
)

# SERVER ---------------------------------------------------------------------------------------------------------

server <- function(input, output) {

  output$maskBufferPlot <- renderPlot({
    mask <- create.mask(traps = trapdf, buffer = input$buffer)
    
    masks_df <- rbind(mask[, 1:2], trapdf)
    masks_df$type <- rep(c("Mask Point", "Detector"), c(nrow(mask), nrow(trapdf)))
    
    
    ggplot() +
      geom_point(data = masks_df, aes(x = x, y = y), alpha = 0.75, colour = "#678BA4") +
      geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.2) +
      theme_minimal() +
      coord_fixed() +
      labs(
        x = "\nX",
        y = "Y"
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#c1e0cb")
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
