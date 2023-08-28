#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

micro_image <- "images/micro.png"

det_array <- make.grid(
  nx = 3, ny = 3, spacing = 50, detector =
    "proximity"
)

trapdf <- data.frame(det_array) # dataframe for plotting


# Define UI for application that draws a histogram
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

# Define server logic required to draw a histogram
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
