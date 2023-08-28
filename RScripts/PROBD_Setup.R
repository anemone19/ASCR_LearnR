# test 

# libraries

library(shiny)
library(tidyverse)
library(DT)
library(ggimage)
library(shinyWidgets)
library(shinydashboard)
library(secr)
library(plotly)
library(ggplotify)
library(tippy)



# load images for plotting
frog_image <- "images/frogGraphic.png"
micro_image <- "images/micro.png"

# function 


add_annotation <- function(trap, frog, det = "no") {
  x1 <- unlist(trap[1]) # x1/x2/y1/y2 defined here for shorthand later
  x2 <- unlist(frog[1])
  y1 <- unlist(trap[2])
  y2 <- unlist(frog[2])
  
  if (det == "yes") {
    lineCol <- "darkgreen"
    textCol <- "darkgreen"
  } else {
    lineCol <- "grey"
    textCol <- "black"
  }
  
  # the function will return the last object it creates, ie this list with two objects
  list(
    annotate("segment",
             color = lineCol,
             x = x1, xend = x2,
             y = y1, yend = y2
    ),
    annotate("text",
             color = textCol, size = 4.5,
             x = x1, y = y1 + 13,
             label = paste(
               round(sqrt((x1 - x2)^2 + (y1 - y2)^2), digits = 1)
             )
    )
  )
}

# data 

load("data/CH2_1.RData")
load("data/CH2_2.RData")
