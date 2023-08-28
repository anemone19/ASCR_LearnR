# Chapter 2: Probability of detection setup script -------------------------------------------------------

# load libraries -----------------------------------------------------------------------------------------

libraries <- c("shiny", "tidyverse", "DT", "ggimage", "shinyWidgets", "shinydashboard",
               "secr", "plotly", "ggplotify", "tippy")

lapply(libraries, require, character.only = TRUE)

# load images for plotting

frog_image <- "images/frogGraphic.png"
micro_image <- "images/micro.png"

# function(s) ---------------------------------------------------------------------------------------------

# Description:
# This function is designed to visually annotate a relationship between two points (represented by trap and frog)
# on a graphical plot. Specifically, it draws a line segment connecting the two points and 
# places a text label near the starting point (trap) indicating the Euclidean distance between the two points.

# Parameters: 
# trap: A vector or list containing the x and y coordinates of the starting point (typically representing the trap's location).
# frog: A vector or list containing the x and y coordinates of the ending point (typically representing the frog's location).
# det (default = "no"): A string indicating if the annotation should be highlighted. 
# If set to "yes", the line segment and text will be colored dark green; 
# otherwise, they will be colored grey (for the line) and black (for the text).

# Return: 
# A line segment (annotate("segment", ...)) connecting the trap and frog points.
# A text label (annotate("text", ...)) positioned slightly above the trap point, 
# displaying the Euclidean distance between trap and frog.

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

# load data --------------------------------------------------------------------------------------------

# list of objects in CH2_1: 
# temppop - simulated population
# CH_combined - capture histories of sim population
# det_array - detector coordinates
# det_dat - dataframe of detections 
# trapdf - trapdf coordinates dataframe 

# list of objects in CH2_1: 
# distdf - dataframe containing distances between detectors and frogs
# prop - datatframe containing prob of detection for distance bins 
# captrues - captures dataframe in acre format 

load("data/CH2_1.RData")
load("data/CH2_2.RData")
