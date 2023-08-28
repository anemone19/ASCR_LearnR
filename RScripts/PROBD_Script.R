# Chapter 2 RScript --------------------------------------------------------------------------------
# Required Libraries

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

# Global objects --------------------------------------------------------------------------------

# load images for plotting
frog_image <- "images/frogGraphic.png"
micro_image <- "images/micro.png"

# Function(s) -----------------------------------------------------------------------------------

e2dist <- function(x, y) {
  if (!is.matrix(x)) x <- as.matrix(x)
  if (!is.matrix(y)) y <- as.matrix(y)

  i <- sort(rep(1:nrow(y), nrow(x)))
  dvec <- sqrt((x[, 1] - y[i, 1])^2 + (x[, 2] - y[i, 2])^2)
  matrix(dvec, nrow = nrow(x), ncol = nrow(y), byrow = F)
}

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

# Setup Shiny App 1 -------------------------------------------------------------------

# Created microphone array with make.grid() from package secr
#  9 microphones, 50 units apart, detector is of type proximity

det_array <- make.grid(
  nx = 3, ny = 3, spacing = 50, detector =
    "proximity"
)

trapdf <- data.frame(det_array) # dataframe for plotting

# Objects for frogPopPlot -------------------------------------------------------------------

# simulate population using functions from secr package
set.seed(1908) # set seed
temppop <- sim.popn(D = 5, expand.grid(x = c(0, 100), y = c(0, 100)), buffer = 50)

# generate capture histories with halfnormal detection function, g0 = 1, sigma = 20

CH <- sim.capthist(det_array,
  detectpar = list(g0 = 1, sigma = 20),
  noccasions = 1,
  popn = temppop, renumber = FALSE
)

# modify capture histories
CH_combined <- matrix(NA, nrow = dim(CH)[1], ncol = dim(CH)[3])
rownames(CH_combined) <- names(CH[, , 1])

for (i in 1:9) {
  CH_combined[, i] <- CH[, , i]
}

# create dataframe for first plot (frogPopPlot) indicating which which point in tempop was detected
# add row number as column

det_dat <- temppop %>%
  mutate(
    det = ifelse(row_number() %in% rownames(CH_combined), "Detected", "Not Detected"),
    num = row_number()
  )

save(temppop,CH_combined,det_array,det_dat,trapdf,file = "data/CH2_1.RData")

# Setup Shiny App 3 ----------------------------------------------------------------------

#calculate distances between all frogs and detectors
distances <- t(e2dist(det_array, temppop))
distances_df <- data.frame(d = as.numeric(distances))

# distances between detected frogs and all detectors
all_detect_distances <- distances[as.numeric(rownames(CH_combined)), ]

# extract only distances of detected frogs, i.e. where CH_combined = 1

detect_distances <- c()

# Checking if entry in df1 is equal to 1 and extracting corresponding entry from df2
for (i in 1:nrow(CH_combined)) {
  for (j in 1:ncol(CH_combined)) {
    if (CH_combined[i, j] == 1) {
      detect_distances <- c(detect_distances, all_detect_distances[i, j])
    }
  }
}

# create dataframe with detected distances,
# and frog locations of all not detected that were within 50 m from a microphone
# detect_distances_df <- data.frame(d = detect_distances)

distdf <- data.frame(
  dist = c(detect_distances, as.numeric(distances)),
  group = c(rep("Detected", length(detect_distances)), rep("All", nrow(distances_df)))
)

# Proportions plot

# Cut distances into intervals
# Define the intervals
intervals <- seq(0, 50, 10)

interval_counts_det <- table(cut(subset(distdf$dist, 
                                        distdf$group == "Detected"),
                                 breaks = intervals,
                                 include.lowest = TRUE))

interval_counts_Ndet <- table(cut(subset(distdf$dist, 
                                         distdf$group == "All"),
                                  breaks = intervals,
                                  include.lowest = TRUE))

props <- as.numeric(interval_counts_det / interval_counts_Ndet)

# Given dataframe 'prop'
prop <- data.frame(
  Var1 = c(5,15,25,35,45),
  Freq = props
)

# Setup Shiny App 2 ----------------------------------------------------------------------

# Create a matrix of row and column indices where capt == 1
indices <- which(as.data.frame(CH_combined) == 1, arr.ind = TRUE)

# Extract the row and column indices
ID <- rownames(indices)
traps_ID <- indices[, 2]

# Create a dataframe with the results
captures <- data.frame(ID, trap = traps_ID) %>%
  arrange(ID)

save(distdf,prop,captures,file="data/CH2_2.RData")

