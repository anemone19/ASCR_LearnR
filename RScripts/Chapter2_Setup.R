# Chapter 2 RScript --------------------------------------------------------------------------------
# Required Libraries

library(shiny)
library(tidyverse)
library(DT)
library(ggimage)
library(shinyWidgets)
library(shinydashboard)
library(mt5751a)
library(secr)
library(RColorBrewer)
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

# Objects for first shiny app -------------------------------------------------------------------

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


# Additional objects for second shiny app ------------------------------------------------------

# calculate distances between all frogs and detectors
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

interval_counts_det <- table(cut(subset(distdf$dist, distdf$group == "Detected"), breaks = intervals, include.lowest = TRUE))

interval_counts_Ndet <- table(cut(subset(distdf$dist, distdf$group == "All"), breaks = intervals, include.lowest = TRUE))

props <- as.numeric(interval_counts_det / interval_counts_Ndet)
# save(prop, file = "data/prop.RData")

# Given dataframe 'prop'
prop <- data.frame(
  Var1 = c(5,15,25,35,45),
  Freq = props
)

# selected_row <- det_ind[3,]
# 
# # row_name <- rownames(selected_row)
# # row_num <- substr(row_name, nchar(row_name), nchar(row_name))
# 
# det_dat <- selected_row %>%
#   mutate(
#     det = ifelse(row_number() %in% rownames(CH), "Detected", "Not Detected"),
#     num = row_number()
#   )
# 
# det_dat <- points() %>%
#   mutate(
#     det = ifelse(row_number() %in% rownames(cpt_rv()), "Detected", "Not Detected"),
#     num = row_number()
#   )
# 
# ggplot(det_dat) +
#   geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
#   geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.25) +
#   geom_text(aes(x = x, y = y, label = num), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
#   scale_color_manual(values = c(
#     "Detected" = "darkgreen",
#     "Not Detected" = "darkred"
#   )) +
#   xlim(-50, 150) +
#   ylim(-50, 150) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     panel.background = element_rect(fill = "#c1e0cb"),
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     legend.text = element_text(size = 12)
#   )
# 
# 
