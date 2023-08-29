# Chapter 3: Introduction to SCR ----------------------------------------------------------------------

# load libraries --------------------------------------------------------------------------------------

libraries <- c("shiny", "tidyverse", "DT", "ggimage", "shinyWidgets", "shinydashboard",
               "secr", "plotly", "ggplotify", "tippy",'ggforce',"ggalt","proxy","metR",
               "viridis","learnr","DT","acre")

lapply(libraries, require, character.only = TRUE)

# Images for plotting 

micro_image <- "images/micro.png"

#  Objects for constant p and half normal detection function ----------------------------------------------
# Sequences of distances
dist_seq <- seq(0, 100, 0.01)

# constant detection probability dataframe
const_p_dat <- data.frame(x=dist_seq,y=0.25)


# Half-Normal Detection Function
halfnormal <- function(distance, g0, sigma) {
  g0 * exp(-distance^2 / (2 * sigma^2))
}

# detection probabilities
probs <- halfnormal(dist_seq,1,20) 

# dataframe for plotting 
hn_detfunc_dat<-data.frame(dist=dist_seq,probs=probs) 

# Objects for detection function shiny app ---------------------------------------------------------

# Additional detection Functions

# Hazard Rate Detection Function
hazard_rate <- function(distance, g0, sigma, z) {
  g0 * (1 - exp(-(distance / sigma)^(-z)))
}

# Exponential Detection Function
exponential <- function(distance, g0, sigma) {
  g0 * exp(-distance / sigma)
}

# Uniform Detection Function
uniform <- function(distance, g0, sigma) {
  ifelse(distance <= sigma, g0, 0)
}

# Hazard Half-Normal Detection Function
hazard_halfnormal <- function(distance, g0, sigma) {
  1 - exp(-g0 * exp(-distance^2 / (2 * sigma^2)))
}

# Names of detection functions fordropdown list in shiny app
detfunctions <- c("Halfnormal", "Hazard Rate", "Exponential", "Uniform", "Hazard Halfnormal")

# load data ------------------------------------------------------------------------------------------

# list of objects loaded: 
# all acre models and density plots 
# linear_mod
# quad_mod
# noise_mod
# forest_mod
# forest_vol_data

load("data/CH3.RData")
