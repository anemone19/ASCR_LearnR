# Chapter 3: Introduction to SCR ----------------------------------------------------------------------

# load libraries --------------------------------------------------------------------------------------

libraries <- c("shiny", "tidyverse", "DT", "ggimage", "shinyWidgets", "shinydashboard",
               "secr", "plotly", "ggplotify", "tippy",'ggforce',"ggalt","proxy","metR",
               "viridis","learnr","DT","acre")

lapply(libraries, require, character.only = TRUE)


# load data ------------------------------------------------------------------------------------------

# list of objects loaded: 
# all acre models and density plots 
# linear_mod
# quad_mod
# noise_mod
# forest_mod
# forest_vol_data

load("data/CH3.RData")
