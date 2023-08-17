# Chapter 3 RScript --------------------------------------------------------------------------------
# Required Libraries
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(ggplotify)
library(plotly)
library(secr)
library(ggforce)
library(ggalt)
library(proxy)
library(acre)
library(ggimage)
library(metR)
library(viridis)
library(learnr)


micro_image <- "images/micro.png"

det_array <- make.grid(
  nx = 3, ny = 3, spacing = 50, detector =
    "proximity"
)

trapdf <- data.frame(det_array) # dataframe for plotting


# Objects for constant p and half normal detection function ----------------------------------------------
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

# Density Model --------------------------------------------------------------------------------------------------

set.seed(1908) # set seed
temppop <- sim.popn(D = 100, expand.grid(x = c(0, 100), y = c(0, 100)), buffer = 50)

ggplot(temppop,aes(x=x,y=y)) +
  geom_point()+
  labs(
    x = "\nX",
    y="Y\n"
  )+
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#c1e0cb"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 10)

## Inhomogenous density --------------------------------------------------------------------------

# using simulated datasets from acre package, specifically "ihd"
acre::ihd

# mask 
ihdens_mask<-create.mask(ihd$traps,buffer=30)

# Data setup 
ihdens_data <- read.acre(captures = ihd$capt, 
                         traps = ihd$traps,
                         control_create_mask = list(buffer = 30),
                         loc_cov = ihd$loc_cov)

# function that fits acre model for ihd dataset given different density model formulas
# acre_data is the data object created by read.acre above 
# dens_mod is a model formula that has to start with ~ e.g. "~x+y"

# section is commented out so not to run everytime tutorial loads 

# ihd_data_plot <- function(acre_data,dens_mod,save.fit=FALSE){
# 
#   model_fit <- fit.acre(acre_data,list(D=as.formula(dens_mod)),"hn",fix=list(g0=1))
# 
#   # dataframe for plotting
#   pred_data<-data.frame(model_fit$D.mask)
#   colnames(pred_data) <- "Density"
#   pred_data$X <- as.numeric(acre_data$mask[[1]][,1])
#   pred_data$Y <- as.numeric(acre_data$mask[[1]][,2])
# 
#   if(save.fit == TRUE){
#     return(list(model_fit = model_fit, pred_data = pred_data))
#   } else{
#     return(pred_data)
#   }
# }
# 
# # different density models
# linear_mod <- ihd_data_plot(ihdens_data,"~x+y",save.fit = TRUE) # linear trend
# quad_mod <- ihd_data_plot(ihdens_data,"~x+y+x^2+y^2+x*y",save.fit = TRUE) # quadratic trend
# noise_mod <- ihd_data_plot(ihdens_data,"~noise",save.fit = TRUE) # continuous covariate noise
# forest_mod <- ihd_data_plot(ihdens_data,"~forest_volumn",save.fit = TRUE) # forest volume categorical covariate
# 
# save(linear_mod,quad_mod,noise_mod,forest_mod, file = "data/dens_model.RData" )

load("data/dens_model.RData")
