#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(secr)
library(tidyverse)
library(acre)
library(metR)
library(viridis)

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

ihd_data_plot <- function(acre_data,dens_mod,save.fit=FALSE){
  
  model_fit <- fit.acre(acre_data,list(D=as.formula(dens_mod)),"hn",fix=list(g0=1))
  
  # dataframe for plotting
  pred_data<-data.frame(model_fit$D.mask)
  colnames(pred_data) <- "Density"
  pred_data$X <- as.numeric(acre_data$mask[[1]][,1])
  pred_data$Y <- as.numeric(acre_data$mask[[1]][,2])
  
  if(save.fit == TRUE){
    return(list(model_fit = model_fit, pred_data = pred_data))
  } else{
    return(pred_data)
  }
}

# different density models 
linear_mod <- ihd_data_plot(ihdens_data,"~x+y") # linear trend
quad_mod <- ihd_data_plot(ihdens_data,"~x+y+x^2+y^2+x*y") # quadratic trend
noise_mod <- ihd_data_plot(ihdens_data,"~noise",save.fit = TRUE) # continuous covariate noise
forest_mod <- ihd_data_plot(ihdens_data,"~forest_volumn") # forest volume categorical covariate

ui <- fluidPage(
  # use shiny feedback
  tabsetPanel(
    tabPanel("Linear trend", 
             fluid = TRUE,
             plotOutput("linearPlot")),
    tabPanel("Quadratic trend", 
             fluid=TRUE,
             plotOutput("quadPlot")),
    tabPanel("Noise", 
             fluid = TRUE,
             fluidRow(
               column(6, plotOutput("noisePlot")),
               column(6, plotOutput("noiseDensityPlot"))
             )),
    tabPanel("Forest cover", 
             fluid = TRUE,
             plotOutput("forestPlot") )
  )
)

server <- function(input, output) {
  output$linearPlot <- renderPlot({
    
    ggplot(linear_mod) + 
      aes(x = X, y = Y, z = Density,fill = Density) + 
      geom_tile() + 
      geom_contour(color = "grey", alpha = 0.5) +
      geom_text_contour(aes(z = Density),stroke = 0.1,colour="grey") + 
      coord_equal() +
      scale_fill_viridis(option="H", na.value="white") + 
      theme_bw()
  })
  
  output$quadPlot <- renderPlot({
    
    ggplot(quad_mod) + 
      aes(x = X, y = Y, z = Density,fill = Density) + 
      geom_tile() + 
      geom_contour(color = "grey", alpha = 0.5) +
      geom_text_contour(aes(z = Density),stroke = 0.1,colour="grey") + 
      coord_equal() +
      scale_fill_viridis(option="H", na.value="white") + 
      theme_bw()
  })
  
  
  output$noisePlot <- renderPlot({
    # noise covariate data 
    noise_data <- noise_mod$model_fit$all.covariates
    colnames(noise_data)[3]<-"Noise"
    
    ggplot(noise_data) + 
      aes(x = x, y = y, z = Noise,fill = Noise) + 
      geom_tile() + 
      geom_contour(color = "grey", alpha = 0.5) +
      geom_text_contour(aes(z = Noise),stroke = 0.1,colour="grey") + 
      coord_equal() +
      scale_fill_distiller(palette="Spectral") + 
      theme_bw()
    
  })
  
  
  output$noiseDensityPlot <- renderPlot({
    
    ggplot(noise_mod$pred_data) + 
      aes(x = X, y = Y, z = Density,fill = Density) + 
      geom_tile() + 
      geom_contour(color = "grey", alpha = 0.5) +
      geom_text_contour(aes(z = Density),stroke = 0.1,colour="grey") + 
      coord_equal() +
      scale_fill_viridis(option="H", na.value="white") + 
      theme_bw()
  })
  
  output$forestPlot <- renderPlot({
    
    ggplot(forest_mod) + 
      aes(x = X, y = Y, z = Density,fill = Density) + 
      geom_tile() + 
      geom_contour(color = "grey", alpha = 0.5) +
      geom_text_contour(aes(z = Density),stroke = 0.1,colour="grey") + 
      coord_equal() +
      scale_fill_viridis(option="H", na.value="white") + 
      theme_bw()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
