# Chapter 3 Shiny App 1 -------------------------------------------------------------------------------------

# Overview: 
# This Shiny application provides an interactive visualization tool to explore different detection functions. 
# Detection functions describe the probability of detecting an object (or signal) based on its distance from a sensor 
# (or detector). The user can choose among various detection functions and adjust their parameters to observe 
# how the probability of detection changes with distance.


# Key Features:

# Function Selection: Users can select the desired detection function from a dropdown list.

# Adjustable Parameters: Users can adjust key parameters of the chosen detection function using sliders. 
# For instance, they can modify values like (initial detection probability) and σ (scale parameter). Some 
# functions also have additional parameters likez for the Hazard Rate function.

# Interactive Plot: As parameters are adjusted, the plot updates in real-time to show the corresponding 
# detection function curve.

# required libraries
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(ggplotify)
library(plotly)
# Setup ---------------------------------------------------------------------------------------------------------

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

# UI ------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  chooseSliderSkin("Flat", color = "#ED5565"),
  sidebarLayout(
    sidebarPanel(
      selectInput("detFunc", "Select Function:",
                  choices = c("Halfnormal", "Hazard Rate", "Exponential", "Uniform", "Hazard Halfnormal")
      ),
      sliderInput("g0", "Set value of g0:", min = 0, max = 1, value = 1),
      sliderInput("sigma", "Set value for σ", min = 0, max = 20, value = 5),
      conditionalPanel(
        condition = "input.detFunc == 'Hazard Rate'",
        sliderInput("z", "Set value for z", min = 0, max = 1, value = 0.5)
      )
    ),
    mainPanel(
      plotlyOutput("detFuncPlot")
    )
  )
)


# SERVER ------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  observeEvent(input$detFunc == detfunctions[5], {
    updateSliderInput(
      session = session,
      inputId = "g0",
      label = "Set value for λ0",
      max = 5
    )
  }, ignoreInit = TRUE)
  
  output$detFuncPlot <- renderPlotly({
    if (input$detFunc == detfunctions[1]) {
      y <- halfnormal(dist_seq, input$g0, input$sigma)
    } else if (input$detFunc == detfunctions[2]) {
      y <- hazard_rate(dist_seq, input$g0, input$sigma, input$z)
    } else if (input$detFunc == detfunctions[3]) {
      y <- exponential(dist_seq, input$g0, input$sigma)
    } else if (input$detFunc == detfunctions[4]) {
      y <- uniform(dist_seq, input$g0, input$sigma)
    } else if (input$detFunc == detfunctions[5]) {
      y <- hazard_halfnormal(dist_seq, input$g0, input$sigma)
    }
    
    # Create data frame
    df <- data.frame(Distance = dist_seq, Probd = y)
    df$Probability <- round(df$Probd,2) # just for ggplotly label
    
    # Plot
    ggplotly(
      ggplot(df, aes(Distance, Probd, label = Probability)) +
        geom_line(linewidth = 0.7, colour = "#97CBA9") +
        labs(title = paste(input$detFunc, "Detection Function")) +
        xlab("\nDistance") +
        ylab("Detection probability\n") +
        ylim(0, 1) +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 15)
        ),
      tooltip = c("Distance","label")
    ) 
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)
