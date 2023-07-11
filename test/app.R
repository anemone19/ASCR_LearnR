library(shiny)
library(tidyverse)
library(DT)
library(spatstat)
library(raster)
library(ggimage)
library(shinyWidgets)
library(shinydashboard)

# frog image
frog_image <- "www/frogGraphic.png"
micro_image <- "www/micro.png"

# Detection function
hazard_halfnormal_detection <- function(sigma, lam0, d) {
  prob <- 1 - exp(-lam0 * exp(-d * (2 * sigma^2)))
  return(prob)
}

calculate_prob_succ <- function(dataframe, detector) {
  # Calculate distance between each point and the detector
  dataframe$distance <- sqrt((dataframe$x - detector$x)^2 + (dataframe$y - detector$y)^2)
  
  # Define distance intervals and associated probabilities
  distance_intervals <- seq(0, 5, 0.1)
  probabilities <- hazard_halfnormal_detection(1.5, 15, distance_intervals)
  
  # Create a new column to store the probabilities
  dataframe$prob_succ <- NA
  
  # Create a new column to store the probabilities
  dataframe$det <- NA
  
  # Iterate over each row and assign the corresponding probability based on the distance
  for (i in 1:nrow(dataframe)) {
    distance <- dataframe$distance[i]
    
    # Find the index of the interval where the distance falls
    interval_index <- findInterval(distance, distance_intervals)
    
    # Assign the probability based on the interval index
    dataframe$prob_succ[i] <- probabilities[interval_index]
    
    dataframe$det[i] <- rbinom(1, 1, dataframe$prob_succ[i])
  }
  
  # Remove the distance column
  dataframe$distance <- NULL
  
  # Mutate each element based on the probability using the binomial distribution
  # dataframe <- dataframe %>%
  #   mutate(result = rbinom(n(), size=1, prob_succ))
  
  return(dataframe$det)
}

# Microphone array 

# Define the range of x and y
x_range <- seq(1, 4, by = 1)
y_range <- seq(2, 3, by = 1)

# Create the grid of points
microphones <- expand.grid(x = x_range, y = y_range)

# Define frog module UI
frogUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      6,
      actionButton(ns("addBtn"), "Reveal frogs 🐸", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4"),
      actionButton(ns("detBtn"), "Survey! 🎙", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4"),
      actionButton(ns("startOverBtn"), "Start Over 🔄", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4")
    ),
    column(6,
           actionButton(ns("repeatBtn"), "Repeat x 5000", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4"),
           align = "right"
    )
  )
}

# Define frog module server logic
frogServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Reactive dataframe to store locations of frogs
      points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
      
      # Add frogs when the addBtn is clicked
      observeEvent(input$addBtn, {
        window <- owin(c(0, 5), c(0, 5))
        ppp_object <- rpoispp(2, win = window)
        
        x <- ppp_object$x
        y <- ppp_object$y
        new_points <- data.frame(x, y)
        
        points(new_points)
      })
      
      # Render plot with frogs
      output$plot <- renderPlot({
        ggplot() +
          geom_image(data = points(), aes(x = x, y = y, image = frog_image), size = 0.09) +
          geom_image(data = microphones, aes(x = x, y = y, image = micro_image), size = 0.25) +
          xlim(0, 5) +
          ylim(0, 5) +
          geom_text(aes(label = rownames(points())), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
          theme_minimal() +
          theme(
            legend.position = "top",
            legend.title = element_blank(),
            panel.background = element_rect(fill = "#DDE0AB"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
          )
      })
      
      # Reset points and plot when startOverBtn is clicked
      observeEvent(input$startOverBtn, {
        points(data.frame(x = numeric(0), y = numeric(0)))
      })
      
      return(points)
    }
  )
}

# Define plot module UI
plotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

# Define plot module server logic
plotServer <- function(id, points) {
  moduleServer(
    id,
    function(input, output, session) {
      # Render plot
      output$plot <- renderPlot({
        ggplot(points(), aes(x = x, y = y)) +
          geom_image(image = frog_image, colour = "#8A3A0D", size = 0.09) +
          geom_image(data = microphones, aes(x = x, y = y, image = micro_image), size = 0.25) +
          xlim(0, 5) +
          ylim(0, 5) +
          theme_minimal() +
          theme(
            legend.position = "top",
            legend.title = element_blank(),
            panel.background = element_rect(fill = "#DDE0AB"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
          )
      })
    }
  )
}

# Define capture history module UI
captHistUI <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("capt_hist"))
}

# Define capture history module server logic
captHistServer <- function(id, points) {
  moduleServer(
    id,
    function(input, output, session) {
      # Reactive dataframe to store capture history
      capt_hist <- reactiveVal(matrix(nrow = 10, ncol = 8))
      
      # Render capture history table
      output$capt_hist <- renderDataTable({
        datatable(
          capt_hist(),
          rownames = TRUE,
          class = "cell-border",
          escape = FALSE,
          extensions = "Buttons",
          selection = "single",
          options = list(dom = "t", ordering = FALSE)
        )
      })
      
      return(capt_hist)
    }
  )
}

# Define value boxes module UI
valueBoxesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("trueN"), width = 4),
      valueBoxOutput(ns("estN"), width = 4),
      valueBoxOutput(ns("probD"), width = 4)
    )
  )
}

# Define value boxes module server logic
valueBoxesServer <- function(id, points, capt_hist) {
  moduleServer(
    id,
    function(input, output, session) {
      # Render number of calls value box
      output$trueN <- renderValueBox({
        N <- nrow(points())
        valueBox(N, "Number of calls")
      })
      
      # Render estimated number of calls value box
      output$estN <- renderValueBox({
        ave <- 1 - (1 - mean(capt_hist()))^8
        Nhat <- nrow(capt_hist()) / ave
        valueBox(round(Nhat), "Estimated number of calls")
      })
      
      # Render average probability of detection value box
      output$probD <- renderValueBox({
        probd <- mean(capt_hist())
        valueBox(round(probd, 3), "Average probability of detection")
      })
    }
  )
}

# Define simulation history module UI
simHistUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("sim_hist"))
}

# Define simulation history module server logic
simHistServer <- function(id, points) {
  moduleServer(
    id,
    function(input, output, session) {
      # Render simulation history plot
      output$sim_hist <- renderPlot({
        simulate_surveys <- function(points, micro, nsims = 1000) {
          nhats <- rep(0, nsims)
          for (i in 1:nsims) {
            prob_hist <- sapply(1:nrow(micro), function(j) {
              calculate_prob_succ(points, micro[j, ])
            })
            
            model <- fit.cr(as.data.frame(prob_hist[rowSums(prob_hist) > 0, ]), model = "M0")
            nhats[i] <- model$Nhat[1]
          }
          
          return(list(nhats = nhats, true = nrow(points)))
        }
        
        data5000 <- suppressWarnings(simulate_surveys(points(), microphones))
        
        ggplot(data.frame(nhats = data5000$nhats), aes(x = nhats)) +
          geom_histogram(bins = 10, fill = "#97cba9", colour = "#668ba4") +
          geom_vline(xintercept = data5000$true) +
          theme_minimal() +
          labs(
            x = "\nAbundance estimates",
            y = "Frequency\n"
          ) +
          theme(
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15)
          )
      })
    }
  )
}

# Define main app UI
ui <- fluidPage(
  frogUI("frog_module"),
  
  fluidRow(
    column(6, plotUI("plot_module")),
    column(6,
           captHistUI("capt_hist_module"),
           br(),
           valueBoxesUI("value_boxes_module")
    )
  ),
  
  fluidRow(column(
    12,
    simHistUI("sim_hist_module")
  ))
)

# Define main app server logic
server <- function(input, output, session) {
  # Initialize module variables
  points <- frogServer("frog_module")
  capt_hist <- captHistServer("capt_hist_module", points)
  
  # Call plot module server
  plotServer("plot_module", points)
  
  # Call value boxes module server
  valueBoxesServer("value_boxes_module", points, capt_hist)
  
  # Call simulation history module server
  simHistServer("sim_hist_module", points)
}

# Run the application
shinyApp(ui = ui, server = server)
