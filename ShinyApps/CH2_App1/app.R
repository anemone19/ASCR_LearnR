# Chapter 2 Shiny App 1 --------------------------------------------------------------------------------

# Overview: 
# This Shiny application simulates the process of detecting frog calls using a grid of microphones.
# The app allows users to add frog calls to a plot, initiate a survey to detect these calls,
# and then visualize the detection results. 
# Additionally, users can simulate the survey process multiple times to observe the distribution of 
# abundance estimates.

# Key Features:

# Visualization: Users can visually place frog calls on a grid and see the positions of microphones.
# Detection: Once frogs are added, users can initiate a survey to detect frog calls. 
# Detection is probabilistic, based on the distance between the frog and each microphone.
# Metrics Display: The app displays key metrics including the true number of frog calls, 
# the estimated number of calls, and the overall probability of detection.
# Simulation: Users can simulate the survey process 1000 times to see the distribution of abundance 
# estimates.
# Interactivity: Users can reset the app to its initial state, repeat the simulation, 
# or select specific rows to highlight particular frogs and the microphones that detected them.


library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(secr)
library(RColorBrewer)
library(ggimage)
library(ggplotify)

# Setup ----------------------------------------------------------------------------------------------
# frog image 

frog_image <- "www/frogGraphic.png"
micro_image <- "www/micro.png"

# Detection function
hazard_halfnormal_detection <- function(sigma, lam0, d) {
  prob <- 1 - exp(-lam0 * exp(-d * (2 * sigma^2)))
  return(prob)
}

# Function
# Description:
# This function calculates the probability of successful detection for each point in a given dataframe 
# based on its distance from a specified detector. 
# The function also simulates detection events for each point based on these probabilities.

# Parameters: 
# dataframe: A dataframe containing the x and y coordinates of the points (e.g., frog calls). 
# The dataframe should have columns named x and y.
# detector: A list or vector containing the x and y coordinates of the detector, named as x and y, respectively.

# Return: 
# A vector of binary values (1 or 0), indicating the simulated detection (1) or non-detection (0) of each point in the dataframe
# based on the calculated probabilities.

calculate_prob_succ <- function(dataframe, detector) {
  # Calculate distance between each point and the detector
  dataframe$distance <- sqrt((dataframe$x - detector$x)^2 + (dataframe$y - detector$y)^2)
  
  # Define distance intervals and associated probabilities
  distance_intervals <- seq(0, 5, 0.1)
  probabilities <- hazard_halfnormal_detection(1.5, 15, distance_intervals)
  
  # Assign probabilities based on the distance intervals
  interval_indices <- findInterval(dataframe$distance, distance_intervals)
  dataframe$prob_succ <- probabilities[interval_indices]
  
  # Simulate detection based on the probabilities
  dataframe$det <- rbinom(nrow(dataframe), 1, dataframe$prob_succ)
  
  return(dataframe$det)
}

# Microphone array 

# Define the range of x and y
x_range <- seq(1, 4, by = 1)
y_range <- seq(2,3, by = 1)

# Create the grid of points
det_array <- make.grid(
  nx = 3, ny = 3, spacing = 50, detector =
    "proximity"
)

trapdf <- data.frame(det_array) # dataframe for plotting

# styles 
button_style <- "color: #fff; background-color: #668ba4; border-color: #2e6da4"

# ggplot theme 
common_theme <- theme(
  legend.position = "top",
  legend.title = element_blank(),
  panel.background = element_rect(fill = "#c1e0cb"),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank()
)


# UI ----------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(
      6,
      actionButton("addBtn", "Reveal frogs", style = button_style),
      actionButton("detBtn", "Survey!", style = button_style),
      actionButton("startOverBtn", "Start Over", style = button_style),
      style = "padding-left:23px;"
    ),
    column(6,
           actionButton("repeatBtn", "Repeat x 1000", style = button_style),
           align = "right"
    )
  ),
  fluidRow(
    column(6, plotOutput("frogPopPlot")),
    column(
      6, dataTableOutput("captHistData"),
      br(),
      fluidRow(
        valueBoxOutput("trueN", width = 4),
        valueBoxOutput("probD", width = 4),
        valueBoxOutput("estN", width = 4)
      )
    )
  ),
  fluidRow(column(
    12,
    conditionalPanel(
      "input.repeatBtn%2 == 1",
      plotlyOutput("simPlot")
    )
  ))
)


# SERVER ----------------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # DEFAULT DISPLAY --------------------------------------------------------------------
  output$frogPopPlot <- renderPlot({
    ggplot() +
      geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.3) +
      xlim(-50, 150) +
      ylim(-50, 150) +
      theme_minimal() +
      common_theme
  })
  
  output$captHistData <- DT::renderDT({
    empty_dat <- matrix(nrow = 10, ncol = 9)
    colnames(empty_dat) <- paste0("M", 1:9)
    
    datatable(empty_dat,
              rownames = TRUE,
              class = "cell-border",
              escape = FALSE,
              extensions = "Buttons",
              selection = "single",
              options = list(dom = "t", ordering = F)
    )
  })
  
  # metrics
  output$trueN <- shinydashboard::renderValueBox({
    N <- 0
    shinydashboard::valueBox(N, "True number of calls")
  })
  
  output$estN <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(0, "Estimated number of calls")
  })
  
  output$probD <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(0, "Overall probability of detection")
  })
  
  # -----------------------------------------------------------------------------------
  # ADD FROGS -------------------------------------------------------------------------
  
  # Create a reactive dataframe to store locations of frogs
  points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  cpt_rv <- reactiveVal(NULL)
  
  # Add frogs when the addBtn is clicked
  observeEvent(input$addBtn, {
    temppop <- sim.popn(D = 5, expand.grid(x = c(0, 100), y = c(0, 100)), buffer = 50)
    popdf <- data.frame(temppop)
    
    points(temppop) # add points to reactive dataframe for use outside observeEvent
  })
  
  # Render new plot with frogs
  output$frogPopPlot <- renderPlot({
    ggplot(points(), aes(x = x, y = y)) +
      geom_image(image = frog_image, colour = "#8A3A0D", size = 0.09) +
      geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.25) +
      xlim(-50, 150) +
      ylim(-50, 150) +
      geom_text(aes(label = rownames(points())), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
      common_theme
  })
  
  output$trueN <- shinydashboard::renderValueBox({
    N <- nrow(points())
    shinydashboard::valueBox(N, "Number of calls")
  })
  
  # -----------------------------------------------------------------------------------
  # ADD DETECTIONS --------------------------------------------------------------------
  
  # Create a reactive dataframe to for capthist dataframe
  capthist <- reactiveVal(NULL)
  
  # Add points when the button is clicked
  observeEvent(input$detBtn, {
    if (is.null(points()) || nrow(points()) == 0) {
      # Display an error if points are empty
      showModal(modalDialog(
        title = "Oops!",
        "Please add frogs before surveying!",
        easyClose = TRUE
      ))
    } else {
      # simulate capture history using points() dataframe, current frog pop
      cpt <- sim.capthist(det_array,
                          detectpar = list(g0 = 1, sigma = 20),
                          noccasions = 1,
                          popn = points(), renumber = FALSE
      )
      
      cpt_rv(cpt) # add to reactive dataframe for use outside observeEvent
      
      prob_hist <- matrix(NA, nrow = dim(cpt)[1], ncol = dim(cpt)[3])
      rownames(prob_hist) <- names(cpt[, , 1])
      
      for (i in 1:9) {
        prob_hist[, i] <- cpt[, , i]
      }
      
      det_ind <- as.data.frame(prob_hist)
      rownames(det_ind) <- paste("Frog", rownames(det_ind))
      colnames(det_ind) <- paste("M", 1:9)
      capthist(prob_hist)
      
        output$captHistData <- DT::renderDT({
    det_ind %>%
      DT::datatable(
        rownames = TRUE,
        class = "cell-border",
        escape = FALSE,
        extensions = "Buttons",
        selection = "single",
        options = list(dom = "t", ordering = F)
      ) %>%
      formatStyle(0, target = "row", color = "black", lineHeight = "50%") %>%
      formatStyle(0, width = "20%")
  })
  
  
  # Render the plot
  output$frogPopPlot <- renderPlot({
    det_dat <- points() %>%
      mutate(
        det = ifelse(row_number() %in% rownames(cpt_rv()), "Detected", "Not Detected"),
        num = row_number()
      )
    
    ggplot(det_dat) +
      geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
      geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.25) +
      geom_text(aes(x = x, y = y, label = num), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
      scale_color_manual(values = c(
        "Detected" = "darkgreen",
        "Not Detected" = "darkred"
      )) +
      xlim(-50, 150) +
      ylim(-50, 150) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#c1e0cb"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 12)
      )
  })
  
  output$trueN <- shinydashboard::renderValueBox({
    N <- nrow(points())
    shinydashboard::valueBox(N, "True number of calls")
  })
  
  output$estN <- shinydashboard::renderValueBox({
    p_star <- 1 - (1 - mean(prob_hist))^nrow(det_array)
    Nhat <- nrow(det_ind) / p_star
    shinydashboard::valueBox(round(Nhat), "Estimated number of calls")
  })
  
  output$probD <- shinydashboard::renderValueBox({
    p_star <- 1 - (1 - mean(prob_hist))^nrow(det_array)
    shinydashboard::valueBox(round(p_star, 2), "Overall probability of detection")
  })
    }
  })
  
  # --------------------------------------------------------------------------------------------------------------
  # ROW SELECTION ------------------------------------------------------------------------------------------------
  
  # when row selected, show frog and microphone which detected call 
  observeEvent(input$captHistData_rows_selected, {
    selected_row <- input$captHistData_rows_selected
    frogID <- rownames(capthist())[selected_row]
    
    # which microphone detected 
    micro_index <- which(capthist()[selected_row,] == 1)
    micro_df <- trapdf[c(micro_index),]
    
    if (length(input$captHistData_rows_selected)){
      
    }
    output$frogPopPlot <- renderPlot({
      if (length(input$captHistData_rows_selected)){
        det_dat <- points() %>%
          mutate(
            det = ifelse(row_number() %in% rownames(cpt_rv()), "Detected", "Not Detected"),
            num = row_number()
          ) %>%
          filter(num == frogID)
        
        ggplot(det_dat) +
          geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
          geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.25) +
          geom_image(data = micro_df, aes(x = x, y = y, image = micro_image), size = 0.25, colour = "darkgreen") +
          geom_text(aes(x = x, y = y, label = num), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
          scale_color_manual(values = c(
            "Detected" = "darkgreen",
            "Not Detected" = "darkred"
          )) +
          xlim(-50, 150) +
          ylim(-50, 150) +
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.background = element_rect(fill = "#c1e0cb"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            legend.text = element_text(size = 12)
          )
      } else {
        det_dat <- points() %>%
          mutate(
            det = ifelse(row_number() %in% rownames(cpt_rv()), "Detected", "Not Detected"),
            num = row_number()
          )
        
        ggplot(det_dat) +
          geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
          geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.25) +
          geom_text(aes(x = x, y = y, label = num), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
          scale_color_manual(values = c(
            "Detected" = "darkgreen",
            "Not Detected" = "darkred"
          )) +
          xlim(-50, 150) +
          ylim(-50, 150) +
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.background = element_rect(fill = "#c1e0cb"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            legend.text = element_text(size = 12)
          )
      }
    })
    
  })

  # ----------------------------------------------------------------------------------------------------------
  # STARTOVER ------------------------------------------------------------------------------------------------
  
  observeEvent(input$startOverBtn, {
    points(data.frame(x = numeric(0), y = numeric(0)))
    
    output$frogPopPlot <- renderPlot({
      ggplot() +
        geom_image(data = trapdf, aes(x = x, y = y, image = micro_image), size = 0.25) +
        xlim(-50, 150) +
        ylim(-50, 150) +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          panel.background = element_rect(fill = "#c1e0cb"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )
    })
    
    output$simPlot <- renderPlot({
      NULL
    })
    
    output$captHistData <- DT::renderDT({
      empty_dat <- matrix(nrow = 10, ncol = 9)
      colnames(empty_dat) <- paste0("M", 1:9)
      
      datatable(empty_dat,
                rownames = TRUE,
                class = "cell-border",
                escape = FALSE,
                extensions = "Buttons",
                selection = "single",
                options = list(dom = "t", ordering = F)
      )
    })
    
    output$trueN <- shinydashboard::renderValueBox({
      N <- 0
      shinydashboard::valueBox(N, "True number of calls")
    })
    
    output$estN <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(0, "Estimated number of calls")
    })
    
    output$probD <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(0, "Overall probability of detection")
    })
  })
  
  # --------------------------------------------------------------------------------------------------
  # Simulation ---------------------------------------------------------------------------------------
  
  observeEvent(input$repeatBtn, {
    if (is.null(points()) || nrow(points()) == 0) {
      # Display an error if points are empty
      showModal(modalDialog(
        title = "Oops!",
        "Please add frogs before surveying!",
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog("Surveying!", footer = NULL))
      simulate_surveys <- function(points, micro, nsims = 1000) {
        nhats <- rep(0, nsims)
        for (i in 1:nsims) {
          # capture histories
          CH <- sim.capthist(micro,
                             detectpar = list(g0 = 1, sigma = 20),
                             noccasions = 1,
                             popn = points, renumber = FALSE
          )
          
          CH_combined <- matrix(NA, nrow = dim(CH)[1], ncol = dim(CH)[3])
          rownames(CH_combined) <- names(CH[, , 1])
          
          for (j in 1:9) {
            CH_combined[, j] <- CH[, , j]
          }
          
          ave <- 1 - (1 - mean(CH_combined))^nrow(micro)
          Nhat <- nrow(CH_combined) / ave
          
          nhats[i] <- Nhat
        }
        
        return(list(nhats = nhats, true = nrow(points())))
      }
      
      data5000 <- suppressWarnings(simulate_surveys(points(), det_array))
      
      output$simPlot <- renderPlotly({
        ggplotly(
          ggplot(data.frame(nhats = data5000$nhats), aes(x = nhats)) +
            geom_histogram(bins = 10, fill = "#97cba9", colour = "#668ba4") +
            geom_vline(aes(colour = "True_Number", xintercept = data5000$true)) +
            scale_color_manual(name = "", values = c(True_Number= "#C86F35")) +
            theme_minimal() +
            labs(
              x = "\nAbundance estimates",
              y = "Count\n"
            ) +
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10)
            ),
          tooltip = c("y"), showlegend = F
        ) %>%
          layout(legend = list(
            orientation = "h"
          ))
      })
      removeModal()
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)