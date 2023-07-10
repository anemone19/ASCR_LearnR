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
y_range <- seq(2,3, by = 1)

# Create the grid of points
microphones <- expand.grid(x = x_range, y = y_range)


# Define UI
ui <- fluidPage(
  fluidRow(
    column(
      6,
      actionButton("addBtn", "Add Points"),
      actionButton("detBtn", "Detect!"),
      actionButton("startOverBtn", "Start Over")
    ),
    column(6,
           actionButton("repeatBtn", "Repeat x 1000"),
           align="right")
  ),
  
  fluidRow(
    column(6, plotOutput("plot")),
    column(
      6, dataTableOutput("capt_hist"),
      br(),
      fluidRow(valueBoxOutput("trueN", width = 4),
               valueBoxOutput("estN", width = 4),
               valueBoxOutput("probD", width = 4))
    )
  ),
  
  fluidRow(column(12,
                  plotOutput("sim_hist")))
  
)

# Define server logic
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot() +
      geom_image(data = microphones, aes(x = x, y = y, image = micro_image), size = 0.3) +
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
  
  output$capt_hist <- DT::renderDT({
    empty_dat <- matrix(nrow = 10, ncol = 8)
    colnames(empty_dat) <- paste0("M", 1:8)
    
    datatable(empty_dat,
              rownames = TRUE,
              class = "cell-border",
              escape = FALSE,
              extensions = "Buttons",
              selection = "single",
              options = list(dom = "t", ordering = F)
    )
  })
  
  
  # Create a reactive dataframe to store locations of frogs
  points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Add frogs when the addBtn is clicked
  observeEvent(input$addBtn, {
    window <- owin(c(0, 5), c(0, 5))
    ppp_object <- rpoispp(1, win = window)
    
    x <- ppp_object$x
    y <- ppp_object$y
    new_points <- data.frame(x, y)
    
    points(new_points)
    
    # Render new plot with frogs
    output$plot <- renderPlot({
      ggplot(points(), aes(x = x, y = y)) +
        geom_image(image = frog_image, colour = "#8A3A0D", size = 0.09) +
        geom_image(data = microphones, aes(x = x, y = y, image = micro_image), size = 0.25) +
        xlim(0, 5) +
        ylim(0, 5) +
        geom_text(aes(label = rownames(points())), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
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
    
    output$trueN <- shinydashboard::renderValueBox({
      N <- nrow(points())
      shinydashboard::valueBox(N, "Number of calls")
    })
  })
  
  # create reactive dataframe for capture histories 
  prob_hist <- reactiveVal({matrix(NA, nrow = 10, ncol = nrow(microphones))})

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
      
      # Calculate probabilities
      
      caphists <- matrix(NA, nrow = nrow(points()), ncol = nrow(microphones))
      for (i in 1:nrow(microphones)) {
        caphists[, i] <- calculate_prob_succ(points(), microphones[i, ])
      }
      
      prob_hist(caphists)
      
      det_ind <- as.data.frame(prob_hist())
      rownames(det_ind) <- paste("Frog", rownames(det_ind))
      colnames(det_ind) <- paste("M", 1:8)
      det_ind <- det_ind[rowSums(det_ind[, 1:8]) > 0, ]
      
      output$capt_hist <- DT::renderDT({
        
        det_ind %>%
          DT::datatable(
            rownames = TRUE,
            class = "cell-border",
            escape = FALSE,
            extensions = "Buttons",
            selection = "single",
            options = list(dom = "t", ordering = F)
          )%>%
          formatStyle( 0, target= 'row',color = 'black', lineHeight='50%')
      })
      
      
      # Render the plot
      output$plot <- renderPlot({
        dat <- cbind(points(), prob_hist())
        
        det_dat <- dat %>%
          rowwise() %>%
          mutate(
            sum = sum(c_across(3:10)),
            det = ifelse(sum > 0, "Detected", "Not Detected")
          )
        
        det_dat <- det_dat %>%
          data.frame() %>%
          mutate(num = row_number())
        
        
        ggplot(det_dat) +
          geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
          geom_image(data = microphones, aes(x = x, y = y, image = micro_image), size = 0.25) +
          xlim(0, 5) +
          geom_text(aes(x = x, y = y, label = num), vjust = 0.5, hjust = 0.5, colour = "white", size = 4, fontface = "bold") +
          scale_color_manual(values = c(
            "Detected" = "darkgreen",
            "Not Detected" = "darkred"
          )) +
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.background = element_rect(fill = "#DDE0AB"),
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
        shinydashboard::valueBox(N, "Number of calls")
      })
      
      output$estN <- shinydashboard::renderValueBox({
        ave <- 1 - (1 - mean(prob_hist()))^8
        Nhat <- nrow(det_ind) / ave
        shinydashboard::valueBox(round(Nhat), "Estimated number of calls")
      })
      
      output$probD <- shinydashboard::renderValueBox({
        probd <- mean(prob_hist())
        shinydashboard::valueBox(round(probd,3), "Average probability of detection")
      })
      
    }
  })
  
  # Reset to initial state when "Start Over" button is clicked
  observeEvent(input$startOverBtn, {
    points(data.frame(x = numeric(0), y = numeric(0)))
    
    output$plot <- renderPlot({
      ggplot() +
        geom_image(data = microphones, aes(x = x, y = y, image = micro_image), size = 0.25) +
        xlim(0, 5) +
        ylim(0, 5) +
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
    
    output$capt_hist <- DT::renderDT({
      empty_dat <- matrix(nrow = 10, ncol = 8)
      colnames(empty_dat) <- paste0("M", 1:8)
      
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
      shinydashboard::valueBox(NULL,NULL)
    })
    
    output$estN <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(NULL,NULL)
    })
    
    output$probD <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(NULL,NULL)
    })
  })
  
  # Simualtion 
  
  observeEvent(input$repeatBtn,{
    simulate_surveys <- function(nsims=1000){
      
      nhats <- NULL
      for(i in 1:nsims){
        # Calculate probabilities
        prob_hist <- matrix(NA, nrow = nrow(points()), ncol = nrow(microphones))
        for (i in 1:nrow(microphones)) {
          prob_hist[, i] <- calculate_prob_succ(points(), microphones[i, ])
        }
        ave <- 1 - (1 - mean(prob_hist))^8
        est <- nrow(prob_hist[rowSums(prob_hist)>0,]) / ave
        nhats <- c(nhats,est)
      }
      
      return(list(nhats = nhats, true = nrow(ppp_df)))
    }
    
    data1000 <- simulate_surveys()
    
    output$sim_hist <- renderPlot({
      ggplot(data.frame(nhats=data1000$nhats),aes(x=nhats))+
      geom_histogram(bins=10)+
      geom_vline(xintercept=data1000$true)
    })
    
  })
}

# 
# Run the application
shinyApp(ui = ui, server = server)
