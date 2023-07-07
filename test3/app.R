library(shiny)
library(ggplot2)
library(DT)
library(spatstat)
library(raster)

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
  titlePanel("Shiny App - Add Points to Plot"),
  sidebarLayout(
    sidebarPanel(
      actionButton("addBtn", "Add Points"),
      actionButton("detBtn", "Detect!"),
      actionButton("startOverBtn", "Start Over")
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("capt_hist")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initial plot
  output$plot <- renderPlot({
    ggplot() +
      geom_point(data = microphones, aes(x = x, y = y, colour = "black")) +
      xlim(0, 5) +
      ylim(0, 5)
  })
  
  # Create a reactive dataframe to store the points
  points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Add points when the button is clicked
  observeEvent(input$addBtn, {
    window <- owin(c(0, 5), c(0, 5))
    ppp_object <- rpoispp(1, win = window)
    
    
    x <- ppp_object$x
    y <- ppp_object$y
    new_points <- data.frame(x, y)
    
    points(new_points)
    
    # Render the plot
    output$plot <- renderPlot({
      ggplot(points(), aes(x = x, y = y), colour = "blue") +
        geom_point() +
        geom_point(data = microphones, aes(x = x, y = y, colour = "black")) +
        xlim(0, 5) +
        ylim(0, 5) +
        geom_text(aes(label = rownames(points())), vjust = -0.5)
    })
  })
  
  
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
      prob_hist <- matrix(NA, nrow = nrow(points()), ncol = nrow(microphones))
      for (i in 1:nrow(microphones)) {
        prob_hist[, i] <- calculate_prob_succ(points(), microphones[i, ])
      }
      
      output$capt_hist <- DT::renderDT({
        # det_ind <- as.data.frame(prob_hist) %>%
        #   mutate(num = paste("Frog",row_number()))
        #
        # det_ind <- det_ind[rowSums(det_ind[,1:8])>0,]
        # colnames(det_ind) <- 1:8
        det_ind <- as.data.frame(prob_hist)
        rownames(det_ind) <- paste("Frog", rownames(det_ind))
        colnames(det_ind) <- paste("M", 1:8)
        det_ind <- det_ind[rowSums(det_ind[, 1:8]) > 0, ]
        
        
        det_ind %>%
          DT::datatable(
            rownames = TRUE,
            class = "cell-border",
            escape = FALSE,
            extensions = "Buttons",
            selection = "single",
            options = list(dom = "t", ordering = F)
          )
      })
      
      
      # Render the plot
      output$plot <- renderPlot({
        dat <- cbind(points(), prob_hist)
        
        det_dat <- dat %>%
          rowwise() %>%
          mutate(
            sum = sum(c_across(3:10)),
            det = ifelse(sum > 0, "Detected", "Not Detected")
          )
        
        det_dat %>% 
          data.frame()%>%
          mutate(num = row_number()) %>%
          ggplot(aes(x = x, y = y, colour = det)) +
          geom_point() +
          geom_point(data = microphones, aes(x = x, y = y), colour = "black") +
          xlim(0, 5) +
          geom_text(aes(label = num), vjust = -0.5) +
          scale_color_manual(values = c("Detected" = "darkgreen",
                                        "Not Detected"= "darkred"))
      })
    }

    
  })
  
  # Reset to initial state when "Start Over" button is clicked
  observeEvent(input$startOverBtn, {
    points(data.frame(x = numeric(0), y = numeric(0)))
    
    output$plot <- renderPlot({
      ggplot() +
        geom_point(data = microphones, aes(x = x, y = y, colour = "black")) +
        xlim(0, 5) +
        ylim(0, 5)
    })
    
    output$capt_hist <- DT::renderDT(NULL)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
