library(shiny)
library(ggplot2)
library(shinyWidgets)

# UI
# Define functions

# halfnormal

# Half-Normal Detection Function
halfnormal <- function(distance, g0, sigma) {
  g0 * exp(-distance^2 / 2 * sigma^2)
}

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
  1 - exp(-g0 * exp(-distance * (2 * sigma^2)))
}

# # Signal Strength Detection Function
# signal_strength <- function(distance, alpha, beta) {
#   alpha * exp(-beta * distance)
# }


detfunctions <- c("Halfnormal", "Hazard Rate", "Exponential", "Uniform", "Hazard Halfnormal", "Signal Strength")
ui <- fluidPage(
  withMathJax(),
  sidebarLayout(
    sidebarPanel(
      setSliderColor(rep("#668BA4",3),c(1,2)),
      chooseSliderSkin("Flat"),
      selectInput("detFunc", "Select Function:",
        choices = c("Halfnormal", "Hazard Rate", "Exponential", "Uniform", "Hazard Halfnormal", "Signal Strength")
      ),
      sliderInput("g0", "Set Value of g0:", min = 0, max = 1, value = 1),
      sliderInput("sigma","Set value for \\ \\sigma \\", min = 0, max = 1, value = 0.2),
      conditionalPanel(
        condition = "input.detFunc == 'Hazard Rate'",
        sliderInput("z", "Set value for z", min = 0, max = 1, value = 0.5)
      ),
    ),
    mainPanel(
      plotOutput("functionPlot")
    )
  )
)


# Server
server <- function(input, output) {
  dists <- seq(0, 100, by = 1)
  output$functionPlot <- renderPlot({
    x <- seq(-10, 10, length = 100)

    if (input$detFunc == detfunctions[1]) {
      y <- halfnormal(dists, input$g0, input$sigma)
    } else if (input$detFunc == detfunctions[2]) {
      y <- hazard_rate(dists, input$g0, input$sigma, input$z)
    } else if (input$detFunc == detfunctions[3]) {
      y <- exponential(dists, input$g0, input$sigma)
    } else if (input$detFunc == detfunctions[4]) {
      y <- uniform(dists, input$g0, input$sigma)
    } else if (input$detFunc == detfunctions[5]) {
      y <- hazard_halfnormal(dists,input$g0, input$sigma)
    }

    # Create data frame
    df <- data.frame(d = dists, y = y)

    # Plot
    ggplot(df, aes(d, y)) +
      geom_line(linewidth=1, colour = "#97CBA9") +
      labs(title = paste(input$detFunc, "Detection Function")) +
      xlab("\nDistance") +
      ylab("Detection probability\n") +
      ylim(0,1) +
      theme_minimal()+
      theme(axis.title = element_text(size=15),
            plot.title = element_text(size=20))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
