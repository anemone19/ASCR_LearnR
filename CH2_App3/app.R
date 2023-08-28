# Chapter 2 RScript --------------------------------------------------------------------------------
# Required Libraries

library(shiny)
library(tidyverse)
library(DT)
library(ggimage)
library(shinyWidgets)
library(shinydashboard)
library(secr)
library(RColorBrewer)
library(plotly)
library(ggplotify)
library(tippy)

# Global objects --------------------------------------------------------------------------------

# load images for plotting
frog_image <- "images/frogGraphic.png"
micro_image <- "images/micro.png"


# Function(s) -----------------------------------------------------------------------------------

e2dist <- function(x, y) {
  if (!is.matrix(x)) x <- as.matrix(x)
  if (!is.matrix(y)) y <- as.matrix(y)

  i <- sort(rep(1:nrow(y), nrow(x)))
  dvec <- sqrt((x[, 1] - y[i, 1])^2 + (x[, 2] - y[i, 2])^2)
  matrix(dvec, nrow = nrow(x), ncol = nrow(y), byrow = F)
}

# Objects for first shiny app -------------------------------------------------------------------

# Created microphone array with make.grid() from package secr
#  9 microphones, 50 units apart, detector is of type proximity

det_array <- make.grid(
  nx = 3, ny = 3, spacing = 50, detector =
    "proximity"
)

trapdf <- data.frame(det_array) # dataframe for plotting

# Objects for frogPopPlot -------------------------------------------------------------------

# simulate population using functions from secr package
set.seed(1908) # set seed
temppop <- sim.popn(D = 5, expand.grid(x = c(0, 100), y = c(0, 100)), buffer = 50)

# generate capture histories with halfnormal detection function, g0 = 1, sigma = 20

CH <- sim.capthist(det_array,
  detectpar = list(g0 = 1, sigma = 20),
  noccasions = 1,
  popn = temppop, renumber = FALSE
)

# modify capture histories
CH_combined <- matrix(NA, nrow = dim(CH)[1], ncol = dim(CH)[3])
rownames(CH_combined) <- names(CH[, , 1])

for (i in 1:9) {
  CH_combined[, i] <- CH[, , i]
}

# create dataframe for first plot (frogPopPlot) indicating which which point in tempop was detected
# add row number as column

det_dat <- temppop %>%
  mutate(
    det = ifelse(row_number() %in% rownames(CH_combined), "Detected", "Not Detected"),
    num = row_number()
  )

# convert

# Create a matrix of row and column indices where capt == 1
indices <- which(as.data.frame(CH_combined) == 1, arr.ind = TRUE)

# Extract the row and column indices
ID <- rownames(indices)
traps_ID <- indices[, 2]

# Create a dataframe with the results
captures <- data.frame(ID, trap = traps_ID) %>%
  arrange(ID)

# calculate distances between all frogs and detectors
distances <- as.data.frame(t(e2dist(det_array, temppop)))

frog_row <- det_dat[1, ]
trap <- trapdf[1, ]

frog_row

# click frog
# if detected
# plot all distances but different colour for distance between microphone that detected
# its call
# if not detected plot just distances same colour

add_annotation <- function(trap, frog, det = "no") {
  x1 <- unlist(trap[1]) # x1/x2/y1/y2 defined here for shorthand later
  x2 <- unlist(frog[1])
  y1 <- unlist(trap[2])
  y2 <- unlist(frog[2])

  if (det == "yes") {
    lineCol <- "darkgreen"
    textCol <- "darkgreen"
  } else {
    lineCol <- "grey"
    textCol <- "black"
  }

  # the function will return the last object it creates, ie this list with two objects
  list(
    annotate("segment",
      color = lineCol,
      x = x1, xend = x2,
      y = y1, yend = y2
    ),
    annotate("text",
      color = textCol, size = 4,
      x = x1, y = y1 + 13,
      label = paste(
        round(sqrt((x1 - x2)^2 + (y1 - y2)^2), digits = 1)
      )
    )
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidPage(
    fluidRow(
      column(6, plotOutput("distPlot",click = "plot_click")),
      column(6, plotOutput("isoFrogPlot"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(det_dat) +
      geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
      geom_image(data = det_array, aes(x = x, y = y, image = micro_image), size = 0.2) +
      geom_text(aes(x = x, y = y, label = num),
        vjust = 0.5, hjust = 0.5, colour = "white",
        size = 2, fontface = "bold"
      ) +
      labs(
        x = "\nX",
        y = "Y\n"
      ) +
      scale_color_manual(values = c(
        "Detected" = "darkgreen",
        "Not Detected" = "darkred"
      )) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#c1e0cb"),
        legend.text = element_text(size = 7)
      ) +
      scale_x_continuous(n.breaks = 15) +
      scale_y_continuous(n.breaks = 10)
  })

  frog_row <- reactiveVal()

  # click to generate density plot
  observeEvent(input$plot_click, {
    frog_row <- nearPoints(det_dat, input$plot_click, maxpoints = 1)

    # output$text <- renderPrint({
    #   nrow(frog_row)
    # })

    baseplot <- ggplot(frog_row) +
      geom_image(aes(x = x, y = y, colour = det, image = frog_image), size = 0.09) +
      geom_image(data = det_array, aes(x = x, y = y, image = micro_image), size = 0.2) +
      geom_text(aes(x = x, y = y, label = num),
        vjust = 0.5, hjust = 0.5, colour = "white",
        size = 2, fontface = "bold"
      ) +
      labs(
        x = "\nX",
        y = "Y\n"
      ) +
      scale_color_manual(values = c(
        "Detected" = "darkgreen",
        "Not Detected" = "darkred"
      )) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#c1e0cb"),
        legend.text = element_text(size = 7)
      ) +
      xlim(-50, 150) +
      ylim(-50, 150)

    if (nrow(frog_row) == 0) {
      showModal(modalDialog(
        title = "Oops!",
        "Please click on a frog :)",
        easyClose = TRUE
      ))
    } else {
      output$isoFrogPlot <- renderPlot({
        if (frog_row$det == "Detected") {
          # Extract trap IDs
          micro_index <- captures$trap[captures$ID == frog_row$num]

          # Filter trapdf to include only micro_index rows
          micro_trapdf <- trapdf[micro_index, ]

          # Create a base plot
          plot <- baseplot

          # Iterate through micro_index and add annotations to the plot
          for (i in seq_along(micro_index)) {
            plot <- plot +
              add_annotation(micro_trapdf[i, ], frog_row[, 1:2], det = "yes")
          }

          # Find indices of rows not in micro_index
          rest_index <- setdiff(rownames(trapdf), rownames(micro_trapdf))

          # Iterate through rest_index and add annotations to the plot
          for (i in seq_along(rest_index)) {
            plot <- plot +
              add_annotation(trapdf[rest_index[i], ], frog_row[, 1:2])
          }
          
          plot 
          
        } else {
          annotations <- list()

          for (i in 1:9) {
            annotations[[i]] <- add_annotation(trapdf[i, ], frog_row[, 1:2])
          }

          baseplot + annotations
        }
      })
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
