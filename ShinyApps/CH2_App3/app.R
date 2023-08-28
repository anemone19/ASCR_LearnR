# Chapter 2 Shiny App 3 --------------------------------------------------------------------------------

# Overview 
# This Shiny application provides an interactive visualization of frog detections based on their distance
# from various microphones (detectors). The app allows users to visualize and understand the distribution 
# of distances between frogs and detectors and to explore the proportion of detected frogs at different distances.

# Distance Histogram: A histogram showcasing the distances between frogs and detectors. Users can toggle between visualizing all frogs, only detected frogs, or the proportion of detected frogs.

# Interactive Buttons:
# All Frogs: Displays a histogram of distances for all frogs.
# Detected Frogs: Augments the histogram to show detected frogs differentiated by color.
# Proportions Detected: Reveals a bar plot that displays the proportion of detected frogs at various distance intervals.
# Proportions Plot: A bar plot that showcases the probability of frog detection at different distance intervals. 
# This plot becomes visible upon clicking the "Proportions Detected" button.

# Required Libraries

libraries <- c("shiny", "tidyverse", "DT", "ggimage", "shinyWidgets", "shinydashboard", "secr", "plotly", "ggplotify", "tippy")
lapply(libraries, require, character.only = TRUE)


# Setup --------------------------------------------------------------------------------

# load images for plotting
frog_image <- "images/frogGraphic.png"
micro_image <- "images/micro.png"


# Function(s) -----------------------------------------------------------------------------------

e2dist <- function(x, y) {
  if (!is.matrix(x)) x <- as.matrix(x)
  if (!is.matrix(y)) y <- as.matrix(y)
  
  dist_x <- outer(x[, 1], y[, 1], `-`)
  dist_y <- outer(x[, 2], y[, 2], `-`)
  
  sqrt(dist_x^2 + dist_y^2)
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

# save(temppop,CH_combined,det_array,det_dat,trapdf,file = "data/CH2.RData")
# load("data/CH2.RData")

# Additional objects for second shiny app ------------------------------------------------------

#calculate distances between all frogs and detectors
distances <- t(e2dist(det_array, temppop))
distances_df <- data.frame(d = as.numeric(distances))

# distances between detected frogs and all detectors
all_detect_distances <- distances[as.numeric(rownames(CH_combined)), ]

# extract only distances of detected frogs, i.e. where CH_combined = 1

detect_distances <- c()

# Checking if entry in df1 is equal to 1 and extracting corresponding entry from df2
for (i in 1:nrow(CH_combined)) {
  for (j in 1:ncol(CH_combined)) {
    if (CH_combined[i, j] == 1) {
      detect_distances <- c(detect_distances, all_detect_distances[i, j])
    }
  }
}

# create dataframe with detected distances,
# and frog locations of all not detected that were within 50 m from a microphone
# detect_distances_df <- data.frame(d = detect_distances)

distdf <- data.frame(
  dist = c(detect_distances, as.numeric(distances)),
  group = c(rep("Detected", length(detect_distances)), rep("All", nrow(distances_df)))
)


# Proportions plot

# Cut distances into intervals
# Define the intervals
intervals <- seq(0, 50, 10)

interval_counts_det <- table(cut(subset(distdf$dist, 
                                        distdf$group == "Detected"),
                                 breaks = intervals,
                                 include.lowest = TRUE))

interval_counts_Ndet <- table(cut(subset(distdf$dist, 
                                         distdf$group == "All"),
                                  breaks = intervals,
                                  include.lowest = TRUE))

props <- as.numeric(interval_counts_det / interval_counts_Ndet)

# Given dataframe 'prop'
prop <- data.frame(
  Var1 = c(5,15,25,35,45),
  Freq = props
)


# Shiny App ------------------------------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    plotlyOutput("histDistPlot"),
    conditionalPanel(
      "input.propBtn%2 == 1",
      plotlyOutput("propPlot")
    )
  ),
  br(),
  fluidRow(
    actionButton("allFrogs", "All frogs", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4"),
    actionButton("addDet", "Detected frogs", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4"),
    actionButton("propBtn", "Proportions detected", style = "color: #fff; background-color: #668ba4; border-color: #2e6da4"),
    align = "center"
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$histDistPlot <- renderPlotly({
    ggplotly(
      ggplot(distdf[15:239,], aes(x = dist, group='All',text=after_stat(count))) +
        geom_histogram(
          alpha = 0.5, binwidth = 10,
          center = 5, position = "identity",
          colour = "#5b7a65",
          fill = "#97CBA9"
        ) +
        labs(
          x = "\nDistance (m)",
          y = "Count\n"
        ) +
        scale_x_continuous(breaks=seq(0,200,10)) +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
        ),
      tooltip = c("text")
    )
    
  })
  
  observeEvent(input$allFrogs, {
    output$histDistPlot <- renderPlotly({
      ggplotly(
        ggplot(distdf[15:239,], aes(x = dist, text = after_stat(count))) +
          geom_histogram(
            alpha = 0.5, binwidth = 10,
            center = 5, position = "identity",
            colour = "#5b7a65",
            fill = "#97CBA9"
          ) +
          labs(
            x = "\nDistance (m)",
            y = "Count\n"
          ) +
          scale_x_continuous(breaks=seq(0,200,10)) +
          theme_minimal() +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
          ),
        tooltip = c("text")
      )
      
    })
    
    #   output$propPlot <- renderPlotly({
    #   NULL
    # })
    
  })
  # Add detections
  
  observeEvent(input$addDet, {
    output$histDistPlot <- renderPlotly({
      ggplotly(
        ggplot(distdf, aes(x = dist, fill = group, colour = group, text = after_stat(count))) +
          geom_histogram(
            alpha = 0.5, binwidth = 10,
            center = 5, position = "identity"
          ) +
          scale_fill_manual(
            values = c("#97CBA9", "#668BA4"),
            labels = c("All", "Detected"),
            guide = guide_legend(title = "", position = "top")
          ) +
          scale_color_manual(
            values = c("#5b7a65", "#142D4C"),
            labels = c("All", "Detected"),
            guide = guide_legend(title = "", position = "top")
          ) +
          labs(
            x = "\nDistance (m)",
            y = "Count\n"
          ) +
          scale_x_continuous(breaks=seq(0,200,10)) +
          theme_minimal() +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
          ),
        tooltip = c("text")
      ) %>%
        layout(legend = list(
          orientation = "h"
        ))
    })
    
    #   output$propPlot <- renderPlotly({
    #   NULL
    # })
  })
  
  observeEvent(input$propBtn, {
    output$histDistPlot <- renderPlotly({
      ggplotly(
        ggplot(distdf[distdf$dist <= 50, ], aes(x = dist, fill = group, colour = group, text = after_stat(count))) +
          geom_histogram(
            alpha = 0.5, binwidth = 10,
            center = 5, position = "identity"
          ) +
          scale_fill_manual(
            values = c("#97CBA9", "#668BA4"),
            labels = c("All", "Detected"),
            guide = guide_legend(title = "")
          ) +
          scale_color_manual(
            values = c("#5b7a65", "#142D4C"),
            labels = c("All", "Detected"),
            guide = guide_legend(title = "")
          ) +
          labs(
            x = "\nDistance (m)",
            y = "Count\n"
          ) +
          theme_minimal() +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "top"
          ),
        tooltip = c("text")
      ) %>%
        layout(legend = list(
          orientation = "h"
        ))
    })
    
    output$propPlot <- renderPlotly({
      ggplotly(
        # Plot the bar plot
        ggplot(prop, aes(x = Var1, y = Freq, text = round(Freq,2))) +
          geom_bar(stat = "identity", colour = "#5b7a65", fill = "#97CBA9") +
          labs(x = "\nDistance (m)",
               y = "Probability of detection\n") +
          theme_minimal() +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10)
          ),
        tooltip = c("text")
      )
      
    })
  })
    
}

# Run the application
shinyApp(ui = ui, server = server)
