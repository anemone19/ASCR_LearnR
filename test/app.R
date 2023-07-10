#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(palmerpenguins)
library(DT)

dataset_1<-penguins
dataset_2<-iris

ui <- fluidPage(
  titlePanel("Rstudio community issue"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select_dataset",
                  label = "Select the dataset to visualize",
                  choices = c("Penguin dataset"="dataset_1",
                              "Iris dataset"="dataset_2")),
      actionButton("dataset_selection",
                   label = "Select the dataset"),
      br(),
      br(),
      uiOutput("more_selection"),
      uiOutput("look_dataset")),
    mainPanel(uiOutput("datatable"),
              uiOutput("last_datatable"))
  )
)

server <- function(input, output) {
  
  
  dataset_to_use <- eventReactive(input$dataset_selection, {
    switch(input$select_dataset,
           "dataset_1"=dataset_1,
           "dataset_2"=dataset_2)
  })
  
  
  output$datatable<-renderUI({
    DT::dataTableOutput("datatable_1")
  })
  
  output$datatable_1<-renderDT({
    datatable(dataset_to_use())
  })
  
  output$more_selection<-renderUI({
    wellPanel(
      selectInput("filter_to_analyze",
                  label = "Select variable to see",
                  choices = names(dataset_to_use()),
                  multiple = TRUE))
  })
  
  output$look_dataset<-renderUI({
    actionButton("run_analysis",
                 label = "Run Analysis")
  })
  
  
  dataset_filtered <- eventReactive(input$run_analysis, {
    dataset_to_use() %>% 
      dplyr::select(input$filter_to_analyze)
  })
  
  output$last_datatable<-renderUI({
    DT::dataTableOutput("datatable_2")
  })
  
  
  output$datatable_2<-renderDT({
    DT::datatable(dataset_filtered())
  })
  
}

shinyApp(ui, server)