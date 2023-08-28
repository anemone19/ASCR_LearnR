library(shiny)

shinyApp(
  ui <- fluidPage(DT::dataTableOutput('tableId'),
                  textOutput("celltext"),
                  actionButton("next","Next")),
  
  server <- function(input, output) {
    rv <- reactiveValues(text=NULL)
    dt <- reactiveValues(data=NULL)
    rnum <- reactiveVal(0)
    output$tableId = DT::renderDataTable(
      iris[,c(1,5)],  selection = list(target = 'row',mode="single")
    )
    species<-c("setosa","setosa","virginica","virginica","setosa","setosa","virginica","virginica")
    flower<-c("a","b","c","d","e","f","g","h")
    score<-c(7,5,6,9,1,2,3,4)
    df<-data.frame(species,flower,score)
    
    observeEvent(input$tableId_rows_selected, {
      if(is.null(input$tableId_rows_selected)){
        return(NULL)
      }
      else{
        row <- input$tableId_rows_selected
        dat<-df[df$species %in% iris[row,5],]
        dt$data <-dat[order(dat$score,decreasing = T),]
        rv$text <- paste("flower",dt$data[1,2],"has score",dt$data[1,3])
        rnum(1)
        
        output$celltext <- renderText({
          if(length(input$tableId_rows_selected))  rv$text
          else ''
        })
      }
      
      
    })
    
    observeEvent(input[['next']], {
      rnum(rnum()+1)
      rv$text <- paste("flower",dt$data[rnum(),2],"has score",dt$data[rnum(),3])
    })
  }
)
# Run the application 
shinyApp(ui = ui, server = server)
