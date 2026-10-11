library(shiny)
ui<-fluidPage(
  titlePanel("MTCars Summary Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable","Select Variable:",
                  choices=names(mtcars),
                  selected="mpg")
    ),
    mainPanel(
      verbatimTextOutput("summary")
    )
  )
)
server<-function(input,output){
  output$summary<-renderPrint({
    summary(mtcars[[input$variable]])
  })
}
shinyApp(ui=ui,server=server)
