library(shiny)
ui<-fluidPage(
  titlePanel("MTCars Histogram"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable","Select Variable:",
                  choices=names(mtcars),
                  selected="mpg")
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)
server<-function(input,output){
  output$histogram<-renderPlot({
    hist(mtcars[[input$variable]],
         main=paste("Histogram of",input$variable),
         xlab=input$variable,
         col="skyblue",
         border="black")
  })
}
shinyApp(ui=ui,server=server)
