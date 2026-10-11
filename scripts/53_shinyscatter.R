library(shiny)
ui<-fluidPage(
  titlePanel("MTCars Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar","Select X-axis Variable:",
                  choices=names(mtcars),selected="wt"),
      selectInput("yvar","Select Y-axis Variable:",
                  choices=names(mtcars),selected="mpg")
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)
server<-function(input,output){
  output$scatterplot<-renderPlot({
    plot(mtcars[[input$xvar]],mtcars[[input$yvar]],
         main=paste(input$yvar,"vs",input$xvar),
         xlab=input$xvar,
         ylab=input$yvar,
         pch=19,
         col="blue")
  })
}
shinyApp(ui=ui,server=server)
