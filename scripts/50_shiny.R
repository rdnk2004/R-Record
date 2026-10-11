library(shiny)
ui<-fluidPage(
  titlePanel("Car Cylinder Filter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cyl","Select Cylinders:",choices=c(4,6,8),selected=4)
    ),
    mainPanel(
      tableOutput("car_table")
    )
  )
)
server<-function(input,output){
  output$car_table<-renderTable({
    mtcars[mtcars$cyl==as.numeric(input$cyl),]
  })
}
shinyApp(ui=ui,server=server)
