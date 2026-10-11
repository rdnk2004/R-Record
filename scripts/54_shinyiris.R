library(shiny)
data(iris)
ui<-fluidPage(
  titlePanel("Iris Species Filter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species","Select Species:",
                  choices=levels(iris$Species),
                  selected="setosa")
    ),
    mainPanel(
      tableOutput("iris_table")
    )
  )
)
server<-function(input,output){
  output$iris_table<-renderTable({
    iris[iris$Species==input$species,]
  })
}
shinyApp(ui=ui,server=server)
