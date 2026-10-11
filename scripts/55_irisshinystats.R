library(shiny)
data(iris)
ui<-fluidPage(
  titlePanel("Iris Species Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species","Select Iris Species:",
                  choices=levels(iris$Species),
                  selected="setosa")
    ),
    mainPanel(
      verbatimTextOutput("observations"),
      verbatimTextOutput("sepal_length"),
      verbatimTextOutput("petal_length")
    )
  )
)
server<-function(input,output){
  filtered_data<-reactive({
    iris[iris$Species==input$species,]
  })
  output$observations<-renderPrint({
    cat("Number of Observations:",nrow(filtered_data()))
  })
  output$sepal_length<-renderPrint({
    cat("Average Sepal Length:",round(mean(filtered_data()$Sepal.Length),2))
  })
  output$petal_length<-renderPrint({
    cat("Average Petal Length:",round(mean(filtered_data()$Petal.Length),2))
  })
}
shinyApp(ui=ui,server=server)
