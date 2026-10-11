library(shiny)
library(ggplot2)
data(mtcars)
ui<-fluidPage(
  titlePanel("Car Analytics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cyl","Select Cylinders:",
                  choices=c("All",sort(unique(mtcars$cyl))),
                  selected="All"),
      sliderInput("mpg","Minimum MPG:",
                  min=floor(min(mtcars$mpg)),
                  max=ceiling(max(mtcars$mpg)),
                  value=floor(min(mtcars$mpg)))
    ),
    mainPanel(
      fluidRow(
        column(4,h4("Number of Cars"),textOutput("total_cars")),
        column(4,h4("Average MPG"),textOutput("avg_mpg")),
        column(4,h4("Average Horsepower"),textOutput("avg_hp"))
      ),
      h3("Filtered Car Records"),
      tableOutput("car_table"),
      fluidRow(
        column(6,plotOutput("mpg_histogram")),
        column(6,plotOutput("cylinder_chart"))
      )
    )
  )
)
server<-function(input,output){
  filtered_cars<-reactive({
    data<-mtcars
    if(input$cyl!="All")
      data<-data[data$cyl==as.numeric(input$cyl),]
    data<-data[data$mpg>=input$mpg,]
    data
  })
  output$total_cars<-renderText({
    nrow(filtered_cars())
  })
  output$avg_mpg<-renderText({
    data<-filtered_cars()
    if(nrow(data)==0) "N/A" else round(mean(data$mpg),2)
  })
  output$avg_hp<-renderText({
    data<-filtered_cars()
    if(nrow(data)==0) "N/A" else round(mean(data$hp),2)
  })
  output$car_table<-renderTable({
    data<-filtered_cars()
    data.frame(Car=rownames(data),data,check.names=FALSE)
  })
  output$mpg_histogram<-renderPlot({
    data<-filtered_cars()
    validate(need(nrow(data)>0,"No cars match the selected filters"))
    ggplot(data,aes(x=mpg))+
      geom_histogram(binwidth=2,fill="steelblue",color="white")+
      labs(title="MPG Distribution",x="Miles per Gallon",y="Number of Cars")+
      theme_minimal()
  })
  output$cylinder_chart<-renderPlot({
    data<-filtered_cars()
    validate(need(nrow(data)>0,"No cars match the selected filters"))
    cylinder_avg<-aggregate(mpg~cyl,data=data,FUN=mean)
    ggplot(cylinder_avg,aes(x=factor(cyl),y=mpg))+
      geom_col(fill="darkorange")+
      labs(title="Average MPG by Cylinder Category",x="Cylinders",y="Average MPG")+
      theme_minimal()
  })
}
shinyApp(ui=ui,server=server)
