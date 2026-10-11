library(shiny)
library(ggplot2)
students<-data.frame(
  Student_ID=1:15,
  Name=c("Arun","Priya","Rahul","Meera","Kiran","Anu","Vijay","Divya","Ajay","Sneha","Ravi","Nisha","Manu","Diya","Hari"),
  Gender=c("Male","Female","Male","Female","Male","Female","Male","Female","Male","Female","Male","Female","Male","Female","Male"),
  Department=c("CS","CS","IT","IT","DA","DA","CS","IT","DA","CS","IT","DA","CS","IT","DA"),
  Semester=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3),
  Mark=c(85,78,92,67,88,74,56,81,95,69,72,90,62,86,77),
  Attendance=c(95,88,97,75,92,80,68,91,98,78,84,96,72,89,85)
)
ui<-fluidPage(
  titlePanel("Student Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("department","Department",
                  choices=c("All",sort(unique(students$Department))),
                  selected="All"),
      selectInput("semester","Semester",
                  choices=c("All",sort(unique(students$Semester))),
                  selected="All"),
      sliderInput("min_mark","Minimum Mark",
                  min=0,max=100,value=0)
    ),
    mainPanel(
      fluidRow(
        column(4,h4("Total Students"),textOutput("total")),
        column(4,h4("Average Mark"),textOutput("avg_mark")),
        column(4,h4("Average Attendance"),textOutput("avg_attendance"))
      ),
      h3("Filtered Student Records"),
      tableOutput("student_table"),
      fluidRow(
        column(6,plotOutput("marks_plot")),
        column(6,plotOutput("department_plot"))
      )
    )
  )
)
server<-function(input,output){
  filtered_students<-reactive({
    data<-students
    if(input$department!="All")
      data<-data[data$Department==input$department,]
    if(input$semester!="All")
      data<-data[data$Semester==as.numeric(input$semester),]
    data<-data[data$Mark>=input$min_mark,]
    data
  })
  output$total<-renderText({
    nrow(filtered_students())
  })
  output$avg_mark<-renderText({
    data<-filtered_students()
    if(nrow(data)==0) "N/A" else round(mean(data$Mark),2)
  })
  output$avg_attendance<-renderText({
    data<-filtered_students()
    if(nrow(data)==0) "N/A" else paste0(round(mean(data$Attendance),2),"%")
  })
  output$student_table<-renderTable({
    filtered_students()
  })
  output$marks_plot<-renderPlot({
    data<-filtered_students()
    validate(need(nrow(data)>0,"No students match the selected filters"))
    ggplot(data,aes(x=Mark))+
      geom_histogram(binwidth=10,fill="steelblue",color="white")+
      labs(title="Marks Distribution",x="Marks",y="Number of Students")+
      theme_minimal()
  })
  output$department_plot<-renderPlot({
    data<-filtered_students()
    validate(need(nrow(data)>0,"No students match the selected filters"))
    dept_avg<-aggregate(Mark~Department,data=data,FUN=mean)
    ggplot(dept_avg,aes(x=Department,y=Mark))+
      geom_col(fill="darkorange")+
      labs(title="Department-wise Average Marks",x="Department",y="Average Marks")+
      theme_minimal()
  })
}
shinyApp(ui=ui,server=server)
