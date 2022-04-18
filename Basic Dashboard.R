#Delete Everything
rm(list=ls())

#Read in Libraries
library(shiny)
library(readxl)

#Read in Data
my_data <- read_excel("C:\\Users\\maryh\\OneDrive - University College Cork\\College - 4th Year\\Semester 2\\ST4092 - Data Analytics Project\\Irish Racing Data\\Databases\\Dashboard Data.xlsx")
data = subset(my_data, select = c("Y Variable", "Race Course", "Race Type", "Distance (in miles)", "Age", "Stone", "Pounds", "Weight", "Favourite", "Race Conditions", "Number of Horses"))

#Regression Model
regressionModel <- glm(data$`Y Variable` ~ data$`Race Type` + data$`Race Course` + data$`Race Conditions` + data$`Distance (in miles)` + data$`Number of Horses` + data$Age + data$Weight + data$Favourite, family = binomial(link = logit))

#UI
ui <- fluidPage(
  
  titlePanel("Calculating Probability of Horse Not Completing Race"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Race Type (type)
      radioButtons(inputId="type", label="Select Race Type:", choices=c(unique(data$`Race Type`)),selected=character(0)),
      
      #Race Course (course)
      selectInput(inputId="course", label="Select Race Course:", choices=c(sort(unique(data$`Race Course`)))),
      
      #Ground Conditions (conditions)
      selectInput(inputId="conditions", label="Select Ground Conditions:", choices=c(sort(unique(data$`Race Conditions`))), multiple=FALSE),
      
      #Distance in Miles (distance)
      sliderInput(inputId="distance", label="Select Distance in Miles:", min=1.88, max=4.25, value=1.88, step=0.01),
      
      #Number of Horses (horses)
      sliderInput(inputId="horses", label="Select Number of Horses Racing:", min=3, max=30, value=3, step=1),
      
      #Age (age)
      sliderInput(inputId="age", label="Select Age of Horse:", min=3, max=16, value=3, step=1),
      
      #Weight (weight)
      numericInput(inputId="stone", label="Select Weight Horse is Carrying (Stone):", min=9, max=12, step=1, value=9),
      numericInput(inputId="pounds", label="Select Weight Horse is Carrying (Pounds):", min=0, max=14, step=1, value=0),
      
      #Favourite (favourite)    
      radioButtons(inputId="favourite", label="Select Whether Horse is Favourite or Not:", choices=c(sort(unique(data$Favourite))), selected=character(0))
      
    ),
    
    mainPanel(
      textOutput("selected_type"),
      textOutput("selected_course"),
      textOutput("selected_conditions"),
      textOutput("selected_distance"),
      textOutput("selected_horses"),
      textOutput("selected_age"),
      textOutput("selected_weight"),
      textOutput("selected_favourite"),
      textOutput("regression_model"),
      textOutput("selected_data"),
      textOutput("predicted_value")
    )
  )
)

server <- function(input, output) {
  output$selected_type <- renderText({
    a = input$type
    paste("Race Type:", a)
  })
  output$selected_course <- renderText({ 
    b = input$course
    paste("Race Course:", b)
  })
  output$selected_conditions <- renderText({ 
    c = input$conditions
    paste("Ground Conditions:", c)
  })
  output$selected_distance <- renderText({ 
    d = input$distance
    paste("Race Distance in Miles:", d)
  })
  output$selected_horses <- renderText({ 
    e = input$horses
    paste("Number of Horses in Race:", e)
  })
  output$selected_age <- renderText({ 
    f = input$age
    paste("Horse's Age:", f)
  })
  output$selected_weight <- renderText({
    s = input$stone * 14
    p = input$pounds
    g = s + p
    paste("Weight Horse is Carrying (in Pounds):", g)
  })
  output$selected_favourite <- renderText({
    h = input$favourite
    paste("Whether Horse is Favourite or Not: ", h)
  })
  output$predicted_value <- renderText({
    y = data$`Y Variable`
    raceType = data$`Race Type`
    raceCourse = data$`Race Course`
    raceConditions = data$`Race Conditions`
    raceDistance = data$`Distance (in miles)`
    numberHorses = data$`Number of Horses`
    age = data$Age
    weight = data$Weight
    favourite = data$Favourite
    regressionModel <- glm(y ~ raceType + raceCourse + raceConditions + raceDistance + numberHorses + age + weight + favourite,family = binomial(link = logit))
    a = input$type
    b = input$course
    c = input$conditions
    d = input$distance
    e = input$horses
    f = input$age
    s = input$stone*14
    p = input$pounds
    g = s+p
    h = input$favourite
    newdata = data.frame(raceType=a, raceCourse=b, raceConditions=c, raceDistance=d, numberHorses=e, age=f, weight=g, favourite=h)
    i = predict(regressionModel, newdata, type="response")
    i = i*100
    i = round(i, digits=2)
    paste("Probability of Selected Horse Not Completing Race:", i, "%")
  })
}


shinyApp(ui=ui, server=server)
