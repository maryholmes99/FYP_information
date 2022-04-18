#Delete Everything
rm(list=ls())

#Read in Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(flexdashboard)

#Read in Data
my_data <- read_excel("C:\\Users\\maryh\\OneDrive - University College Cork\\College - 4th Year\\Semester 2\\ST4092 - Data Analytics Project\\Irish Racing Data\\Databases\\Dashboard Data.xlsx")
data = subset(my_data, select = c("Y Variable", "Race Course", "Race Type", "Distance (in miles)", "Age", "Stone", "Pounds", "Weight", "Favourite", "Race Conditions", "Number of Horses"))

#UI
ui <- dashboardPage(skin="green",
                    
                    dashboardHeader(title="Calculating Probability of Horse Not Completing Race", titleWidth=600),
                    
                    dashboardSidebar(width=250,
                                     sidebarMenu(
                                       menuItem("Select Values Below:"),
                                       radioButtons(inputId="type", label="Race Type:", choices=c(unique(data$`Race Type`))),
                                       selectInput(inputId="course", label="Race Course:", choices=c(sort(unique(data$`Race Course`)))),
                                       selectInput(inputId="conditions", label="Ground Conditions:", choices=c(sort(unique(data$`Race Conditions`))), multiple=FALSE),
                                       sliderInput(inputId="distance", label="Distance in Miles:", min=1.88, max=4.25, value=1.88, step=0.01),
                                       sliderInput(inputId="horses", label="Number of Horses Racing:", min=3, max=30, value=3, step=1),
                                       sliderInput(inputId="age", label="Age of Horse:", min=3, max=16, value=3, step=1),
                                       sliderInput(inputId="stone", label="Weight Horse is Carrying (Stone):", min=9, max=12, step=1, value=9),
                                       sliderInput(inputId="pounds", label="Weight Horse is Carrying (Pounds):", min=0, max=14, step=1, value=0),
                                       radioButtons(inputId="favourite", label="Whether Horse is Favourite or Not:", choices=c(sort(unique(data$Favourite)))))),
                                    
                    dashboardBody(
                      setBackgroundImage(src="https://www.martela.com/sites/default/files/styles/material_gallery_thumb/public/pim_files/MU43_light_grey_melamine_web.jpg?itok=PRxB0kI2", shinydashboard=TRUE),
                      column(12, box(flexdashboard::gaugeOutput("guage"), title="Percentage of Incompletion", width="520", height="200", background ="orange")),
                      img(src="https://news.paddypower.com/wp-content/uploads/2020/08/GettyImages-1204826993-2.jpg", width=255, height=255),
                      img(src="https://www.hri.ie/uploadedImages/HRI-Corporate/HRI_Corporate/Press_Office/News/CilNaas.jpg", width=255, height=255),
                      img(src="https://imengine.public.prod.cmg.infomaker.io/?uuid=f332bf2f-9815-5e7c-abc3-cf11a5d81a19&function=cropresize&type=preview&source=false&q=75&crop_w=0.99999&crop_h=0.74574&x=0&y=0&width=1200&height=675", width=255, height=255),
                      img(src="https://ballinroberacecourse.ie/wp-content/uploads/2019/08/mchale-1.jpg", width=255, height=255),
                      img(src="https://i2-prod.irishmirror.ie/incoming/article23850397.ece/ALTERNATES/s615/0_inpho_01538868.jpg", width=513, height=270)
              ))

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
  
  output$guage <- flexdashboard::renderGauge({
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
    i = as.numeric(i)
    gauge(i, min=0, max=100, symbol='%', label=paste("Probability (as %)"), gaugeSectors(
      success=c(0, 39), warning=c(40, 69), danger=c(70, 100), colors=c("success", "warning", "danger")))
  })
}

shinyApp(ui=ui, server=server)
