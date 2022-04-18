rm(list = ls())

#Read in Database
library(readxl)
my_data <- read_excel("C:\\Users\\maryh\\OneDrive - University College Cork\\College - 4th Year\\Semester 2\\ST4092 - Data Analytics Project\\Irish Racing Data\\Databases\\Dashboard Data.xlsx")

#Get Column Names
names(my_data)

#Select Useful Columns
data = subset(my_data, select = c("Y Variable", "Race Course", "Race Type", "Distance (in miles)", "Age", "Stone", "Pounds", "Weight", "Favourite", "Race Conditions", "Number of Horses"))
names(data)

#Regression Model
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
summary(regressionModel)

#Prediction Model
newdata = data.frame(raceType="Hurdle", raceCourse="Cork", raceConditions="Soft", raceDistance=3.70, numberHorses=15, age=8, weight=158, favourite="Not Favourite")
predict(regressionModel, newdata, type="response")

#Get Coefficents in Table
coefficents_df <- summary.glm(regressionModel)$coefficients

#Get Odds Values in Table
calculateOdds <- function(value){
  odds <- exp(value)
  return(odds)
}
odds_df <- calculateOdds(coef(regressionModel))

#Get Probabilities in Table
logitToprob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
probability_df <- logitToprob(coef(regressionModel))

#Put into Data Frame
data_df <- data.frame(coefficents_df, odds_df, probability_df)
write.csv(data_df, "C://Users//maryh//Documents//FYP//data.csv")

#Predicting
predict(regressionModel, )