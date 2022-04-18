rm(list = ls())

#Read in Database
library(readxl)
my_data <- read_excel("C:\\Users\\maryh\\Documents\\FYP\\Mary Holmes Database.xlsx")

#Get Column Names
names(my_data)

#Select Useful Columns
data = subset(my_data, select = c("Y Variable", "Race Course", "Race Type", "Distance (in miles)", "Age", "Weight (in lbs)", "Favourite", "Race Conditions", "Number of Horses"))
names(data)
data

#CATEGORICAL - Sum of Completed/Incompleted Horses
total_horses = nrow(data)
incomplete <- data[data$`Y Variable` == "1",]
incomplete_horses = nrow(incomplete)
incomplete_percentage = (round(incomplete_horses/total_horses, digits = 2)) * 100
complete <- data[data$`Y Variable` == "0",]
complete_horses = nrow(complete)
complete_percentage = (round(complete_horses/total_horses, digits = 2)) * 100
horses <- data.frame(complete_horses, complete_percentage, incomplete_horses, incomplete_percentage, total_horses)
colnames(horses) <- c('Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
write.csv(horses, "C://Users//maryh//Documents//FYP//horses.csv")

#CATEGORICAL - Sum of Race Type
total_raceType = aggregate(data$`Race Type`, by=list(data$`Race Type`), FUN=length)
complete_raceType = aggregate(complete$`Race Type`, by=list(complete$`Race Type`), FUN=length)
incomplete_raceType = aggregate(incomplete$`Race Type`, by=list(incomplete$`Race Type`), FUN=length)
complete_raceType_percentage = (round(complete_raceType$x/total_raceType$x, digits = 2)) * 100
incomplete_raceType_percentage = (round(incomplete_raceType$x/total_raceType$x, digits = 2)) * 100
raceType <- data.frame(complete_raceType, complete_raceType_percentage, incomplete_raceType$x, incomplete_raceType_percentage, total_raceType$x)
colnames(raceType) <- c('Race Type', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
raceType
write.csv(raceType, "C://Users//maryh//Documents//FYP//raceType.csv")


#CATEGORICAL - Sum of Race Courses
total_raceCourse = aggregate(data$`Race Course`, by=list(data$`Race Course`), FUN=length)
complete_raceCourse = aggregate(complete$`Race Course`, by=list(complete$`Race Course`), FUN=length)
incomplete_raceCourse = aggregate(incomplete$`Race Course`, by=list(incomplete$`Race Course`), FUN=length)
complete_raceCourse_percentage = (round(complete_raceCourse$x/total_raceCourse$x, digits = 2)) * 100
incomplete_raceCourse_percentage = (round(incomplete_raceCourse$x/total_raceCourse$x, digits = 2)) * 100
raceCourse <- data.frame(complete_raceCourse, complete_raceCourse_percentage, incomplete_raceCourse$x, incomplete_raceCourse_percentage, total_raceCourse$x)
colnames(raceCourse) <- c('Race Course', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
raceCourse
write.csv(raceCourse, "C://Users//maryh//Documents//FYP//raceCourse.csv")

#CATEGORICAL - Sum of Favourite
total_favourite = aggregate(data$Favourite, by=list(data$Favourite), FUN=length)
complete_favourite = aggregate(complete$Favourite, by=list(complete$Favourite), FUN=length)
incomplete_favourite = aggregate(incomplete$Favourite, by=list(incomplete$Favourite), FUN=length)
complete_favourite_percentage = (round(complete_favourite$x/total_favourite$x, digits = 2)) * 100
incomplete_favourite_percentage = (round(incomplete_favourite$x/total_favourite$x, digits = 2)) * 100
favourite <- data.frame(complete_favourite, complete_favourite_percentage, incomplete_favourite$x, incomplete_favourite_percentage, total_favourite$x)
colnames(favourite) <- c('Favourite', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
favourite
write.csv(favourite, "C://Users//maryh//Documents//FYP//favourite.csv")

#CATEGORICAL - Sum of Race Conditions
total_raceConditions = aggregate(data$`Race Conditions`, by=list(data$`Race Conditions`), FUN=length)
complete_raceConditions = aggregate(complete$`Race Conditions`, by=list(complete$`Race Conditions`), FUN=length)
incomplete_raceConditions = aggregate(incomplete$`Race Conditions`, by=list(incomplete$`Race Conditions`), FUN=length)
complete_raceConditions_percentage = (round(complete_raceConditions$x/total_raceConditions$x, digits = 2)) * 100
incomplete_raceConditions_percentage = (round(incomplete_raceConditions$x/total_raceConditions$x, digits = 2)) * 100
raceConditions <- data.frame(complete_raceConditions, complete_raceConditions_percentage, incomplete_raceConditions$x, incomplete_raceConditions_percentage, total_raceConditions$x)
colnames(raceConditions) <- c('Race Conditions', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
raceConditions
write.csv(raceConditions, "C://Users//maryh//Documents//FYP//raceConditions.csv")

#NUMERIC - Sum of Number of Horses That Ran
total_horsesRan = aggregate(data$`Number of Horses`, by=list(data$`Number of Horses`), FUN=length)
complete_horsesRan = aggregate(complete$`Number of Horses`, by=list(complete$`Number of Horses`), FUN=length)
incomplete_horsesRan = aggregate(incomplete$`Number of Horses`, by=list(incomplete$`Number of Horses`), FUN=length)
complete_horsesRan_percentage = (round(complete_horsesRan$x/total_horsesRan$x, digits = 2)) * 100
incomplete_horsesRan_percentage = (round(incomplete_horsesRan$x/total_horsesRan$x, digits = 2)) * 100
horsesRan <- data.frame(complete_horsesRan, complete_horsesRan_percentage, incomplete_horsesRan$x, incomplete_horsesRan_percentage, total_horsesRan$x)
colnames(horsesRan) <- c('Number of Horses That Ran', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
horsesRan
write.csv(horsesRan, "C://Users//maryh//Documents//FYP//horsesRan.csv")

#NUMERIC - Sum of Horse's Age
total_age = aggregate(data$Age, by=list(data$Age), FUN=length)
incomplete_age = aggregate(incomplete$Age, by=list(incomplete$Age), FUN=length)
complete_age = total_age$x - incomplete_age$x
incomplete_age = aggregate(incomplete$Age, by=list(incomplete$Age), FUN=length)
complete_age_percentage = (round(complete_age/total_age$x, digits = 2)) * 100
incomplete_age_percentage = (round(incomplete_age$x/total_age$x, digits = 2)) * 100
age <- data.frame(incomplete_age$Group.1, complete_age, complete_age_percentage, incomplete_age$x, incomplete_age_percentage, total_age$x)
colnames(age) <- c('Horse Age', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
age
write.csv(age, "C://Users//maryh//Documents//FYP//age.csv")

#NUMERIC - Sum of Weight
total_weight = aggregate(data$`Weight (in lbs)`, by=list(data$`Weight (in lbs)`), FUN=length)
complete_weight = aggregate(complete$`Weight (in lbs)`, by=list(complete$`Weight (in lbs)`), FUN=length)
incomplete_weight = total_weight$x - complete_weight$x
complete_weight_percentage = (round(complete_weight$x/total_weight$x, digits = 2)) * 100
incomplete_weight_percentage = (round(incomplete_weight/total_weight$x, digits = 2)) * 100
weight <- data.frame(complete_weight, complete_weight_percentage, incomplete_weight, incomplete_weight_percentage, total_weight$x)
colnames(weight) <- c('Weight', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
weight
write.csv(weight, "C://Users//maryh//Documents//FYP//weight.csv")

#NUMERIC - Sum of Race Distance
total_raceDistance = aggregate(data$`Distance (in miles)`, by=list(data$`Distance (in miles)`), FUN=length)
total_raceDistance

complete_weight = aggregate(complete$`Weight (in lbs)`, by=list(complete$`Weight (in lbs)`), FUN=length)
incomplete_weight = total_weight$x - complete_weight$x
complete_weight_percentage = (round(complete_weight$x/total_weight$x, digits = 2)) * 100
incomplete_weight_percentage = (round(incomplete_weight/total_weight$x, digits = 2)) * 100
weight <- data.frame(complete_weight, complete_weight_percentage, incomplete_weight, incomplete_weight_percentage, total_weight$x)
colnames(weight) <- c('Weight', 'Complete', 'Complete Percentage', 'Incomplete', 'Incomplete Percentage', 'Total')
weight
write.csv(weight, "C://Users//maryh//Documents//FYP//weight.csv")