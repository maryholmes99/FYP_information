rm(list = ls())
library(readxl)
my_data <- read_excel("C:\\Users\\maryh\\OneDrive - University College Cork\\College - 4th Year\\Semester 2\\ST4092 - Data Analytics Project\\Irish Racing Data\\Databases\\Mary Holmes Database.xlsx")
names(my_data)
data = subset(my_data, select = c("Y Variable", "Race Course", "Race Type", "Distance (in miles)", "Age", "Weight (in lbs)", "Favourite", "Race Conditions", "Number of Horses"))
names(data)

total_horses = nrow(data)
total_horses

incomplete <- data[data$`Y Variable` == "1",]
incomplete_horses = nrow(incomplete)
incomplete_horses

complete <- data[data$`Y Variable` == "0",]
complete_horses = nrow(complete)
complete_horses

#Age
age_complete = complete$Age
age_incomplete = incomplete$Age
meanCompleteAge = round(mean(age_complete), digits = 1)
meanIncompleteAge = round(mean(age_incomplete), digits = 1)
meanCompleteAge
meanIncompleteAge

#Weight
weight_complete = complete$`Weight (in lbs)`
weight_incomplete = incomplete$`Weight (in lbs)`
meanCompleteWeight = round(mean(weight_complete), digits = 2)
meanIncompleteWeight = round(mean(weight_incomplete), digits = 2)
meanCompleteWeight
meanIncompleteWeight

#Horses Ran
horsesRan_complete = complete$`Number of Horses`
horsesRan_incomplete = incomplete$`Number of Horses`
meanCompleteHorsesRan = round(mean(horsesRan_complete), digits = 1)
meanIncompleteHorsesRan = round(mean(horsesRan_incomplete), digits = 1)
meanCompleteHorsesRan
meanIncompleteHorsesRan

#Race Distance
raceDistance_complete = complete$`Distance (in miles)`
raceDistance_incomplete = incomplete$`Distance (in miles)`
meanCompleteRaceDistance = round(mean(raceDistance_complete), digits = 2)
meanIncompleteRaceDistance = round(mean(raceDistance_incomplete), digits = 2)
meanCompleteRaceDistance
meanIncompleteRaceDistance

meanNumerics <- data.frame(meanCompleteAge, meanIncompleteAge, meanCompleteWeight, meanIncompleteWeight, meanCompleteRaceDistance, meanIncompleteRaceDistance, meanCompleteHorsesRan, meanIncompleteHorsesRan)
colnames(meanNumerics) <- c('Mean Complete Horse Age', 'Mean Incompete Horse Age', 'Mean Complete Weight', 'Mean Incomplete Weight', 'Mean Complete Race Distance', 'Mean Incomplete Race Distance', 'Mean Complete Horses Ran', 'Mean Incomplete Horses Ran')
write.csv(meanNumerics, "meanNumerics.csv")
