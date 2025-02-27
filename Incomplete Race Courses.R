total_raceCourse = aggregate(data$`Race Course`, by=list(data$`Race Course`), FUN=length)
incomplete_raceCourse = aggregate(incomplete$`Race Course`, by=list(incomplete$`Race Course`), FUN=length)
incomplete_raceCourse_percentage = round((incomplete_raceCourse$x/total_raceCourse$x) * 100, digits = 2)
incomplete_raceCourse <- data.frame(incomplete_raceCourse, incomplete_raceCourse_percentage, total_raceCourse$x)
colnames(incomplete_raceCourse) <- c('Race Course', 'Incomplete', 'Incomplete Percentage', 'Total')
write.csv(incomplete_raceCourse, "incomplete_raceCourse.csv")