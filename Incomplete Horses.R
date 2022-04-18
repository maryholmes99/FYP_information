total_horses = nrow(data)
incomplete <- data[data$`Y Variable` == "1",]
incomplete_horses = nrow(incomplete)
incomplete_horses_percentage = round((incomplete_horses/total_horses) * 100, digits = 2)
incomplete_horses <- data.frame(incomplete_horses, incomplete_horses_percentage, total_horses)
colnames(incomplete_horses) <- c('Total Incomplete', 'Incomplete Percentage', 'Total Horses')
write.csv(incomplete_horses, "incomplete_horses.csv")