mydata <- read.table("TrainExer11.txt", header = TRUE)
hist(mydata$Age)
summary(mydata$Age)

hist(mydata$Expenditures)
summary(mydata$Expenditures)

plot(mydata$Age, mydata$Expenditures)

# Split the data in two age groups

# Mean of Expenditures is 101.1
summary(mydata)

expenditure_old <- mydata[mydata$Age >= 40,]
summary(expenditure_old) # mean = 95.85
expenditure_young <- mydata[mydata$Age < 40,]
summary(expenditure_young) # mean = 106.4
