raceData <- read.table("TrainExer13.txt", header=TRUE, sep="\t")
mean(raceData$Winning.time.men)
mean(raceData$Game)

b <- sum(((raceData$Winning.time.men-10.082)*(raceData$Game-8))/sum((raceData$Game-8)^2))

e <- raceData$Winning.time.men-10.38+0.038*(raceData$Game)

R_squared = 1 - (sum(e^2)/sum((raceData$Winning.time.men-mean(raceData$Winning.time.men))^2))

s <- sqrt((1/13)*sum(e^2))

# R squared = 0.672
# Therefore 67% of variance in data explained by trend
# Residual is approximately 0.1 ~0.2 for most of the data points

a=10.38
b=-0.038
x1=16
y1=a+b*(x1)

x2=17
y2=a+b*(x2)

x3=18
y3=a+b*(x3)
