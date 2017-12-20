# Test Week 2

rm(list = ls())

setwd("~/Documents/2017/Coursera/Econometrics/Week 2")
resultsData <- read.table("TestExer2-GPA-round2.txt", header=TRUE, sep="\t")
resultsData$Observ. <- NULL

#Question 1a
#Regress FGPA on a constant and SATV. Report the coefficient of SATV and its standard error and p-value
#(give your answers with 3 decimals).
simpleModel <- lm(resultsData$FGPA~resultsData$SATV)
summary(simpleModel)$coefficients

#Question 1b
#Determine a 95% confidence interval (with 3 decimals) for the effect on FGPA of an increase by 1 point
#in SATV.
confint(simpleModel, 'resultsData$SATV', level=0.95)

#Question 2a
#Answer questions (a-i) and (a-ii) also for the regression of FGPA on a constant, SATV, SATM, and FEM.
complexModel <- lm(resultsData$FGPA~resultsData$SATV + resultsData$SATM + resultsData$FEM)
summary(complexModel)$coefficients

#Question 2b
confint(complexModel, 'resultsData$SATV', level=0.95)
confint(complexModel, 'resultsData$SATM', level=0.95)
confint(complexModel, 'resultsData$FEM', level=0.95)

#Question 3
cor(resultsData)

#Question 4
unrestricted <- lm(resultsData$FGPA ~resultsData$SATV + resultsData$SATM + resultsData$FEM)
restricted <- lm(resultsData$FGPA ~resultsData$SATM + resultsData$FEM)

unrestrictedR <- summary(unrestricted)$r.squared
unrestrictedF <- 27.3
  #summary(unrestricted)$fstatistic[1]

restrictedR <- summary(restricted)$r.squared
#restrictedF <- summary(restricted)$fstatistic[1]
restrictedF <- 18.2

#Number of regressors excluded
g <- 1


#Number of observations
n <- 609

#Number of regressors in unrestricted model
k <- 4

fValue <- (((unrestrictedR) - (restrictedR))/g)/((1-(unrestrictedR))/(n-k))

