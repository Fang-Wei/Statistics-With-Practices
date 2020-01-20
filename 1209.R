load("student0405.rdata")
head("student0405.rdata")

PartyDays <- student0405[,6]
StudyHrs <- student0405[,7]

cor.test(PartyDays,StudyHrs)

RESULTS <- lm(PartyDays~StudyHrs)
summary(RESULTS)

coeff <- coefficients(RESULTS)
res <- residuals(RESULTS)
yhat <- fitted.values(RESULTS)

plot(PartyDays~StudyHrs,pch = 16,col="black",main="PartyDays vs. StudyHrs",xlab="Study_Hrs",ylab="Party_Days")
abline(RESULTS,col="red")