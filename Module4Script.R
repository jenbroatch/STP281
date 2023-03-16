##Module 4 - Regression Continued 

#Module 4.1 - Regression- Model adequacy
gestation =read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Animals.csv") 
View(gestation)
reg1=lm(gestation$Longevity~gestation$Gestational.Period )
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
abline(reg1)
summary(reg1)

#residuals
res <- resid(reg1)
#produce residual vs. fitted plot
plot(fitted(reg1), res, xlab="Predicted Value of y- longevity", ylab='Residual')
#add a horizontal line at 0 
abline(0,0)

#produce residual vs. x
plot(gestation$Gestational.Period, res, xlab="Gestational Period (days)", ylab='Residual')
#add a horizontal line at 0 
abline(0,0)

plot(reg1, which = 1)

#calculate the standardized residuals
standard_res <- rstandard(reg1)
#produce residual vs. fitted plot
plot(fitted(reg1), standard_res, xlab="Predicted Value of y- longevity", ylab='Standardized Residual')
#add a horizontal line at 0 
abline(0,0)
library(car)

# Cook's D plot
# identify D values > 4/(n-k-1)
threshold  <- 4/((nrow(gestation)-length(reg1$coefficients)-2))
plot(reg1, which=4, cook.levels=threshold)


#create Q-Q plot for residuals
qqnorm(res)
#add a straight diagonal line to the plot
qqline(res) 


qqPlot(reg1,labels=row.names(gestation$Animal), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

#Module 4.2
library(openintro)
library(dplyr)
library(ggplot2)
library(e1071)
View(mammals)
plot(mammals$body_wt, mammals$brain_wt, xlab="body weight", ylab="brain weight")

#Investigate a transformation on y

hist(mammals$brain_wt, xlab="Brain Weight (kg)", main="Histogram")
hist(log(mammals$brain_wt), xlab="ln(Brain Weight (kg))", main="Histogram")
plot(mammals$body_wt, log(mammals$brain_wt), xlab="body weight", ylab="ln(brain weight)")

#Investigate a transformation on x
hist(mammals$body_wt, xlab="Body Weight (kg)", main="Histogram")
hist(log(mammals$body_wt), xlab="ln(Body Weight (kg))", main="Histogram")
plot(mammals$body_wt, log(mammals$brain_wt), xlab="body weight", ylab="ln(brain weight)")

plot(log(mammals$body_wt), log(mammals$brain_wt), xlab="ln(body weight)", ylab="ln(brain weight)")


#Module 4.3
oring =read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/orings.csv") 
View(oring)
library(dplyr)
oring <- oring %>%
  mutate(failure =ifelse(damaged > 0, 1, 0))
View(oring)

plot(oring$temperature, oring$failure, xlab="Temperature at Launch", ylab="At least one oring failure"
     )

mylogit <- glm(failure ~ temperature, data = oring, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#define new data frame that contains predictor variable
newdata <- data.frame(temperature=seq(min(oring$temperature), max(oring$temperature),len=500))

#use fitted model to predict values of failure
newdata$failure = predict(mylogit, newdata, type="response")

#plot logistic regression curve
plot(failure ~ temperature, data = oring, col="steelblue")
lines(failure ~ temperature, newdata, lwd=2)


#use fitted model to predict values of failure with one value
newdata1=data.frame(temperature=c(31))
newdata1$failure = predict(mylogit, newdata1, type="response")
newdata1$failure
