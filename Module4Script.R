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
