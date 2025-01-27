##Module 4 - Regression and Correlation

#Module 4.1 & 4.2 - Regression & Correlation 
gestation =read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/Animals.csv") 
View(gestation)
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
cor(gestation$Gestational.Period, gestation$Longevity)


reg1=lm(gestation$Longevity~gestation$Gestational.Period )
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
abline(reg1)
summary(reg1)
confint(reg1,level=0.95)

library(ggplot2)
library(dplyr)
p1 <- gestation %>% 
  ggplot(aes(x = Gestational.Period, y = Longevity)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

p1

#Add fancier confidence intervals for the mean **not in slides
ci95 <- predict(reg1, gestation, interval = "confidence", level = 0.95)
View(ci95)
# this does not have the original x values only predicted y and CI


#Add the rest of the gestation data to interval estimates
final_data <- bind_cols(gestation, ci95)
View(final_data)

#What if you wanted to make a prediction?  

#Note that our data set does not have gestation period=100
#So weird... I needed to run the model again without the $
reg1=lm(Longevity~Gestational.Period, data=gestation )
gestation.new<-data.frame( Gestational.Period=c(100))

predict(reg1, gestation.new, interval = "confidence")
predict(reg1, gestation.new, interval = "prediction")

temp_var <- predict(reg1, interval="prediction")


new_df <- cbind(gestation, temp_var)

ggplot(new_df, aes(Gestational.Period, Longevity))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)

#Module 4.3 & 4.4
reg1=lm(gestation$Longevity~gestation$Gestational.Period )
summary(reg1)
anova(reg1) 
confint(reg1,level=0.95)


