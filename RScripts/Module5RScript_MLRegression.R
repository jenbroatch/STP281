##Module 5 - Multiple Linear Regression
library(ggplot2)
library(dplyr)

#Module 5.1 

kidiq =read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/kidiq.csv") 
View(kidiq)

p1 <- kidiq  %>% 
  ggplot(aes(x = mom_iq, y = kid_score)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Simple Linear Model", x="Mom's IQ", y="Kid's IQ") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))
p1

reg1=lm(kid_score~mom_iq, data=kidiq)
summary(reg1) 


p2 <- kidiq  %>% 
  ggplot(aes(x = mom_age, y = kid_score)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = F) +
  labs( x="Mom's Age", y="Kid's IQ") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))
p2

# Generates the linear regression model
# Multiple predictor variables are joined with +
reg2=lm(kid_score~mom_iq+mom_age, data=kidiq)
summary(reg2)


#Module 5.2 = Adding a Categorical Variable 

ggplot(kidiq, aes(x=as.factor(mom_hs), y=kid_score)) + geom_boxplot() +
  ggtitle("Distribution Kid's IQ by Mom's High School Status") +
  ylab("Kid's IQ") + 
  xlab("Mom's High School Graduation Status \n 1= graduated, 0=did not graduate") 


reg3=lm(kid_score~mom_iq+as.factor(mom_hs), data=kidiq)
summary(reg3)


kidiq$predicted <- predict(reg3) 
p3 <- kidiq %>%
  ggplot(aes(x = mom_iq, y = kid_score)) +
  geom_point(aes(color = factor(mom_hs))) +  # Color points by mom_hs
  geom_line(aes(y = predicted, color = factor(mom_hs)), linewidth = 1) +  # Fitted lines with same slope
  labs(title = "Regression Lines with Same Slope but Different Intercepts",
       x = "Mom's IQ", 
       y = "Kid's Score",
       color = "Mom HS") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

p3


#The model can benefit from an interaction term which is outside the scope of the course.

reg4=lm(kid_score~mom_iq+as.factor(mom_hs) + as.factor(mom_hs)*mom_iq, data=kidiq)
summary(reg4)
p4 <- kidiq %>%
  ggplot(aes(x = mom_iq, y = kid_score, color = factor(mom_hs))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Regression Lines for Two Levels of Mom's HS Completion",
       x = "Mom's IQ", 
       y = "Kid's Score",
       color = "Mom HS") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

p4
