#Module 3 - ANOVA 
#STP281


library(dplyr)

# For an F-distribution, if the degrees of freedom between samples is 2
# and the degrees of freedom within samples is 5, what is P(F < 2)?
pf(2, 2, 5, lower.tail = TRUE) 

#Module 3.1 - ANOVA Basics 

anorexia<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/Anorexia.csv")
View(anorexia)
boxplot(anorexia$Cognitive.Therapy, anorexia$Family.Therapy, anorexia$Control,
        names=c("Cogn. Therapy", "Family Therapy", "Control"), 
        ylab="Weight Loss/Gain (lbs)") 

anorexia_stack=na.omit(stack(anorexia)) #One option to change format to tall 
View(anorexia_stack)
anovamod =aov(values~ind, data=anorexia_stack)
summary(anovamod) 

#Quick check of normality assumption
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(anovamod$residuals)

#Another option to transform data

anorexia_tall <- anorexia%>%
  pivot_longer(cols = everything(), names_to = "treatment", values_to = "weight")
View(anorexia_tall)
anovamod2 =aov(weight~treatment, data=anorexia_tall)
summary(anovamod2) 

# QQ-plot
library(car)
qqPlot(anovamod$residuals,
       id = FALSE # id = FALSE to remove point identification
)

#Another choice
par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances
plot(anovamod, which = 3)

# 2. Normality
plot(anovamod, which = 2)

#Test for normality
shapiro.test(anovamod$residuals) 
#Levene's Test

library(car)

leveneTest(values~ind, data=anorexia_stack)






#Module 3.3  Multiple comparisons

TukeyHSD(anovamod, conf.level=.95) 
plot(TukeyHSD(anovamod, conf.level=.95), las = 2)

library(multcomp)

# Tukey HSD test:
post_test <- glht(anovamod,
                  linfct = mcp(ind = "Tukey")
)
summary(post_test)
par(mfrow = c(1, 1)) # combine plots
plot(post_test)

TukeyHSD(anovamod)

