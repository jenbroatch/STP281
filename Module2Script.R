library(dplyr)

anorexia<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Anorexia.csv")
View(anorexia)
par(mfrow = c(1, 1)) # do not combine plots
boxplot(anorexia$Cognitive.Therapy, anorexia$Family.Therapy, anorexia$Control,
        names=c("Cogn. Therapy", "Family Therapy", "Control"), 
        ylab="Weight Loss/Gain (lbs)") 

anorexia_stack=na.omit(stack(anorexia))
View(anorexia_stack)


anovamod =aov(values~ind, data=anorexia_stack)
summary(anovamod) 

#Quick check of normality assumption
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(anovamod$residuals)

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






#Module 2.2 Multiple comparisons

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

#Module 1: Problem Set dcript 
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")


# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-03-02')
tuesdata <- tidytuesdayR::tt_load(2021, week = 10)

youtube <- tuesdata$youtube

# Or read in the data manually

youtube <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

View(youtube)
#Remove ads in both or neither category
youtube2<- youtube %>% filter(funny!=patriotic) 
t.test(youtube2$funny, youtube2$patriotic)



