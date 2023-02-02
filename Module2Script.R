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



#Module 2.3 - Two-Way ANOVA
hyper<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/hypertension.csv") 
head(hyper)
str(hyper)

# wrap in “as.factor()” OR recode into a factor variable
# Convert drug  as a factor and recode the levels
# as "D1", "D2", "D3"
hyper$Drug <- factor(hyper$Drug, 
                     levels = c(1, 2, 3),
                     labels = c("D1", "D2", "D3"))
head(hyper)
library("ggpubr")

ggboxplot(hyper, x = "Drug", y = "HP", color = "Special.Diet",
          palette = c("Red", "Blue"))


ggline(hyper, x = "Drug", y = "HP", color = "Special.Diet",
       add = c("mean_se", "dotplot"),
       palette = c("Red", "Blue"))

ggline(hyper, x = "Drug", y = "HP", color = "Special.Diet",
       add = c("mean_se"),
       palette = c("Red", "Blue"))

#conduct the ANOVA with interaction
hypermod <- aov(HP ~ Drug * Special.Diet, data = hyper)
summary(hypermod)

#conduct the ANOVA - additive model 
hypermod_add <- aov(HP ~ Drug + Special.Diet, data = hyper)
summary(hypermod_add)


#We don’t need to perform the test for the Special Diet 
#variable because it has only two levels, 
#which have been already tested.

TukeyHSD(hypermod_add, which = "Drug")

#Check assumptions/ reasonable test
plot(hypermod_add, 1)
plot(hypermod_add, 2)

#Now look at Biofeed
ggline(hyper, x = "Drug", y = "HP", color = "BioFeed",
       add = c("mean_se"),
       palette = c("Red", "Blue"))

#conduct the ANOVA with interaction
hypermod2 <- aov(HP ~ Drug * BioFeed, data = hyper)
summary(hypermod2)

#conduct the ANOVA - additive model 
hypermod_add2 <- aov(HP ~ Drug + BioFeed, data = hyper)
summary(hypermod_add2)

#conduct the ANOVA - three level

p= ggboxplot(hyper, x = "Drug", y = "HP", 
  color = "Special.Diet", palette = c("Red", "Blue")
)
print(p)
facet(p + theme_bw(), facet.by = "BioFeed",
      short.panel.labs = FALSE
)


hypermod_3<- aov(HP ~ Drug*BioFeed*Special.Diet, data = hyper)
summary(hypermod_3)


#Module 1: Problem Set script 
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




