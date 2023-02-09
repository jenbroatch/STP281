##Module 3 - Regression and Correlation

#Module 3.1 - Regression and Correlation
gestation =read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Animals.csv") 
View(gestation)
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
cor(gestation$Gestational.Period, gestation$Longevity)


reg1=lm(gestation$Longevity~gestation$Gestational.Period )
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
abline(reg1)
summary(reg1)




lion=read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17e1LionNoses.csv"))
head(lion)
View(lion)
plot(lion$proportionBlack,lion$ageInYears)



library(tidyverse)
wine_ratings <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") 
View(wine_ratings)

wine_ratings <- wine_ratings %>% 
  select(-X) %>%
  extract(title, "year", "(20\\d\\d)", convert = TRUE, remove = FALSE) %>%
  mutate(year = ifelse(year < 1900, NA, year)) %>%
  filter(!is.na(price))

mod1=lm(points ~ log(price), data=wine_ratings)
summary(mod1)







