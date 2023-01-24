library(dplyr)

anorexia<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Anorexia.csv")

boxplot(anorexia$Cognitive.Therapy, anorexia$Family.Therapy, anorexia$Control,
        names=c("Cognitive Therapy", "Family Therapy", "Control"), 
        ylab="Weight Loss/Gain (lbs)") 

anorexia_stack=na.omit(stack(anorexia) 
View(anorexia_stack)


anovamod =aov(values~ind, data=anorexia_stack)
summary(anovamod) 

#lab
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



mutate(gradebook, Pass.Fail = ifelse(grade > 60, "Pass", "Fail"))