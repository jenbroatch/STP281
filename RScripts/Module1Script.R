library(tidyverse)
Flintdata<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Flint.csv")
View(Flintdata)
counts<- table(Flintdata$Contaminated)
counts 

barplot(counts, main="Flint Water Contamination",
        xlab="Number of Contaminated Homes") 
barplot(counts, main="Flint Water Contamination",
        xlab="Number of Contaminated Homes", names.arg=c("No Lead Contamination Evident", 
                                                         "Lead Contamination Evident"), 
        col=c("red", "blue"))


prop.test(54, 271, p = 0.10, alternative = "greater")

prop.test(25, 271, p = 0.10, alternative = "greater")

#No Continuity Correction
prop.test(54, 271, p = 0.10, alternative = "greater", correct=F)

Teststat=((54/271)-.1)/sqrt(.1*.9/271)
Teststat

#Note that the square of a variable with a standard normal distribution 
#is a Chi-squared distribution.
Teststat^2

library(dplyr)

library(ggplot2)
resumedata<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/resume.csv")
View(resumedata)





#Filter the dataset to include only the desired variables for LL1
LL1example<-resumedata %>% select('job_city','received_callback')
results <- data.frame(table(LL1example))
results 


#Sample plot code
ggplot(data = results, aes(x = received_callback, y = Freq, fill = job_city)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) +
  geom_text(aes(label = Freq), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(x = "\n Callback Status", y = "Frequency\n", title = "\n Callback Results by Job City \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold",  size = 12),
        axis.title.y = element_text(face="bold",  size = 12),
        legend.title = element_text(face="bold", size = 10))

prop.test(c(210, 182), n = c(210+1956, 182+2522))

#MODULE 1.2 
library(mosaic)
library(mosaicData)
View(CPS85)
women85=filter(CPS85, sex=="F")
w1=favstats(wage~married, data=women85)
w1 

w1$mean[1] 
bwplot(wage~married, data=women85)
#Using the t-test function
t.test(wage ~ married, data = women85)

#MODULE 1.3 
cellphone<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/cellphonereaction.csv")
View(cellphone)
cellphone <- cellphone %>%
  mutate(
    diff = Cell.Phone-No.cell.phone
  )
View(cellphone)
hist(cellphone$diff, xlab='Difference in reaction times: Cell-no cell', main='Histogram')
favstats(~diff, data=cellphone)
t.test(~diff, data=cellphone)   

#Alternative syntax that does not require calculation of differences
t.test(cellphone$Cell.Phone,cellphone$No.cell.phone, paired=T )  
