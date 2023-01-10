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
