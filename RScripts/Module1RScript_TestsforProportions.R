#STP281 
#Module 1 - R Script One and Two Sample Proportions 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(binom)

#Module 1.1: One Sample Confidence intervals for Proportion
binom.confint(142, n = 1432)
binom.confint(142, n = 1432, conf.level=.95, methods="asymptotic")


#Module 1.2- Sample Size Calculations for Proportions
sample.size.prop(0.01, P = 0.97, N = Inf, level = 0.95)  #Proportion known 
sample.size.prop(0.05, P = 0.5, N = Inf, level = 0.95)   #Conservative approach 


#Module 1.3- One Sample Proportion- Hypothesis Test Basics
#Module 1.4- One Sample Proportion- P-value


Flintdata<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/Flint.csv")
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

#Module 1.5- Two Sample Proportions

#The counts used in lecture are from the summarized data.  

# Vector of counts meeting the condition for each group
counts <- c(246, 164)

# Vector of sample sizes for each group
n <- c(2445, 2445)

# Find and store results
testResult <- prop.test(x=counts, 
                        n=n, 
                        alternative="two.sided", 
                        correct=FALSE)

# Print z-test statistic and p-value
unname(sqrt(testResult$statistic))
testResult$p.value

testResult 


#If you wanted to get the counts and sample size (n) from raw data
#DISCLAIMER- the summarized data set above includes 20 observations that were not included in the larger data set 

resumedata<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/resume.csv")
View(resumedata)


#Filter the dataset to include only the desired variables for example
resume<-resumedata %>% select('race','received_callback')
results <- data.frame(table(resume))
results 

#Get counts extracted from the table
summary_data <- resume %>%
  group_by(race) %>%
  summarize(
    counts = sum(received_callback == 1),
    n = n()
  )

# Extract the vectors for successes and sample sizes
counts <- summary_data$counts
n <- summary_data$n


#Intro to R - Reading in Data
flint<-read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/Flint.csv") 


str(flint)
head(flint)
View(flint)
