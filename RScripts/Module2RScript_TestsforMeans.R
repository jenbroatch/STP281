#Module 2 - Inference for Means
#STP281

#Module 2.1 One sample t-test
# Load the data
hours <- read.csv('https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/TVwatching.csv')
View(hours)
hist(hours$Hours, xlab='Hours Spent Watching TV', main='Histogram')

# Find the degrees of freedom, n-1
df <- length(hours$Hours)-1
df

# Find critical value for 95% interval
tCrit <- qt((1-(1-.95)/2), df=df)
round(tCrit,3)

# Find mean and standard error
mean <- mean(hours$Hours)
stderr <- sd(hours$Hours)/sqrt(length(hours$Hours))

# Calculate the confidence interval
lowerBound <- mean - tCrit*stderr
upperBound <- mean + tCrit*stderr
paste("[", round( lowerBound,3), 
      ", ", round(upperBound,3), "]", sep="") 

# FInd the 95% interval using t.test()
interval<-t.test(x=hours$Hours, conf.level=0.95)
interval$conf.int

t.test(x=hours$Hours, mu=3)



#MODULE 1.2 Two sample Difference in Means - Independent 
library(mosaic)
library(mosaicData)

View(Gestation)
Gestation2 <-Gestation %>%
  mutate(smoke_now = if_else(smoke == "now",
                             "yes", 
                             "no")) %>%
  filter(!is.na(smoke)) 

View(Gestation2)
ggplot(Gestation2, aes(x=smoke_now, y=wt)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+
  xlab("Smokes Now during Pregnancy")+
  ylab("Birth weight (oz)")

favstats(wt~smoke_now, data=Gestation2)  #uses Mosaic package
tapply(Gestation2$wt, Gestation2$smoke_now, summary)  # summary stats without the use of mosaic 


t.test(wt~smoke_now, data=Gestation2)


#MODULE 2.3 Dependent t-test
cellphone<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/refs/heads/main/DataSets/cellphonereaction.csv")
View(cellphone)
cellphone <- cellphone %>%
  mutate(
    diff = Cell.Phone-No.cell.phone
  )
View(cellphone)
hist(cellphone$diff, xlab='Difference in reaction times: Cell-no cell', main='Histogram')
favstats(~diff, data=cellphone) # this uses mosiac, but there are MANY other options
t.test(~diff, data=cellphone)   

#Alternative syntax that does not require calculation of differences
t.test(cellphone$Cell.Phone,cellphone$No.cell.phone, paired=T )  
