Sleep <- read.file("http://www.mosaic-web.org/go/datasets/SleepCaffeine.csv") 
bwplot(Group ~ Words, data=Sleep) 

#Calculate the mean of each group
mean(Words ~ Group, data=Sleep)
obs <- diff(mean(Words ~ Group, data=Sleep))
obs

#Shuffle groups
#Let's see what "shuffle" does! 
shuffle(Sleep$Group)

diff(mean(Words ~ shuffle(Group), data=Sleep))

Sleep.null <- do(1000) * diff(mean(Words ~ shuffle(Group), data=Sleep))
histogram(~ Sleep, groups=(Sleep >= obs), data=Sleep.null, width=0.4,
          xlab="Distribution of difference in means \n under the null hypothesis")
extreme=count(Sleep.null$Sleep >= obs)
extreme
pvalue=extreme/1000
pvalue