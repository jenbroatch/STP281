#Module 6 
#Chisquared tests 

#Module 6.1 - Goodness of Fit 
#Harvard Admission
ObsAdmit <- c(432, 247, 226, 1118)
res <- chisq.test(ObsAdmit, p = c(.574, .031, 0.008, .387))
res
#Module 6.2 
Observed=cbind(c(42600,	4657,	296, 95),  c(51620,	4030, 150, 56), 
               c(69384,	9280,	1782,	128))
Observed
rownames(Observed)=c("On-Time Flights", "Delayed Flights", 
                     "Canceled Flights", "Diverted Flights")
colnames(Observed)=c("American","Delta", "Southwest")
Observed= t(Observed)
Observed

prop.table(Observed,2)
barplot(prop.table(Observed,2),beside=TRUE,legend.text=TRUE,
        ylim=c(0,1),ylab="Proportions")

x=addmargins(Observed)
x
res1=chisq.test(Observed)
res1
res1$expected 
res1$stdres

#Module 6.3
#Chi squared test of independence from counts 
#From contingency table 
Observed <- cbind(c(2, 39, 131, 175, 78), c(8, 113, 138, 129, 32), 
                  c(46, 202, 120, 65,  8)) # or use matrix command

Observed
rownames(Observed)=c("Post-Grad Degree", "College Degree", "Some College", 
                     "HS Grad", "No HS Degree")

colnames(Observed)=c("<$30k","$30k to $75k", " >$75k")
Observed

prop.table(Observed,2)
barplot(prop.table(Observed,2),beside=TRUE,legend.text=TRUE,
        ylim=c(0,1),ylab="Proportions")

x=addmargins(Observed)
x
res1=chisq.test(Observed)
res1
res1$expected 
res1$stdres
