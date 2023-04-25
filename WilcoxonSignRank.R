library(mosaic)
chemical=c(-.57, -.19, -.05, .76, 1.30, 2.02, 2.17, 2.46, 2.68, 3.02)
wilcox.test(chemical, mu=0, alternative="greater", exact=T, correct=F, conf.int=T)


mailorders=c(16.5, 14.1, 9.4, 10.4, 4.4, 7.5,
             10.1, 8.4, 7.6, 8.0, 10.3, 9.2,
             10.6, 5.6, 5.4, 7.8, 10.2, 11.9)
sort(mailorders)
histogram(~mailorders, type="percent")
favstats(~mailorders)
wilcox.test(mailorders, mu=8, alternative="greater", exact=T, correct=F, conf.int=T)
