library(mosaic)
View(SaratogaHouses)
reg1=lm(SaratogaHouses$price~SaratogaHouses$livingArea)
summary(reg1)
reg2=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$newConstruction)
summary(reg2)


reg3=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$waterfront)
summary(reg3)



ggplot(data = SaratogaHouses, aes(x = livingArea, y = price, color = newConstruction)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Living Area sq ft",
       y = "Price $")

ggplot(data = SaratogaHouses, aes(x = livingArea, y = price, color = waterfront)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Living Area sq ft",
       y = "Price $")




#More than 2 levels 
levels(SaratogaHouses$heating)

reg4=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$heating)
summary(reg4)

reg5=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$fireplaces)
summary(reg5)


#All comparisons 
library(emmeans)
emm1 = emmeans(reg4, specs = pairwise ~ heating)
summary(emm1)


#Post Learning Lab
#Let's look at interactions 

reg6=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$waterfront+ SaratogaHouses$livingArea+
          SaratogaHouses$waterfront)
summary(reg6)

reg7=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$newConstruction+
          SaratogaHouses$livingArea*SaratogaHouses$newConstruction)
summary(reg7)

#Plot exact model with no interaction

dd_m = data.frame(livingarea=SaratogaHouses$livingArea, price=predict(reg2, SaratogaHouses), newconstruction=SaratogaHouses$newConstruction)
ggplot(SaratogaHouses) + geom_point(aes(livingArea, price, colour=newConstruction))+ 
  geom_line(data=dd_m, aes(livingarea, price, colour=SaratogaHouses$newConstruction))
