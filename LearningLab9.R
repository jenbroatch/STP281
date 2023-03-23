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


levels(SaratogaHouses$heating)

reg4=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$heating)
summary(reg4)

reg5=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$fireplaces)
summary(reg5)

vif(reg5)
cor(SaratogaHouses$livingArea,SaratogaHouses$bedrooms)
