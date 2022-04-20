#question1
library(tidyverse)
my_lizards <- read.csv("lizards.csv")
set.seed(17053777)
my_lizards <- my_lizards %>% sample_n(70)



#question2
ggplot(my_lizards, aes(x = SVL_mm, y = mass_g)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)



#question3
m1 <- lm(mass_g ~ SVL_mm, data = my_lizards)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
ggplot(my_lizards,aes(x=SVL_mm,y=mass_g))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE)

# linear assumption, redline match the 0 line.
# normality assumption, whether deviation.
# constant residual variance assumption, variance is the similar
# outlier around red line



#question4
m2 <- lm(log(mass_g) ~ SVL_mm, data = my_lizards)
summary(m2)
par(mfrow=c(2, 2))
plot(m2)
ggplot(my_lizards,aes(x=SVL_mm,y=log(mass_g)))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE)



#question5
# An increase of 1 in x multiplies y by e ^ (0.04115).
# A useful approximation, 
# If b1 with a smallish magnitude, an increase of 1 in SVL_mm multiples mass_g by approximately (1 + 0.04115)



#question6
m3 <- lm(log(mass_g) ~ log(SVL_mm), data = my_lizards)
summary(m3)
par(mfrow=c(2, 2))
plot(m3)
ggplot(my_lizards, aes(x=log(SVL_mm),y=log(mass_g)))+geom_point()+geom_smooth(method = lm,se=FALSE)
# From residuals vs Fitted graph, we can see the the red line ia flatter in m3, linearity improves



#question7
# an increase of 1%  in SVL_mm multiplies mass_g by approximately (1 + 3.3778 ^ 0.01)



# question8
m4 <- lm(mass_g ~ SVL_mm + I(SVL_mm ^ 2), data = my_lizards)
summary(m4)
par(mfrow=c(2, 2))
plot(m4)
ggplot(my_lizards, aes(x=SVL_mm,y=mass_g))+
  geom_point()+
  geom_smooth(method = lm,formula = y~x + I(x ^ 2),se=FALSE)
# From Scale-location graph, we can see the variance is not that constant than the variance in m3. there is a smaller variance at the beginning but increases afterwards.

#question9
# the intercept 31.383944 is fitted value of mass_g when SVL_mm is 0
# b1 = -1.020905 is the slope of the tangent line to the curve at SVL_mm = 0
# b2 = 0.009699 is a measure of the increasing curvature.



#question10
my_lizards <- my_lizards %>% mutate(SVL_mm_c = (SVL_mm - mean(SVL_mm)))
m5 <- lm(mass_g ~ SVL_mm_c + I(SVL_mm_c ^ 2), data = my_lizards)
summary(m5)
# # the intercept  13.684045 is fitted value of mass_g when SVL_mm is equal to the mean value of SVL_mm
# b1 = 0.596265 is the slope of the tangent line to the curve at SVL_mm is equal to the mean of SVL_mm
# b2 = 0.009699 is a measure of the increasing curvature.
# SVL_mm means lizard's snout-vent length in millimetres which could not be 0 in real life.
# so in m4, the interpretation about the intercept does not make sense.
# but in m5, the interpretation is more meaningful since we can get the mean value of SVL_mm



#question11
# m1 and m4 could be compared using a nested model Ftest with anova
# m1 is the linear relationship of mass_g and SVL_mm
# m4 includes all terms in m1 and plus an extra quadratic term.
# so m1 is nested to m4

# question12
my_lizards %>% ggplot(aes(x = SVL_mm, y = mass_g)) + 
  geom_point() + 
  geom_function( fun = function(x) coef(m1)[[1]] + coef(m1)[[2]] * x, aes(color = "blue"), size = 1 ) + 
  # alternative for linear is geom_smooth(method = "lm", se = FALSE, aes(color = "blue") ) 
  geom_function( fun = function(x) exp(coef(m2)[[1]]) * exp(x * coef(m2)[[2]]), aes(color = "green"), size = 1 ) + 
  geom_function( fun = function(x) exp(coef(m3)[[1]]) * x^coef(m3)[[2]], aes(color = "orange"), size = 1 ) + 
  geom_function( fun = function(x) coef(m4)[[1]] + coef(m4)[[2]] * x + coef(m4)[[3]] * x^2, aes(color = "red"), size = 1 ) + 
  labs(x = "SVL (mm)", y = "mass (g)") + 
  scale_color_identity( name = "Model",
  breaks = c("blue", "green", "orange", "red"), labels = c("Linear", "Log-linear", "Log-log", "Quadratic"), guide = "legend" ) + 
  theme_bw()
# m2(log-linear model is the best one)
# From the graph, we can see the green line can fit the data well for small SVL and large SVL values, and it is a quadratic curve

