#question1
library(tidyverse)
library(GGally)
diamonds <- read_csv("diamonds.csv")
set.seed(17053777)
my_diamonds <- diamonds %>% 
  mutate(cut = factor(cut, levels = c("Good", "Very Good", "Premium"))) %>%
  sample_n(640)



# question2
library(GGally)
ggpairs(my_diamonds, columns = c("price","carat", "x", "cut"), aes(colour = cut))


#question3
m1 <- lm(price ~ carat + x + cut + x:cut + carat:cut, data = my_diamonds)
anova(m1)

m2 <- lm(price ~ carat + x + cut + carat:cut + x:cut, data = my_diamonds)
anova(m2)


#question4
m3 <- lm(price ~ carat + x + cut + carat:cut, data = my_diamonds)
anova(m1,m3)
anova(m2,m3)
# the difference between m1 and m3 is m3 without the interaction between x and cut.


#question5
# if carat:cut is expressed earlier than x:cut, there is no too much for x:cut to express, since caract:cut is sort of including x:cut. That’s why the p-value for x:cut in m1 is significant but the p-value of x:cut is not.

#question6
m4 <- lm(price ~ carat + x + cut, data = my_diamonds)
anova(m4)
anova(m1, m4)
# p-value is 2.118 * 10^(-15) is significant. 
# in nested model F-terst of m1 or m2 against m3, we found the p-value is 0.1487 which is less then 0.05, which means x:cut is not significant.
# m3 and m4 are both reduced model. they cannot compare with each other.
# Since we could miss some important interaction that could be significant.
# a group of regressors could each individually have p-values indicating that individually they are not significant but dropping all of them could still be significant. 

# question7
summary(m3)
# cut = good
# price = 5322.76 + 12373.13 * carat - 1997.32 * x
# cut = very good
# price = 5322.76 + 387.71 + (12091.88 -236.02) * carat - 1578.65 * x
# cut = premium
# price = 5322.76 + 63.61 + (12091.88 -143.27  ) * carrot - 1578.65 * x

# what does model m3 represent?)
# Create the regression diagnostic plots for model m3 
par(mfrow = c(2, 2))
plot(m3)


#question8
summary(m3)

my_diamonds <- my_diamonds %>% 
  mutate(price_NZ000 = price * 1.45 / 1000)
my_diamonds

m5 <- lm(price_NZ000 ~ carat + x + cut + carat:cut, data = my_diamonds)
summary(m5)

#question9
new_data = tibble(carat = c(0.25, 0.4, 0.6, 0.9), x = 4.7, cut = 'Premium')
predict(m5, ndewdata = new_data, interval = "confidence")
predict(m5, ndewdata = new_data, interval = "prediction")





