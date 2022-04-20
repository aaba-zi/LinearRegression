#question1
library(tidyverse)
jobs <- read_csv('job_satisfaction2.csv')
set.seed(17053777)
my_js <- jobs %>% 
  sample_n(104) %>% 
  mutate(education_level = factor(education_level, 
                                  levels = c("school", "college", "university"))) %>% 
  mutate(gender = factor(gender))


#question2
my_js %>% ggplot(aes( x = gender,
                      y = score, colour = gender )) +
  geom_jitter(width = 0.1) + 
  facet_wrap(~education_level) # does a nice layout :)


#question3
# If there is no interaction between gender and educational level, the difference in score of different gender is the same in different educational group.
# If there is interaction between educational level and gender, the mean score of different gender could be different in different educational level.


#question4
m1 <- lm(score ~ education_level*gender, data=my_js)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)
#NEED TO TALK MORE
# From the graph, we can see there is nothing to worry about although there is a outlier  around the red dash linie below in  Residuals vs Leverage graph.




#question5
anova(m1)
# p-value for the F-test for the interaction between education level and gender is 0.118651, 
# Null hypothesis is that there is no interaction between educational level and gender, p-value is greater than 0.05, so do not reject null hypothesis.
# so we could drop the interaction in the model as there is no evidence of interaction.



#question6
library(palmerpenguins)
male_penguins <- penguins %>% 
  drop_na() %>% 
  filter(sex == "male")
set.seed(17053777)
my_pen_m <- male_penguins %>% sample_n(150)

#question7
my_pen_m %>% ggplot(aes( x = bill_length_mm, y = bill_depth_mm )) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = lm, se = FALSE)
  # plot the single linear regression line
  # second plot with colored by species.

my_pen_m %>% ggplot(aes( x = bill_length_mm, y = bill_depth_mm, colour = species)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = lm, se = FALSE)
# with no species color, there is one line
# but after adding the color of species, there are 3 lines. the relationship between bill depth and bill length is different for 3 species. 
# And we can see there are 3 different groups by different color.
# so that we can avoid a biased assessment of the impact of bill length on bill depth.



#question8
#no interaction
m2 <- lm(bill_depth_mm ~ bill_length_mm + species, data = my_pen_m)
summary(m2)
#with interaction
m3 <- lm(bill_depth_mm ~ bill_length_mm * species, data = my_pen_m)
summary(m3)
# For the model of no interaction, there is a common slope of three species.
# for the model with interaction, there are three different slope. 
# from the graph of 3 different species, we can see only the slope of Adelie is negative, and close to 0 and the slope for other 2 are positive. 
# But from the graph ignoring species, we can see the slope is negative. 



#question9
anova(m2, m3)
# the null hypothesis is that in the population model the coefficient of bill_length:species interaction is 0.
# alternative hypotheses is that in the population model the coefficient of bill_length:species interaction is not 0.


#question10
# adelie : y = 19.82 - 0.02 * bill_length 
# chinstrap : y = 19.82 - 11.21 + ((- 0.02) + 0.23) * bill_length
# Gentoo : y = 19.82 - 8.17 + ((- 0.02) + 0.10) * bill_length


#question11
par(mfrow = c(2, 2))
plot(m3)
# the graph is fine overall
# Why the graph fits the assumption.
# NEED TO TALK MORE HERE
