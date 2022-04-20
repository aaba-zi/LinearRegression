#Question1, 2
library(tidyverse)
jobs <- read.csv('job_satisfaction1.csv')

jobs = jobs %>% mutate(education_level = factor(education_level, 
                              levels = c("school", "college", "university")))
set.seed(17053777)
my_js <- jobs %>% sample_n(104)

#Question3
my_js %>% group_by(education_level) %>% 
  summarise( count = n(),
             mean_score = mean(score) )
# mean score of college level is 6.32
# mean score of school level is 5.50
# mean score of university level is 8.54


# question4
my_js %>% ggplot(aes(x = education_level, y = score)) + 
  geom_boxplot() + 
  labs( title = "Job satisfaction by education level", 
        x = "Education level", y = "Job satisfaction score" )


# Question5
m1 <- lm(score ~ education_level, data = my_js)
summary(m1)
# The baseline is school

# Question6
par(mfrow = c(2, 2))
plot(m1)


#question7
anova(m1)

# question8
# pr(at least 1 Type I error in 3 similar tests) = 1-(1-0.05) ^ 3

# Question9
TukeyHSD(aov(m1))


# Question10
data("PlantGrowth")
set.seed(17053777)
my_plants <- PlantGrowth %>% sample_n(25)
my_plants

#question 11
my_plants %>% group_by(group) %>% 
  summarise( count = n(),
             mean_weight = mean(weight))

my_plants %>% ggplot(aes(x = group, y = weight)) +
  geom_jitter(width = 0.1) + # width says how big the jitter is 
  labs( x = "Group", y = "Weight" )

#question12
m2 <- lm(weight ~ group, data = my_plants)
summary(m2)
TukeyHSD(aov(m2))



