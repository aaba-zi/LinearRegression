# question1
library(tidyverse)
jobs <- read_csv('prestige.csv')
set.seed(17053777)
my_jobs <- jobs %>% 
  sample_n(95) %>% 
  mutate(income = income * 10) %>% 
  mutate(job_type = factor(job_type)) %>% 
  mutate(log_income = log10(income))
  

# question2
# install.packages('farver')
library(GGally)
my_jobs %>% 
  ggplot(aes(x=education, y=prestige)) + geom_point() + geom_smooth(method='lm')
m1 <- lm(prestige ~ education, data = my_jobs)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)
# From the Residuals vs Fitted graph, the red line is almost around the 0 dash line. So there is nothing too weird 

# question3
my_jobs %>% 
  ggplot(aes(x = income, y = prestige)) + geom_point()
my_jobs %>% 
  ggplot(aes(x = log_income, y = prestige)) + geom_point() + geom_smooth(method='lm')
m2 <- lm(prestige ~ log_income, data = my_jobs)
summary(m2)
par(mfow = c(2, 2))
plot(m2)
# From the scatter plot of prestige versus income, we found we cannot fit in a straight line, we can only fit the points with a curved line. 
# But for the scatter plot of prestige versus log_income, we found we can see a traight line clearly.
# so the log_income would perform slightly better than income.


# question4
my_jobs %>% ggplot(aes(x = education, y = prestige, colour= job_type)) + geom_point()


# question5
m3 <- lm(prestige~education+job_type, data=my_jobs)
summary(m3)
par(mfrow = c(2, 2))
plot(m3)



#question6
# tradie = (2.9859 -6.0239) + 4.6134 * education
# White collar = (2.9859 - 11.4750) + 4.6134 * education
# professional = 2.9859 + 4.6134 * education

# question7
m4 <- lm(prestige~education+job_type+log_income, data=my_jobs)
summary(m4)
par(mfrow = c(2, 2))
plot(m4)

