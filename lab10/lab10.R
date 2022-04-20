#question1
library(tidyverse)
my_defaults <- read.csv("defaults.csv")
set.seed(17053777)
my_defaults <- my_defaults %>% 
  mutate(student = factor(student)) %>%
  mutate(default = factor(default)) %>%
  sample_n(600)


#question2
library(farver)
my_defaults %>% 
  ggplot(aes(x = balance, y = default, colour = student)) +
  geom_jitter(alpha = 0.6, size = 2, height = 0.1) +
  labs(x = "Balance", y = "Default") +
  theme_bw() +
  theme(legend.position = "bottom")
# Yes, we can predict a credit card holder will default using their credit card balance.
# It does not depend on whether they are student or not since there is no obvious separation between students and non students.



#Question3
m1 <- glm(default ~ balance + 
            student +
            balance:student,
          family = binomial(logit),
          data = my_defaults )
summary(m1)


#question4
m2 <- glm(default ~ balance + 
            student,
          family = binomial(logit),
          data = my_defaults)
summary(m2)


#question5
anova(m2, m1, test = "Chisq")
# p-value from Chi suqare test =  0.9204, p value is greater than 0.05, so we don't reject.
# null hypothesis: there is no interaction
# so the interaction between balance and student is not statistically significant.

#question6
# i)
# 0 for No, 1 for Yes
# log(P/(1-P)) = -7.12
# odds = p/(1-P) = e ^ (-7.12)
# p = 1/(1 + e^ (-(-7.12))) = 0.000808
# If that is non student with balance of 0, there is a really small chance to default.

# ii)
# 0 for No, 1 for Yes
# log(P/(1-P)) = -7.12 - 1.37 * 1 = -8.49
# odds = p/(1-P) = e ^ (-8.49)
# p = 1/(1 + e^ (-(-8.49))) = 0.000205471
# If that is a student with balance of 0, there is also a really small chance to default.

# iii)
# For a $1 increase in balance, the odds of default are multiples by exp(0.0054286)


# question7
new_data <- tibble(balance = 1500, student = c("No", "Yes"))
new_data
predict(m2, new_data, type = "response")
# For non student, there will be 0.7349841 probability to default if with balance $1500.
# For student, there will be 0.4136374  probability to default if with balance $1500.
# There is a higher probability for non-student to default with a balance of $1500. 

#question8
# Apparent Error Ratio = (False Positives + False negatives) / total number
# Apparent Error Ratio = (42 + 33) / 600 = 0.125


# save coefficient values 
b0_N <- coef(m2)[[1]] # intercept, not student
b1_N <- coef(m2)[[2]] # logit slope, not student 
# you need to create variables for b0_Y and b1_Y 
# create b0_Y the intercept for students 
# create b1_Y the logit slope for students 
# plot the data and curves # balances data for plotting 
plot_balances <- tibble(balance = seq(0, 3000, 500)) 
ggplot(my_defaults, aes( 
  x = balance,
  y = ifelse(default == "No", -0.1, 1.1), # plot observed points below 0 and above 1
  colour = student )) + geom_jitter(alpha = 0.8, height = 0.1) +
  # plot curve for not students (assumes you have variables for b0_N, b1_N)
  geom_function( data = plot_balances, aes(x = balance), inherit.aes = FALSE,# does not use parent ggplot's aesthetics
                 fun = function(x) plogis(b0_N + b1_N * x), # plogis does expit conversion
                 size = 1, colour = "#F8766D" # emulate ggplot default colours :)
  ) + geom_function(
    # plot curve for students (assumes you have variables for b0_Y, b1_Y)
    data = plot_balances, aes(x = balance), 
    inherit.aes = FALSE, 
    fun = function(x) plogis(b0_Y + b1_Y * x),
    size = 1, colour = "#00BFC4" # emulate ggplot default colours :) 
    ) + # tidy up R's default y-axis 
  scale_y_continuous(expand = c(0, 0), limits = c(-0.2, 1.2), breaks = c(0, 1)) + 
  labs(x = "balance", y = "y = 1 if default, 0 otherwise\n and modeled probabilities") + 
  theme_bw() +
  theme(legend.position = "bottom")

