# Question 1
library(tidyverse)
kungsan <- read_csv('http://stats.apiolaza.net/data/kungsan_full.csv')
set.seed(17053777)

# Question 2
my_kungsan <- kungsan %>% 
              sample_n(540) %>% 
              mutate(weight2 = weight ^ 2)

# Question 3
# install.packages("GGally")
library(GGally)
my_kungsan %>% ggpairs(columns = c('weight', 'weight2', 'sex', 'height'))


# Question 4
# install.packages('car')
library(car)
m1 <- lm(height ~ weight, data = my_kungsan)
summary(m1)

m2 <- lm(height ~ weight + weight2, data = my_kungsan)
summary(m2)
vif(m2)

m3 <- lm(height ~ weight + weight2 + sex, data=my_kungsan)
summary(m3)
vif(m3)


# question 5
par(mfrow = c(2, 2))
plot(m3)
par(mfrow = c(2, 2))
plot(m1)

# question6
# install.packages('farver')
my_kungsan <- my_kungsan %>% 
              mutate(weight_c = weight - mean(weight),
                     weight_c2 = weight_c ^ 2)
my_kungsan %>% ggpairs(columns = c('weight_c', 'weight_c2', 'sex', 'height'))


# question 7
m4 <- lm(height ~ weight_c + weight_c2 + sex, data = my_kungsan)
summary(m4)
vif(m4)


# question8
new_people <- tibble(weight = c(50, 50), sex = c('female', 'male')) %>%
              mutate(weight_c = weight - 36, weight_c2 = weight_c ^2)
predict(m4, new_people, interval = 'confidence')


# question9
# library(plyr)
wine <- read_csv('http://stats.apiolaza.net/data/white_wines.csv')
my_wine <- wine %>% sample_n(4800)
my_wine
w1 <- lm(quality~fix_acid+vol_acid+
                    cit_acid+res_sugar+chlorides+free_sulphur+
                    total_sulphur+density+pH+sulphates+
                    alcohol+quality , data=my_wine)
summary(w1)
vif(w1)

# question10
library(leaps)
par(mfrow = c(1, 1))
all_mods <- regsubsets(quality ~ ., data = my_wine)
plot(all_mods, scale = 'Cp')

w2 <- lm(quality ~ (fix_acid  + vol_acid  + res_sugar  + free_sulphur + density  + pH  + sulphates  + alcohol), data = my_wine)
summary(m2)
vif(w2)


#question11
w3 <- lm(quality ~ (fix_acid  + vol_acid  + res_sugar  + free_sulphur + pH  + sulphates  + alcohol), data = my_wine)
summary(w3)
vif(w3)
