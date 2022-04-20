library(tidyverse)
set.seed(17053777)
fish <- read_csv('http://stats.apiolaza.net/data/fish.csv')
my_fish <- fish %>% sample_n(18)
ggplot(my_fish, aes(fat, energy)) + geom_point(alpha = 0.5)
ggplot(my_fish, aes(protein, energy)) + geom_point(alpha = 0.5)
ggplot(my_fish, aes(ash, energy)) + geom_point(alpha = 0.5)


m1 <- lm(energy ~ protein, data = my_fish)
summary(m1)
m2 <- lm(energy ~ fat, data = my_fish)
summary(m2)
m3 <- lm(energy ~ ash, data = my_fish)
summary(m3)
m4 <- lm(energy ~ protein + fat, data = my_fish)
summary(m4)
m5 <- lm(energy ~ protein + fat + ash, data = my_fish)
summary(m5)



par(mfrow = c(2, 2))
plot(m5)
# whether the residual is normally distributed with 0 mean and constant variance.
# the plot is randomly scatter around the 0 mean




my_fish = my_fish %>% mutate(cent_protein = protein - mean(protein))
my_fish = my_fish %>% mutate(cent_fat = fat - mean(fat))
my_fish = my_fish %>% mutate(cent_ash = ash - mean(ash))
my_fish

m6 <- lm(energy ~ cent_protein + cent_fat + cent_ash, data = my_fish)
summary(m6)




par(mfrow = c(2, 2))
plot(m6)
# THE ONLY DIFFERENCE IS WE CENTERED THE VARIABLES
# the difference is the intercept is different but the slope is exactly the same.
# still the same model but coordinator changed.



