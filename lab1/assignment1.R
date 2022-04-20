library(tidyverse)
eucalyptus <- read_csv('euc_tricarpa.csv') # Read Eucalyptus tricarpa data set
set.seed(17053777)
my_eucs <- eucalyptus %>% sample_n(500)

ggplot(my_eucs, aes(acoustic_velocity, MOE)) + geom_point()
#As acoustic velocity increases, we observe that the MOE increases as well. They are in a strong positive linear relationship.

model1 <- lm(MOE ~ acoustic_velocity,
            data = my_eucs)

summary(model1)
# intercept regression coefficients = -11.4511,
# acoustic_velocity regression coefficient = 6.0065
# standard error of the residuals = 0.6693
# R^2 * adjusted-R^2 = 0.855 * 0.8547 = 0.73 for two decimal places
# the meaning of the intercept: when the value of x is 0, the value of y is the intercept. In this case, when acoustic velocity is 0, the value of MOE is -11.4511.
# the meaning of the slope: when the value of x increased by 1 unit, how much the value in y will increase.In this case, As the acoustic velocity increases 1 unit, the value of MOE increases by 6.0065.

my_eucs = my_eucs %>% mutate(cent_velocity = acoustic_velocity - mean(acoustic_velocity))
model2 <- lm(MOE ~ cent_velocity,
             data = my_eucs)
summary(model2)
# intercept coefficients = 11.33285
# cent_velocity coefficients = 5.87482
# when acoustic_velocity - mean(acoustic_velocity) = 0, which means acoustic velocity equals the average value of acoustic velocity, the intercept is 11.33285.

model3 <- lm(MOE ~ (acoustic_velocity + dry_density),
   data = my_eucs)
summary(model3)


par(mfrow = c(2, 2))
plot(model3)

library(readr)
write_csv(x = my_eucs, path='eucs.csv')




