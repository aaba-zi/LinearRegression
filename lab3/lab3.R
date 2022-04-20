library(tidyverse)
set.seed(17053777)
kc_house_data <- read_csv("kc_house_data.csv")
my_houses <- kc_house_data %>% sample_n(20000)
my_houses <- my_houses %>%
  select(-id, -date, -lat, -long, -zipcode, sqft_above,
         -sqft_living15, -sqft_lot15)





library(GGally)
my_houses %>% cor()
ggpairs(my_houses, columns = c("price","sqft_living", "sqft_above", "grade"))
# positive linear correlation in sqft_living. 






m1 <- lm(price ~ (sqft_living + sqft_above + grade), data = my_houses)
summary(m1)




library(leaps)
par(mfrow = c(1, 1))
all_mods <- regsubsets(price ~ ., data = my_houses)
plot(all_mods, scale = 'Cp')
# lower the Cp, the better the model
# floors, sqft_above, yr_built





m2 <- lm(price ~ (bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + grade + yr_renovated), data = my_houses)
summary(m2)





par(mfrow = c(2, 2))
plot(m2)
# From Normmal Q-Q, we observe that after theoretical qualities is greater than 2,
# the observation points are further from predited line. 
# The residual is greater as the qualities increase.
# From Residuals vs Leverage, 
# we observe that there is a few points which are close to the red dash line 0.5,
# which could be considered as outliers.




new_houses <- tibble(bedrooms = c(3, 4), bathrooms = c(1.5, 2.5),
                     sqft_living = c(1200, 1920), sqft_lot = c(15606, 8562),
                     floors = c(1, 2), waterfront = c(0, 0), view = c(0, 0),
                     condition = c(3, 4), grade = c(7, 7),
                     sqft_basement = c(0, 0), yr_built = c(1985, 1994),
                     yr_renovated = c(0, 0))
# confidence is the mean could be, much smaller than prediction
# prediction the value could actually take
predict(m2, new_houses, interval = 'confidence')
predict(m2, new_houses, interval = 'prediction')
new_houses
