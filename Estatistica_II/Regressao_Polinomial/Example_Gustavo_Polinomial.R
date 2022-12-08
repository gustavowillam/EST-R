#http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/
#https://www.geeksforgeeks.org/polynomial-regression-in-r-programming/
#https://www.statology.org/polynomial-regression-r/

library(tidyverse)
library(caret)
library(ggplot2)

#y  = c( <colocar os valores separados por ,>  )  
#x  = c( <colocar os valores separados por ,>  )
#df = data.frame(x, y)


#dataset
df <- read.csv('housing.csv', sep = ',', dec = '.')


#Linear regression
################################################################################

ggplot(df, aes(x = df$LSTAT, y = df$MEDV)) +
  geom_point() +
  stat_smooth(method = lm)

# Build the model
lm_model <- lm(MEDV ~ LSTAT, data = df)

# Summarize the model
summary(lm_model)
print(lm_model)

# Make predictions
df$MEDV_lm <- predict(lm_model, df)


ggplot(df, aes(x = df$MEDV, y = df$MEDV_lm)) +
  geom_point() +
  stat_smooth(method = lm)


# Model performance

RMSE = RMSE(df$MEDV_lm, df$MEDV)
R2 = R2(df$MEDV_lm, df$MEDV)



#Polinomial Regression 
#Polinomio de grau 1
################################################################################


#medv = b0 + b1∗lstat 

pm_model1 <- lm(MEDV ~ LSTAT , data = df)


ggplot(df, aes(x = LSTAT, y = MEDV) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1))


# Summarize the model
summary(pm_model1)

print(pm_model1)

# Make predictions
df$MEDV_pm1 <- predict(pm_model1, df)

# Model performance

RMSE = RMSE(df$MEDV_pm1, df$MEDV)
R2 = R2(df$MEDV_pm1, df$MEDV)



#Polinomial Regression - using glm 
#Polinomio de grau 1
################################################################################


ggplot(df, aes(x = df$LSTAT, y = df$MEDV)) +
  geom_point() +
  stat_smooth()

print(cor(df$LSTAT, df$MEDV))

RP1 <- glm(MEDV ~ LSTAT , data = df)


print(RP1)
summary(RP1)


b0 <- summary(RP1)$coefficients[1]

b1 <- summary(RP1)$coefficients[2]


y_est1 = b0 + b1*df$LSTAT    #y_est = 684138.49 - 17759.04*x 


library(caret)
# (a) Prediction error, RMSE
RMSE(y_est1, df$MEDV)
# (b) R-square
R2(y_est1, df$MEDV)



#Polinomial Regression 
#Polinomio de grau 2
################################################################################


#medv = b0 + b1∗lstat + b2∗lstat^2

pm_model2 <- lm(MEDV ~ LSTAT + I(LSTAT^2)    , data = df)


ggplot(df, aes(x = LSTAT, y = MEDV) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))


# Summarize the model
summary(pm_model2)

print(pm_model2)

# Make predictions
df$MEDV_pm2 <- predict(pm_model2, df)

# Model performance

RMSE = RMSE(df$MEDV_pm2, df$MEDV)
R2 = R2(df$MEDV_pm2, df$MEDV)



#Polinomial Regression 
#Polinomio de grau 2
################################################################################


#medv = b0 + b1∗lstat + b2∗lstat^2


df$LSTAT2 =  df$LSTAT^2

RP2 <- glm(MEDV ~ LSTAT + LSTAT2 , data = df)


print(RP2)
summary(RP2)


ggplot(df, aes(x = df$LSTAT2, y = df$MEDV)) +
  geom_point() +
  stat_smooth()

print(cor(df$LSTAT2, df$MEDV))


b0 <- summary(RP2)$coefficients[1]

b1 <- summary(RP2)$coefficients[2]

b2 <- summary(RP2)$coefficients[3]


y_est2 = b0 + b1*df$LSTAT + b2*df$LSTAT2   #y_est = 829836.55 - -41261.52*x  + 728.39x^2


library(caret)
# (a) Prediction error, RMSE
RMSE(y_est2, df$MEDV)
# (b) R-square
R2(y_est2, df$MEDV)



#Polinomial Regression 
#Polinomio de grau 3
################################################################################

#medv = b0 + b1∗lstat + b2∗lstat^2 +b3*lstat^3 


pm_model3 <- lm(MEDV ~ poly(LSTAT, 3), data = df)


ggplot(df, aes(x = LSTAT, y = MEDV) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 3))


# Summarize the model
summary(pm_model3)

print(pm_model3)

# Make predictions
df$MEDV_pm3 <- predict(pm_model3, df)

# Model performance

RMSE = RMSE(df$MEDV_pm3, df$MEDV)
R2 = R2(df$MEDV_pm3, df$MEDV)


#Polinomial Regression 
#Polinomio de grau 5
################################################################################

#medv = b0 + b1∗lstat + b2∗lstat^2 +b3*lstat^3 + b4*lstat^4 + b5*lstat^5


pm_model5 <- lm(MEDV ~ poly(LSTAT, 5), data = df)


ggplot(df, aes(x = LSTAT, y = MEDV) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5))


# Summarize the model
summary(pm_model5)

print(pm_model5)

# Make predictions
df$MEDV_pm5 <- predict(pm_model5, df)

# Model performance

RMSE = RMSE(df$MEDV_pm5, df$MEDV)
R2 = R2(df$MEDV_pm5, df$MEDV)


#Polinomial Regression 
#Polinomio de grau 10
################################################################################

#medv = b0 + b1∗lstat + b2∗lstat^2 +b3*lstat^3  + .... + b10*lstat^10 


pm_model10 <- lm(MEDV ~ poly(LSTAT, 10), data = df)


ggplot(df, aes(x = LSTAT, y = MEDV) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 10))


# Summarize the model
summary(pm_model10)

print(pm_model10)

# Make predictions
df$MEDV_pm10 <- predict(pm_model10, df)

# Model performance

RMSE = RMSE(df$MEDV_pm10, df$MEDV)
R2 = R2(df$MEDV_pm10, df$MEDV)

