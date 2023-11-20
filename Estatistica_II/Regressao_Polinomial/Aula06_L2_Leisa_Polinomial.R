#Exemplo da Aula Regressão Polinomial 


#O tempo de secagem (em horas) de um verniz e a quantidade (em gramas) de certo aditivo químico são os seguintes:

y  = c(7.2,  6.7, 4.7,  3.7,  4.7,  4.2, 5.2, 5.7 )  #Tempo de Secagem (horas)
x  = c(1  ,  2  , 3  ,  4  ,  5  ,  6,   7,   8   )  #Quantidade de aditivo (gramas)


#a) POLINOMIO DE GRAU 1 


#gráfico de dispersão 
#plot scatter plot 
library("ggplot2")


df1 = data.frame(x, y)

ggplot(df1, aes(x = df1$x, y = df1$y)) +
  geom_point() +
  stat_smooth(method = lm)

print(cor(x, y))


# Build the model
lm_model <- lm(df1$y ~ df1$x, data = df1)

# Summarize the model
summary(lm_model)
print(lm_model)

# Make predictions
df1$y_lm <- predict(lm_model, df1)



ggplot(df1, aes(x = df1$y, y = df1$y_lm)) +
  geom_point() +
  stat_smooth(method = lm)



# Model performance

RMSE = RMSE(df1$y_lm, df1$y)
R2 = R2(df1$y_lm, df1$y)


RP1 <- glm(y ~ x , data = df1)


print(RP1)
summary(RP1)


b0 <- summary(RP1)$coefficients[1]

b1 <- summary(RP1)$coefficients[2]


y_est1 = b0 + b1*x    #y_est = 6.2535 - 0.2202*x 


library(caret)
# (a) Prediction error, RMSE
RMSE(y_est1, y)
# (b) R-square
R2(y_est1, y)


#b) POLINOMIO DE GRAU 2

#Ajuste um modelo de regressão quadrática aos dados.
x2 = x^2


#ENCONTRAR  MANUALMENTE b0, b1, b2 

Y = matrix(c(7.2,  6.7, 4.7,  3.7,  4.7,  4.2, 5.2, 5.7), nrow = 8, ncol = 1) 
X = matrix(c(1,  1,  1,  1,  1,  1,  1,  1,   
             1,  2,  3,  4,  5,  6,  7,  8,   
             1,  4,  9, 16, 25, 36, 49, 64),  
            nrow = 8, ncol = 3) 


Xt_X <- t(X) %*% X

inverse_Xt_X <- solve(Xt_X)

Xt_Y <- t(X) %*% Y 

#b <- (X'.X)-1 * X'.Y

b <- inverse_Xt_X %*% Xt_Y

cat(sprintf("\"%f\" +\"%f\"x1 +\"%f\"x^2", b[1], b[2], b[3]))


#b) POLINOMIO DE GRAU 2 USANDO FUNÇÃO GLM


df2 = data.frame(x, x2, y)

RP2 <- glm(y ~ x + x2, data = df2)


print(RP2)
summary(RP2)


ggplot(df2, aes(x = df2$x2, y = df2$y)) +
  geom_point() +
  stat_smooth()

print(cor(x2, y))


b0 <- summary(RP2)$coefficients[1]

b1 <- summary(RP2)$coefficients[2]

b2 <- summary(RP2)$coefficients[3]


y_est2 = b0 + b1*x + b2*x2   #y_est = 9.244 - 2.0148*x  + 0.1994x^2


library(caret)
# (a) Prediction error, RMSE
RMSE(y_est2, y)
# (b) R-square
R2(y_est2, y)



#c) POLINOMIO DE GRAU 3


#Ajuste um modelo de regressão quadrática aos dados.
x3 = x^3


#b) POLINOMIO DE GRAU 3 USANDO FUNÇÃO GLM


df3 = data.frame(x, x2, x3, y)

RP3 <- glm(y ~ x + x2 + x3, data = df3)


print(RP3)
summary(RP3)


ggplot(df3, aes(x = df3$x3, y = df3$y)) +
  geom_point() +
  stat_smooth()

print(cor(x3, y))


b0 <- summary(RP3)$coefficients[1]

b1 <- summary(RP3)$coefficients[2]

b2 <- summary(RP3)$coefficients[3]

b3 <- summary(RP3)$coefficients[4]



y_est3 = b0 + b1*x + b2*x2   + b3*x3 


library(caret)
# (a) Prediction error, RMSE
RMSE(y_est3, y)
# (b) R-square
R2(y_est3, y)



#Polinomial Regression 
#Polinomio de grau 5
################################################################################

RP5 <- glm(y ~ poly(x, 5), data = df1)


ggplot(df1, aes(x = x, y = y) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5))


# Summarize the model
summary(RP5)

print(RP5)

# Make predictions
y_est5 <- predict(RP5, df1)

# Model performance

library(caret)
# (a) Prediction error, RMSE
RMSE(y_est5, y)
# (b) R-square
R2(y_est5, y)




#Polinomial Regression 
#Polinomio de grau 7
################################################################################

RP7 <- lm(y ~ poly(x, 7), data = df1)


ggplot(df1, aes(x = x, y = y) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 7))


# Summarize the model
summary(RP7)

print(RP7)

# Make predictions
y_est7 <- predict(RP7, df1)

# Model performance

library(caret)
# (a) Prediction error, RMSE
RMSE(y_est7, y)
# (b) R-square
R2(y_est7, y)