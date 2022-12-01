#Lista 2

y = c(1.5, 6.5, 10.0, 11.0, 11.5, 16.5)
x1 = c(0,  1,   1,    2,    2,    3   )
x2 = c(0,  2,   4,    2,    4,    6   )

#plot scatter plot 
library("ggplot2")


df = data.frame(x1, y)

ggplot(df, aes(x = df$x1, y = df$y)) +
  geom_point() +
  stat_smooth(method = lm)

print(cor(x1, y))


df = data.frame(x2, y)

ggplot(df, aes(x = df$x2, y = df$y)) +
  geom_point() +
  stat_smooth(method = lm)

print(cor(x2, y))


Y = matrix(c(1.5, 6.5, 10.0, 11.0, 11.5, 16.5), nrow = 6, ncol = 1) 
X = matrix(c(1, 1, 1, 1, 1, 1,  0, 1, 1, 2, 2, 3,  0, 2, 4, 2, 4, 6),  nrow = 6, ncol = 3) 

Xt_X <- t(X) %*% X

inverse_Xt_X <- solve(Xt_X)

Xt_Y <- t(X) %*% Y 

b <- inverse_Xt_X %*% Xt_Y

cat(sprintf("\"%f\" +\"%f\"x1 +\"%f\"x2", b[1], b[2], b[3]))


#Multiple Linear Regression in R

#http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/
#http://www.sthda.com/english/articles/40-regression-analysis/165-linear-regression-essentials-in-r/

df = data.frame(x1, x2, y)

RLS <- lm(y ~ x1 + x2, data = df)

print(RLS)

#R2 e R2-Ajustado 
summary(RLS)

b0 <- summary(RLS)$coefficients[1]

b1 <- summary(RLS)$coefficients[2]

b2 <- summary(RLS)$coefficients[3]


y_est = b0 + b1*x1 + b2*x2   #y_est = 6.8695 - 0.2608*x 


library(caret)
# (a) Prediction error, RMSE
RMSE(y_est, y)
# (b) R-square
R2(y_est, y)


#Exercicio 
#A tabela abaixo indica a idade (X) anos de 10 apartamentos e o 
#valor do aluguel (salario minimo) (Y)

y = c(4,    3,   6,   5,   2,  1,  2.5, 3.5, 2.2, 8)   #valor do aluguel (em salarios minimos)

x1 = c(10,  13,  5,   7,   20, 25, 15,  12,  17,  1)   #idade (anos) dos imoveis

x2 = c(80,  70,  120, 110, 60, 50, 65,  85,  62,  150) #area (m2) dos imoveis 


df = data.frame(x1, x2, y)


#plot scatter plot 
library("ggplot2")

ggplot(df, aes(x = df$x1, y = df$y)) +
  geom_point() + 
  stat_smooth(method = lm)  

cor(df$x2, df$y)

RLS <- lm(y ~. , data = df)

print(RLS)

#R2 e R2-Ajustado 
summary(RLS)


b0 <- summary(RLS)$coefficients[1]

b1 <- summary(RLS)$coefficients[2]

b2 <- summary(RLS)$coefficients[3]

y_est = b0 + b1*x1 + b2*x2   

library(caret)
# (a) Prediction error, RMSE
RMSE(y_est, y)
# (b) R-square
R2(y_est, y)
# (c) Mean Absolute Error
MAE(y_est, y)

#d) Qual o valor estimado e o desvio da regressão quando a idade for 10 ano e a area = 80 m2 ?
x11 <- 10
x21 <- 80
y_est1 <- b0 + b1*x11 + b2*x21 

#desvio da regressao  para x = 26
Y <-  df[df$x1 == 10 & df$x2 == 80, ]

e = Y$y - y_est1 


#e) Qual o valor estimado e o desvio da regressão quando a idade for 1 ano e a area = 100 m2 ?
x11 <- 1
x21 <- 100
y_est1 <- b0 + b1*x11 + b2*x21 








