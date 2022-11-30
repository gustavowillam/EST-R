x = c(18,  19,  20,  23,  24,  25,  26 )
y = c(5.4, 5.6, 5.3, 4.7, 3.9, 4.0, 3.7)

df = data.frame(x, y)

#plot scatter plot 
library("ggplot2")

ggplot(df, aes(x = x, y = y)) +
       geom_point() 

cor(y, x)

n = length(x)
#a) Informe os valores das somas a seguir
sum_x = sum(x)

sum_x_square = sum(x^2)

sum_y = sum(y)

sum_y_square = sum(y^2)

sum_x_y = sum(x*y)

mean_x = mean(x)

mean_y = mean(y)

#b) Ajuste a equação de regressão linear simples.

b1 = (sum_x_y - (sum_x*sum_y)/n) / (sum_x_square - (sum_x^2)/n) 

b0 = mean_y - b1*mean_x

y_est = b0 + b1*x   #y_est = 10.05 -0.24*x 

#c) Interprete b1 

#Estima-se uma diminuição em -0.24 nas notas  para cada 1 ano de aumento na idade 

#c) Interprete b0 
#Não tem interpretação prática uma vez que a variável x não passa na origem

#e) Qual o valor estimado das notas quando a idade e de 21 anos?
y_est <-  10.05 -0.24* 21  

#f) Qual o valor estimado e o desvio da regressao quando a 
#idade for 26 anos?

y_est <-  10.05 -0.24*26  

e <- 3.7 - y_est

#g) Qual o valor estimado das notas quando a idade e de 15 anos? 
#Justifique sua resposta.

y_est <-  10.05 -0.24 * 15  

#Trata-se de uma extrapolação, pois estamos utilizando a equação ajustada 
#para prover valores fora do intervalo coberto pela amostra. 

ggplot(df, aes(x = df$x, y = df$y)) +
  geom_point() +
  stat_smooth(method = lm)

#RESOLVENDO O EXERCICIO UTILIZANDO FUNÇÃO lm

#b) Ajuste a equação de regressão linear simples.
model <- lm(y ~ x, data = df)

print(model)


#Estatistica do teste 
summary(model)

#grafico da regressao linear 
ggplot(df, aes(x = df$x, y = df$y)) +
  geom_point() +
  stat_smooth(method = lm)



#c) get b1
b1 <- summary(model)$coefficients[2]

#d) get bo
b0 <- summary(model)$coefficients[1]


#e) Qual o valor estimado das notas quando a idade é de 21 anos?
x <- 21
y_est <- b0 + b1*x 


#f) Qual o valor estimado e o desvio da regressão quando a idade for 26 anos?
x <- 26
y_est <- b0 + b1*x 

#desvio da regressao  para x = 26
Y <-  df[df$x == 26, ]

e = Y$y - y_est 

#g) Qual o valor estimado das notas quando a idade é de 15 anos? Justifique sua resposta.
x <- 15
y_est <- b0 + b1*x 

