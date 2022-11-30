x = c(18,  19,  20,  23,  24,  25,  26 )
y = c(5.4, 5.6, 5.3, 4.7, 3.9, 4.0, 3.7)

#plot scatter plot 
library("ggplot2")

ggplot(df, aes(x = x, y = y)) +
       geom_point() 

df = data.frame(x, y)

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

SPDxy <-  (sum_x_y - (sum_x*sum_y)/n)

SQDx <- (sum_x_square - (sum_x^2)/n) 

SQDy <- (sum_y_square - (sum_y^2)/n) 

b1 =  SPDxy / SQDx

b0 = mean_y - b1*mean_x

y_est = b0 + b1*x   #y_est = 10.05 -0.24*x 

#c) calculando o coeficiente de correlacao linear simples

rxy <- SPDxy / sqrt(SQDx*SQDy)

ggplot(df, aes(x = df$x, y = df$y)) +
  geom_point() +
  stat_smooth(method = lm)

#Existe uma associação linear negativa. Com o aumento da idade ha uma tendencia 


#d) calculando o coeficiente de determinacao 

r2 <- rxy^2

SQRegressao <- b1 * SPDxy

SQTotal <- SQDy

r2 <- SQRegressao / SQTotal

#Interpretação do r2
#92.7% é o percentual da variabilidade observada nas notas (Y)
#sendo explicada pela RLS nos valores da idade (X)


#r2 ajustado 

#calcular o R2 ajustado 

r2_ajust <- ((n-1)*r2 - 1) / (n-2)


#RESOLVENDO O EXERCICIO UTILIZANDO FUNÇÃO lm

#b) Ajuste a equação de regressão linear simples.
model <- lm(y ~ x, data = df)

print(model)


#grafico da regressao linear 
ggplot(df, aes(x = df$x, y = df$y)) +
  geom_point() +
  stat_smooth(method = lm)


#Estatistica do teste 
summary(model)



#get b1
b1 <- summary(model)$coefficients[2]

#get bo
b0 <- summary(model)$coefficients[1]

#Exercicio 2 

#Enzima A 
y_est_A =  6.5 * x 

#Enzima B
y_est_B =  10.2 * x 

r2_A = 0.91 
r2_B = 0.98

r2_A_ajust <- r2_A^2
r2_B_ajust <- r2_B^2


#a) A enzima B foi mais eficiente pois apresentou um maior valor de r2

#b) O r2_ajustado que melhor explicou o fenomeno foi o da enzima B



