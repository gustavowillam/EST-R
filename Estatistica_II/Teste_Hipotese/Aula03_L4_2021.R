################################################################################
#PARA FAZER A LISTA 1 

#teste t para uma media  -> t.test(x)


#NÃO SERÁ POSSÍVEL RESOLVER A LISTA 1 USANDO t.text(x) 
#POIS NAO FOI INFORMADO OS VALORES DAS AMOSTRAS 


#one-sample t-test is used to compare the mean of one sample to a known standard
#(or theoretical/hypothetical) mean (μ).


#Considera que o peso medio dos estudantes do IF é de 70Kg.
#Admitindo-se um alpha = 5% e com base em uma amostra de
#tamanho 10, cuja média foi de 66.9 kg e desvio padrao 8.29
#Verifique se a média dos pesos dos alunos é inferior a 70kg, apos 
#uma dieta. 


aluno = paste0(rep("Aluno_", 10), 1:10)
peso = c(68, 72, 65, 60, 50, 74, 66, 63, 71, 80)

df <- data.frame(aluno, peso)

mu0   = 70            # hypothesized value 
xbar  = mean(df$peso) # sample mean 
sd    = sd(df$peso)   # sample standard deviation 
n = 10                # sample size 
alpha = 0.05          # 5% of significance 

#a) Hipotese do Teste 

# H0 = mu0 = 70.0
# Ha = mu0 < 70.0 (teste unilateral a esquerda)

#b) Estatistica do Teste 

tcal = (xbar-mu0)/(sd/sqrt(n)) 
print(tcal)           # test statistic 

#c) Regiao Critica

ttab = qt(alpha, df=n-1)   # df = degrees of freedom   -> unilateral test 
                           #to do bilateral test use alpha/2 
print(ttab)

#d) Conclusao 

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#AQUI ESTOU USANDO t.text(x), porque foi fornecido os valores das amostras

#OR using t.test 

# One-sample t-test

#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media populacional
##"greater"    -> media da amostra x > media populacional

#tcal > ttab -> rejeita-se Ho  -> p-valor < nivel de significancia 
#tcal < ttab -> aceita -se Ho  -> p-valor > nivel de significancia 


#a) Hipotese do Teste 

# H0 = mu0 = 70.0
# Ha = mu0 < 70.0 (teste unilateral a esquerda)


#b) Estatistica do Teste 

TEST <- t.test(peso, mu = 70, alternative = "less")
# Printing the results
print(TEST)

#d) Conclusao 

if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#Interpretation of the result
#The p-value of the test is 0.1337, 
#which is less than the significance level alpha = 0.05. 
#We can conclude that the mean weight of the alunos is significantly 
#igual to 70kg with a p-value = 0.1337.


#The result of t.test() function is a list containing the following components:
#statistic: the value of the t test statistics
#parameter: the degrees of freedom for the t test statistics
#p.value: the p-value for the test
#conf.int: a confidence interval for the mean appropriate to the specified alternative hypothesis.
#estimate: the means of the two groups being compared (in the case of independent t test) or difference in means (in the case of paired t test).


################################################################################
#PARA FAZER A LISTA 2 

#Teste t para duas médias  
#Considere variância homogenea para resolver os exercicios 1 e 2  


#The independent t test (or two sample t test) used to compare 
#the means of two unrelated groups of samples.

#Exemplo: 

#Considerando que o peso médio das alunas  do IF é de 50Kg.
#Considerando que o peso médio dos alunos  do IF é de 60Kg.

#Admitindo-se um alpha = 5% 

alpha = 0.05          # 5% of significance 

#com base em uma amostra de tamanho 10, para as meninas com os seguintes dados:
#média = 49.4, desvio padrao = 5.73, variancia = 32.93

menina = paste0(rep("Aluna_", 10), 1:10)
peso = c(48, 42, 55, 50, 60, 54, 46, 43, 51, 45)

df_menina <- data.frame(menina, peso)

F_mu0   = 50                   # hypothesized value 
F_xbar  = mean(df_menina$peso) # sample mean 
F_sd    = sd(df_menina$peso)   # sample standard deviation 
F_var   = var(df_menina$peso)  # sample variance 
F_n = 10                       # sample size 


#com base em uma amostra de tamanho 8, para as meninos com os seguintes dados:
#média = 57.37, desvio padrao = 4.59, variancia = 21.12

menino = paste0(rep("Aluno_", 8), 1:8)
peso = c(58, 52, 55, 60, 64, 54, 63, 53)

df_menino <- data.frame(menino, peso)

M_mu0   = 60                   # hypothesized value 
M_xbar  = mean(df_menino$peso) # sample mean 
M_sd    = sd(df_menino$peso)   # sample standard deviation 
M_var   = var(df_menino$peso)  # sample variance 
M_n = 8                        # sample size 


#Verifique se a média dos pesos das meninas é diferente da media dos pesos dos meninos


#a) Hipotese do Teste 

# H0 = F_mu0 =  M_mu0
# Ha = F_mu0 != M_mu0 (teste bilateral)

#b) Estatistica do Teste 

#variancia homogenea 

sc_2 =  ((F_n -1)*F_var + (M_n -1)*M_var)/(F_n + M_n-2)

tcal = (F_xbar-M_xbar)/sqrt(sc_2 * (1/F_n + 1/M_n)) 
print(tcal)           # test statistic 

#c) Regiao Critica

ttab = qt(alpha/2, df=F_n+M_n-2)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)


#d) Conclusao 

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#NÃO SERÁ POSSÍVEL RESOLVER A LISTA 2 USANDO t.text  para os exercicios 1. 2 e 3
#POIS NAO FOI INFORMADO OS VALORES DAS AMOSTRAS 


#OR using t.test 

# Two-sample t-test


#var.equal = FALSE -> variancia heterogenea 
#var.equal = TRUE  -> então a estimativa combinada da variância é usada (variância homogenea)


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

# paired=TRUE -> Se emparelhado for VERDADEIRO, tanto x como y devem ser especificados e devem ter o mesmo comprimento.

#conf.level   -> nível de confiança do intervalo.

#tcal > ttab -> rejeita-se Ho  -> p-valor < nivel de significancia 
#tcal < ttab -> aceita -se Ho  -> p-valor > nivel de significancia 


#a) Hipotese do Teste 

# H0 = F_mu0 =  M_mu0
# Ha = F_mu0 != M_mu0 (teste bilateral)


#b) Estatistica do Teste 
#variancia homogenea 


TEST <- t.test(df_menina$peso, df_menino$peso, alternative = "two.sided", var.equal = TRUE, paired=FALSE, conf.level = 0.95)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#Interpretation of the result
#The p-value of the test is 0.0028, 
#which is less than the significance level alpha = 0.05. 
#We can conclude that the mean weight of the alunas is significantly 
#diference to mean weight of the alunos with a p-value = 0.0028.


################################################################################
#No exercicio 4 foram dadas os valores das amostras logo sera possivel revolver 
#usando o comando t.test 

#Admitindo-se um alpha = 5% 

alpha = 0.05          # 5% of significance 

#com base em uma amostra de tamanho 10, para as meninas com os seguintes dados:
#média = 49.4, desvio padrao = 5.73, variancia = 32.93

menina = paste0(rep("Aluna_", 10), 1:10)
peso = c(48, 42, 55, 50, 60, 54, 46, 43, 51, 45)

df_menina <- data.frame(menina, peso)

F_mu0   = 50                   # hypothesized value 
F_xbar  = mean(df_menina$peso) # sample mean 
F_sd    = sd(df_menina$peso)   # sample standard deviation 
F_var   = var(df_menina$peso)  # sample variance 
F_n = 10                       # sample size 



#com base em uma amostra de tamanho 8, para as meninas com os seguintes dados:
#média = 59.37, desvio padrao = 4.59, variancia = 21.12

menino = paste0(rep("Aluno_", 8), 1:8)
peso = c(58, 52, 55, 60, 64, 54, 63, 53)

df_menino <- data.frame(menino, peso)

M_mu0   = 60                   # hypothesized value 
M_xbar  = mean(df_menino$peso) # sample mean 
M_sd    = sd(df_menino$peso)   # sample standard deviation 
M_var   = var(df_menino$peso)  # sample variance 
M_n = 8                        # sample size 


#Verifique se a média dos pesos das meninas é diferente da media dos pesos dos meninos


#a) Hipotese do Teste 

# H0 = F_mu0 =  M_mu0
# Ha = F_mu0 != M_mu0 (teste bilateral)

#b) Estatistica do Teste 

#variancia heterogenea  

tcal = (F_xbar-M_xbar)/sqrt(F_var/F_n + M_var/M_n) 
print(tcal)           # test statistic 


#c) Regiao Critica

#n* -> degrees of freedom 
df = ((F_var/F_n + M_var/M_n)^2) / ( ((F_var/F_n)^2)/(F_n - 1) + ((M_var/M_n)^2)/(M_n - 1) )  

df = floor(df)

ttab = qt(alpha/2, df)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)


#d) Conclusao 

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#or using t-test 


#var.equal = FALSE -> variancia heterogenea 
#var.equal = TRUE  -> então a estimativa combinada da variância é usada (variância homogenea)


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

# paired=TRUE -> Se emparelhado for VERDADEIRO, tanto x como y devem ser especificados e devem ter o mesmo comprimento.

#conf.level   -> nível de confiança do intervalo.

#tcal > ttab -> rejeita-se Ho  -> p-valor < nivel de significancia 
#tcal < ttab -> aceita -se Ho  -> p-valor > nivel de significancia 

#a) Hipotese do Teste 

# H0 = F_mu0 =  M_mu0
# Ha = F_mu0 != M_mu0 (teste bilateral)

#b) Estatistica do Teste 

#variancia heterogenea  

TEST <- t.test(df_menina$peso, df_menino$peso, alternative = "two.sided", var.equal = FALSE, paired=FALSE, conf.level = 0.95)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


################################################################################
#PARA FAZER A LISTA 3 

#teste t para duas amostras pareadas 


#Admitindo-se um alpha = 5% e com base em uma amostra de tamanho 10, 
#Verifique se a dieta foi eficaz 


x1 = c(68, 72, 65, 60, 50, 74, 66, 63, 71, 80)   #antes da dieta 
x2 = c(65, 68, 56, 48, 49, 62, 54, 57, 59, 76)   #apos a dieta 

#tamanho da amostra 
n = length(x1)

#Verifique, ao nivel de 5% de significancia, 
#se a dieta é eficaz para reducao do peso

alpha = 0.05 #5% of significance 

#diferenca -> depois da dieta - antes a dienta 
d_x2_x1 = x2 - x1  
media_d_x2_x1 = mean(d_x2_x1)

#variancia das diferencas amostrais 
var_d_x2_x1 = var(d_x2_x1)

#desvio padrao das diferencas amostrais 
sd_d_x2_x1 = sqrt(var_d_x2_x1)


#a) Hipotese do Teste 

#H0 = mu_d = 0  Nao Ha indicios que a dieta foi eficaz
#Ha = mu_d < 0      Ha indicios que a dieta foi eficaz

#b) Estatistica do Teste 

tcal = (media_d_x2_x1)/(sd_d_x2_x1/sqrt(n)) 
print(tcal)           # test statistic 

#c) Regiao Critica

ttab = qt(alpha, df=n-1)   # df = degrees of freedom   -> unilateral test 
                           #to do bilateral test use alpha/2 
print(ttab)

#d) Conclusao 

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#or using t.test to solve the problem 


#a) Hipotese do Teste 

#H0 = mu_d = 0  Nao Ha indicios que a dieta foi eficaz
#Ha = mu_d < 0      Ha indicios que a dieta foi eficaz (d < 0 -> x2 - x1 < 0 -> x1 > x2)


help(t.test)

#peso antes da dieta 
x1 = c(68, 72, 65, 60, 50, 74, 66, 63, 71, 80)  #antes da dieta 

#peso apos da dieta 
x2 = c(65, 68, 56, 48, 49, 62, 54, 57, 59, 76)  #apos a dieta 


#tamanho da amostra 
n = length(x1)

alpha = 0.05          # 5% of significance 


#var.equal = FALSE -> variancia heterogenea 
#var.equal = TRUE  -> então a estimativa combinada da variância é usada (variância homogenea)


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

#paired=TRUE -> Se emparelhado for VERDADEIRO, tanto x como y devem ser especificados e devem ter o mesmo comprimento.

#conf.level   -> nível de confiança do intervalo.


#b) Estatistica do Teste 

TEST <- t.test(x1, x2, alternative = "greater", var.equal = FALSE, paired=TRUE, conf.level = 0.95)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#teste F para comparação da variância de 2 populações

#http://www.r-tutor.com/elementary-statistics/probability-distributions/f-distribution
#http://www.sthda.com/english/wiki/f-test-compare-two-variances-in-r

#2)  Teste-F 
#Com o objetivo de verificar se duas amostras de alunos possuem pesos 
#com a mesma homogeneidade (variancia) 
#sorteou-se duas amostras de dez
#alunos obtendo os seguintes pesos 

x =  c(68, 72, 65, 60, 50, 74, 66, 63, 71, 80) 
y =  c(65, 68, 56, 48, 49, 62, 54, 57, 59, 76)   

#tamanho da amostra 
n_x = length(x)
n_y = length(y)

#variancia amostral 
S2_x = var(x)
S2_y = var(y)


alpha = 0.05          # 5% of significance 

#a) Hipotese do Teste 

#H0 = mu_x = mu_y  -> Variancia populacional de X  = variancia populacional de y
#Ha = mu_x > mu_y  -> Variancia populacional de x  > variancia populacional da y


#b) Estatistica do Teste 

if (S2_x > S2_y) {  
  F_cal = S2_x / S2_y 
} else {
  F_cal = S2_y / S2_x 
}  
print(F_cal) 

#c) Regiao Critica

help(qf)

if (S2_x > S2_y) {  
  F_tab = qf(1-alpha, df1=n_x-1, df2=n_y-1)  
} else {
  F_tab = qf(1-alpha, df1=n_y-1, df2=n_x-1)  
}  
print(F_tab) 

#d) Conclusao 

if (abs(F_cal) > abs(F_tab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#or using var.test to solve the problem 

help(var.test)

x =  c(68, 72, 65, 60, 50, 74, 66, 63, 71, 80) 
y =  c(65, 68, 56, 48, 49, 62, 54, 57, 59, 76)   

alpha = 0.05          # 5% of significance 

#a) Hipotese do Teste 

#H0 = mu_x = mu_y  -> Variancia populacional de X  = variancia populacional de y
#Ha = mu_x > mu_y  -> Variancia populacional de x  > variancia populacional da y


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

#conf.level   -> nível de confiança do intervalo.

TEST <- var.test(x, y, alternative = "greater", conf.level = 1-alpha)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#3)  Teste Qui-Quadrado 

#https://statsandr.com/blog/chi-square-test-of-independence-by-hand/
#http://www.r-tutor.com/elementary-statistics/probability-distributions/chi-squared-distribution


#150 pessoas foram entrevistados acerca dos sintomas desenvolvidos durante a pandemia 
#O resultado da pesquisa de opinioes resumido na seguinte tabela:

#Verificar, ao nivel de 5% de significância, 
#se devemos considerar que, no consenso geral, 
#sintomas  e pandemia nao guardam relacao entre si

#Sintomas               Pandemia de Covid-19
#                     Antes   Durante    Total        
#Crise de Ansiedade    24        26        50
#Depressao             18        32        50
#Sente-se Infeliz?     22        28        50


#             Total    64        86       150


#matriz de valores observados 
df_matrix_obs <- matrix(1:6, nrow = 3, ncol = 2)
rownames(df_matrix_obs) <- c("Crise de Ansiedade", "Depressao", "Se sente Infeliz")
colnames(df_matrix_obs) <- c("Pre_Pandemia", "Durante_Pandemia")

df_matrix_obs[1,1] = 24
df_matrix_obs[1,2] = 26

df_matrix_obs[2,1] = 18
df_matrix_obs[2,2] = 32

df_matrix_obs[3,1] = 22
df_matrix_obs[3,2] = 28


#df <- read.csv('covid_19.csv', sep = ';', dec = '.')
#rownames(df) <- c("Crise de Ansiedade", "Depressao", "Se sente Infeliz")
#df  <- within(df, rm("ï.."))    #remover a coluna variavel 
#df_matrix_obs <- as.matrix(df)


#matriz de valores esperados 
#https://www.datamentor.io/r-programming/matrix/

df_matrix_esp <- matrix(1:6, nrow = 3, ncol = 2)

colnames(df_matrix_esp) <- colnames(df_matrix_obs)
rownames(df_matrix_esp) <- rownames(df_matrix_obs)


for (l in (1:3))  {
  for (c in (1:2)) {
    df_matrix_esp[l,c] = (sum(df_matrix_obs[l,])*sum(df_matrix_obs[,c]))/150
  }
}



#a) Hipotese do Teste 

#H0 = As variaveis     sao independentes -> não existe relação entre os sintomas e a pandemia 
#Ha = As variaveis nao sao independentes ->     existe relação entre os sintomas e a pandemia 


#b) Estatistica do Teste 

#qui_quadrado calculado 
qui_cal = 0 

for (l in (1:3))  {
  for (c in (1:2))  {
    qui_cal = qui_cal + (df_matrix_obs[l,c] - df_matrix_esp[l,c])^2/df_matrix_esp[l,c]
  }
}
print(qui_cal)

#c) Regiao Critica

df = (3-1)*(2-1)

alpha = 0.05 
qui_tab <- qchisq(1 - alpha, df=df)        # 2 degrees of freedom 
print(qui_tab)

#d) Conclusao 

if (abs(qui_cal) > abs(qui_tab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#solve using  chisq.test(...)


#df <- read.csv('covid_19.csv', sep = ',', dec = '.')
#rownames(df) <- c("Crise de Ansiedade", "Depressao", "Se sente Infeliz")
#df  <- within(df, rm("ï.."))    #remover a coluna variavel 

#df_matrix <- as.matrix(df)

library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(df_matrix_obs))
# 2. Graph
balloonplot(t(dt), main ="df_matrix_obs", xlab ="", ylab="", label = FALSE, show.margins = FALSE)


#a) Hipotese do Teste 

#H0 = As variaveis     sao independentes -> não existe relação entre os sintomas e a pandemia 
#Ha = As variaveis nao sao independentes ->     existe relação entre os sintomas e a pandemia 


TEST <- chisq.test(df_matrix_obs)

print(TEST$p.value)
print(TEST$observed)
print(TEST$expected)
#print(TEST$residuals)


alpha = 0.05          # 5% of significance 

if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

help(corrplot)

library(corrplot)
corrplot(TEST$residuals, is.corr = FALSE)

#is.corr = Lógico, se a matriz de entrada é uma matriz de correlação ou não. 
#Podemos visualizar a matriz de não correlação definindo is.corr = FALSE.


#1)Positive residuals are in blue. 
#Positive values in cells specify an attraction (positive association) 
#between the corresponding row and column variables.
#In the image above, it’s evident that there are an association between the column Pré_Pandemia and the row Crise de Ansiedade.
#There is a strong positive association between the column Durante_Pandemia and the row Depressão. 


#2)Negative residuals are in red. This implies a repulsion (negative association) 
#between the corresponding row and column variables. 
#For example the column Durante_Pandemia are negatively associated (~ “not associated”) with the row Crise de Ansiedade. 
#There is a repulsion between the column Pre Pandemia and, the row Depressão. 
