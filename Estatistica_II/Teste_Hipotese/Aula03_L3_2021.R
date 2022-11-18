#1) Teste-T 

#Deseja-se avaliar a efetividade de uma dieta na redução do nível de colesterol. 
#A tabela a seguir mostra uma sequência de valores do nível de colesterol 
#de 8 indivíduos antes e depois da dieta.

#nivel de colesterol antes da dieta 
x1 = c(201, 231, 221, 260, 228, 237, 326, 284)

#nivel de colesterol depois da dieta 
x2 = c(200, 202, 216, 233, 224, 216, 296, 210)

#tamanho da amostra 
n = length(x1)

#Verifique, ao nivel de 5% de significancia, 
#se a dieta é eficaz para reducao do colesterol.

#H0 = mu_d = 0  Nao Ha indicios que a dieta foi eficaz
#Ha = mu_d < 0      Ha indicios que a dieta foi eficaz

alpha = 0.05 #5% of significance 

#diferenca -> depois da dieta - antes a dienta 
d_x2_x1 = x2 - x1  
media_d_x2_x1 = mean(d_x2_x1)

#variancia das diferencas amostrais 
var_d_x2_x1 = var(d_x2_x1)

#desvio padrao das diferencas amostrais 
sd_d_x2_x1 = sqrt(var_d_x2_x1)

#estatistica do teste 
tcal = (media_d_x2_x1)/(sd_d_x2_x1/sqrt(n)) 
print(tcal)           # test statistic 

ttab = qt(alpha, df=n-1)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#or using t.test to solve the problem 


# H0 = mud = 0
# Ha = mud < 0 (teste unilateral a esquerda)


help(t.test)

#nivel de colesterol antes da dieta 
x1 = c(201, 231, 221, 260, 228, 237, 326, 284)

#nivel de colesterol depois da dieta 
x2 = c(200, 202, 216, 233, 224, 216, 296, 210)

#tamanho da amostra 
n = length(x1)

alpha = 0.05          # 5% of significance 


#var.equal = FALSE -> variancia heterogenea 
#var.equal = TRUE  -> então a estimativa combinada da variância é usada (variância homogenea)


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

# paired=TRUE -> Se emparelhado for VERDADEIRO, tanto x como y devem ser especificados e devem ter o mesmo comprimento.

#conf.level   -> nível de confiança do intervalo.

TEST <- t.test(x1, x2, alternative = "greater", var.equal = FALSE, paired=TRUE, conf.level = 0.95)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#2)  Teste-F 
#Com o objetivo de verificar se duas maquinas produzem pecas 
#com a mesma homogeneidade (variancia) quanto 
#a resistencia, sorteou-se duas amostras de seis 
#pecas de cada máquina obtendo as seguintes resistencias

Maq_A = c(145, 127, 136, 142, 141, 137)
Maq_B = c(143, 128, 132, 138, 142, 132)

#tamanho da amostra 
n_A = length(Maq_A)
n_B = length(Maq_B)

alpha = 0.05          # 5% of significance 

#H0 = mu_Maq_A = mu_Maq_B  -> Variancia populacional da Maq_A = variancia populacional da Maq_B 
#Ha = mu_Maq_A > mu_Maq_B  -> Variancia populacional da Maq_A > variancia populacional da Maq_B 

#variancia amostral 
S2_A = var(Maq_A)
S2_B = var(Maq_B)

#Estatistica do Teste 

if (S2_A > S2_B) {  
  F_cal = S2_A / S2_B 
} else {
  F_cal = S2_B / S2_A 
}  
print(F_cal) 


#http://www.r-tutor.com/elementary-statistics/probability-distributions/f-distribution
#http://www.sthda.com/english/wiki/f-test-compare-two-variances-in-r

help(qf)

if (S2_A > S2_B) {  
  F_tab = qf(1-alpha, df1=n_A-1, df2=n_B-1)  
} else {
  F_tab = qf(1-alpha, df1=n_B-1, df2=n_A-1)  
}  
print(F_tab) 

if (abs(F_cal) > abs(F_tab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}



#or using var.test to solve the problem 

help(var.test)

Maq_A = c(145, 127, 136, 142, 141, 137)
Maq_B = c(143, 128, 132, 138, 142, 132)

alpha = 0.05          # 5% of significance 

#H0 = mu_Maq_A = mu_Maq_B  -> Variancia populacional da Maq_A = variancia populacional da Maq_B 
#Ha = mu_Maq_A > mu_Maq_B  -> Variancia populacional da Maq_A > variancia populacional da Maq_B 


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

#conf.level   -> nível de confiança do intervalo.

TEST <- var.test(Maq_A, Maq_B, alternative = "greater", conf.level = 1-alpha)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#3)  Teste Qui-Quadrado 

#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r


#125 proprietarios de certa marca de automovel 
#foram entrevistados acerca do desempenho e do 
#consumo de combustível de seus carros. 
#O resultado da pesquisa de opinioes resumido na seguinte tabela:

#Verificar, ao nivel de 5% de significância, 
#se devemos considerar que, no consenso geral, 
#desempenho e consumo nao guardam relacao entre si

#Consumo            Desempenho 
#            Pessimo   Regular  Bom    Total        
#Alto           29        27    42      98
#Baixo           4         6    17      27

#      Total    33        33    59      125


#matriz de valores observados 
df <- read.csv('consumo_x_desempenho.csv', sep = ',', dec = '.')
rownames(df) <- c("Consumo_Alto", "Consumo_Baixo")
df  <- within(df, rm("X"))    #remover a coluna variavel   (ï..)
df_matrix_obs <- as.matrix(df)


#matriz de valores esperados 
#https://www.datamentor.io/r-programming/matrix/

df_matrix_esp <- matrix(1:6, nrow = 2, ncol = 3, dimnames = list(c("Consumo_Alto","Consumo_Baixo"), c("Desempenho_Pessimo","Desempenho_Regular","Desempenho_Bom")))

colnames(df_matrix_esp) <- colnames(df_matrix_obs)
rownames(df_matrix_esp) <- rownames(df_matrix_obs)


for (l in (1:2))
{
  for (c in (1:3))  
  {
    df_matrix_esp[l,c] = (sum(df_matrix_obs[l,])*sum(df_matrix_obs[,c]))/125
  }
}


#qui_quadrado calculado 

qui_cal = 0 
for (l in (1:2))
{
  for (c in (1:3))  
  {
    qui_cal = qui_cal + (df_matrix_obs[l,c] - df_matrix_esp[l,c])^2/df_matrix_esp[l,c]
  }
}
print(qui_cal)

#https://statsandr.com/blog/chi-square-test-of-independence-by-hand/
#http://www.r-tutor.com/elementary-statistics/probability-distributions/chi-squared-distribution

df = (2-1)*(3-1)

alpha = 0.05 
qui_tab <- qchisq(1 - alpha, df=df)        # 2 degrees of freedom 
print(qui_tab)

if (abs(qui_cal) > abs(qui_tab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#solve using  chisq.test(...)


df <- read.csv('consumo_x_desempenho.csv', sep = ',', dec = '.')
rownames(df) <- c("Consumo_Alto", "Consumo_Baixo")
df  <- within(df, rm("X"))    #remover a coluna variavel   (ï..)

#df_matrix <- as.matrix(df)

library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(df))
# 2. Graph
balloonplot(t(dt), main ="df", xlab ="", ylab="", label = FALSE, show.margins = FALSE)
            
#H0 = desempenho e consumo sao independentes
#Ha = desempenho e consumo nao sao independentes

TEST <- chisq.test(df)

print(TEST$p.value)
print(TEST$observed)
print(TEST$expected)
print(TEST$residuals)


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
#In the image above, it’s evident that there are an association between the column Desempenho_pessimo and the row Consumo_Alto.
#There is a strong positive association between the column Desempenho_bom and the row Consumo_Baixo


#2)Negative residuals are in red. This implies a repulsion (negative association) 
#between the corresponding row and column variables. 
#For example the column Desempenho_Bom are negatively associated (~ “not associated”) with the row Consumo_Alto. 
#There is a repulsion between the column Desempenho_Pessimo and, the row Consumo_Baixo
  


