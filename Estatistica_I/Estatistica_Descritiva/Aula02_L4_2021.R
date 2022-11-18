#Estatistica Descritiva - Variancia, desvio padrão, erro padrão da media, CV 

#definindo a função erro padrão da media 
epm <- function(x) sd(x)/sqrt(length(x))


#Lista 4



telefonema <- c(1:8)
tempo <- c(1, 3, 6, 15, 8, 1, 4, 2)


df <- data.frame(telefonema, tempo)


#1a) desvio padrão 


result <- sd(tempo)
print(result)

#1b) coeficiente de variação 

cv <- sd(tempo) / mean(tempo) * 100
print(cv)

#1c)  Erro padrão da média   


result <- epm(tempo)
print(result)


#1d)  Amplitude Total 

amplitude <- diff(range(tempo))
print(amplitude)


#2)  variancia amostral 


nr_alunos <- c(17, 12,  7,  7,  1,  6)
idade     <- c(20, 19, 21, 22, 28, 23)

df <- data.frame(nr_alunos, idade)

df$total <- df$nr_alunos * df$idade


A20 <- rep(20, 17)
A19 <- rep(19, 12)
A21 <- rep(21, 7)
A22 <- rep(22, 7)
A28 <- rep(28, 1)
A23 <- rep(23, 6)

#concatenar os vetores 
A <- c(A20, A19, A21, A22, A28, A23)


variancia <- var(A)
print(variancia)

#3a) 
#precisão da média -> erro padrão da média 

A <- c(1.5, 2,   3.5, 4.4, 5)
B <- c(1.8, 2.3, 3,   4.5, 6)

epm_A <- epm(A)
print(epm_A)

epm_B <- epm(B)
print(epm_B)

#ou 
library(sciplot)
erro_padrao_a = se(A)
erro_padrao_b = se(B)


ifelse (epm_A < epm_B, "Localidade A tem maior precisão", "Localidade B tem maior precisão")

#3b) 
#Coeficiente de Variação

cv_A <- sd(A) / mean(A) * 100
print(cv_A)


cv_B <- sd(B) / mean(B) * 100
print(cv_B)


ifelse (cv_A < cv_B, "Localidade A é mais homogênea", "Localidade B é mais homogênea")

