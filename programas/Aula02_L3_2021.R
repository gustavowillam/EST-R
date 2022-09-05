#Estatistica Descritiva - Media, Media, Moda 


#https://pt.stackoverflow.com/questions/386187/como-calcular-a-moda-com-valor-bimodal-em-um-data-frame-no-r

moda = function(x)
{
  z = table(as.vector(x))
  names(z)[z == max(z)]
}


#Lista 3

#1a, b) 

telefonema <- c(1:7)
tempo <- c(1, 2, 6, 15, 8, 1, 2)

df <- data.frame(telefonema, tempo)

#media, mediana 
#https://www.statmethods.net/stats/descriptives.html

summary(df$tempo)
#or
mean(tempo)
#or
median(tempo)

#1c)



result <- moda(df$tempo)
print(result)


#2a, b)

disciplina <- c(1:8)
notas <- c(7.5, 6.2, 4.2, 3.9, 4.8, 6.2, 8.0, 5.4)

summary(notas)

#2c)
result <- moda(notas)
print(result)


#3a) 

#https://rstudio-pubs-static.s3.amazonaws.com/350498_65c35f9643d74df2bd18779a0ec607f6.html

tamanho <- c(2:7)
nr_familias <- c(20300, 12000, 11000, 6300, 3000, 2400)

df <- data.frame(tamanho, nr_familias)

media <- sum(df$tamanho * df$nr_familias) / sum(df$nr_familias) 
print(media)

#3b)

F2 <- rep(2, 20300)
F3 <- rep(3, 12000)
F4 <- rep(4, 11000)
F5 <- rep(5, 6300)
F6 <- rep(6, 3000)
F7 <- rep(7, 2400)

#concatenar os vetores 
F <- c(F2, F3, F4, F5, F6, F7)

mean(F)

median(F)

summary(F)

#3c) 

result <- moda(F)
print(result)

hist(F)

#4a) 

S2 <- rep(2, 12)
S3 <- rep(3, 10)
S4 <- rep(4, 8)
S5 <- rep(5, 6)
S6 <- rep(6, 4)


#concatenar os vetores 
F <- c(S2, S3, S4, S5, S6)

result <- mean(F) * 415
print(result)

#4b)
result <- sum(F * 415)
print(result)


