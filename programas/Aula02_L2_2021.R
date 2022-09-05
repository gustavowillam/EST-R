#Estatistica Descritiva 

#Lista 2

#1a) 


sobrenomes <- c("Silva" , "Santos", "Sousa", "Oliveira", "Rodrigues", "Santos", "Oliveira", "Santos", "Sousa", "Ferreira", 
                "Santos", "Silva", "Rodrigues", "Santos", "Ferreira", "Oliveira", "Silva", "Sousa", "Ferreira", "Rodrigues", 
                "Santos", "Oliveira", "Sousa", "Ferreira", "Sousa", "Sousa", "Silva", "Rodrigues", "Ferreira", "Sousa", 
                "Santos", "Oliveira", "Ferreira", "Santos", "Rodrigues", "Silva", "Oliveira", "Santos", "Ferreira", "Rodrigues", 
                "Silva", "Santos", "Rodrigues", "Sousa", "Santos", "Sousa", "Silva", "Santos", "Rodrigues", "Sousa")

tab_sobrenomes <- table(sobrenomes)

df_sobrenomes <- as.data.frame(tab_sobrenomes)


#renomear as colunas do dataframe 

names(df_sobrenomes)[names(df_sobrenomes) == "Freq"]   <- "frequencia"



#or
sobrenomes <- c("silva", "santos", "oliveira", "sousa", "rodrigues", "ferreira")
frequencia <- c(7, 12, 6, 10, 8, 7)

df_sobrenomes <- data.frame(sobrenomes, frequencia)

#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

#install.packages("ggplot2")
library(ggplot2)

p <-ggplot(data=df_sobrenomes, aes(x=sobrenomes, y=frequencia, fill = sobrenomes)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=frequencia), vjust=-0.3, size=3.5) 

print(p)


#1b)

#https://stackoverflow.com/questions/6894246/how-to-sort-a-data-frame-in-r

df_order <- df_sobrenomes[order(-df_sobrenomes$frequencia),]  #to order in decreasing use -  

head(df_order, 2)
#sobrenomes mais comuns: santos, souza 


#2)


intencao <- c("Poupanca", "Pagar Dividas", "Compras", "Viagens")
frequencia <- c(60, 90, 40, 10)


df <- data.frame(intencao, frequencia)

p <-ggplot(data=df, aes(x=intencao, y=frequencia, fill = intencao)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=frequencia), vjust=-0.3, size=3.5) 

print(p)


#3a) 

length_amostra = 20 
salarios <- c(187,184,174,185,175,172,202,197,165,208,215,164,162,172,182,156,172,175,171,183)
print(salarios)  
sort(salarios)
summary(salarios)
mean(salarios)
median(salarios)
sd(salarios)

moda = function(x)
{
  z = table(as.vector(x))
  names(z)[z == max(z)]
}

moda(salarios)


interval <- range(salarios)

breaks = seq(150, 220, by=10)

salarios.cut <- cut(salarios, breaks, right=FALSE)

salarios.freq = table(salarios.cut)

freq_relative <- salarios.freq/length_amostra
freq_relative_format <- format(freq_relative, digits=2)

porcentagem <- freq_relative * 100
porcentagem_format <- format(porcentagem, digits=2)

#exibir o resultado em colunas 
cbind(salarios.freq,  freq_relative_format, porcentagem_format)

#3b) 
#https://www.datamentor.io/r-programming/histogram/
#https://www.tutorialspoint.com/r/r_histograms.htm

hist(salarios)

#4a) 

df = read.csv("poupanca.csv", sep = ",", dec = ".")
head(df)

library(plyr)       #para usar função rename 
df <- rename(df, c("ï..Ano" = "Ano"))
#or 
#renomear as colunas do dataframe 
names(df)[names(df) == "ï..Ano"]   <- "Ano"


df$Ano<-as.factor(df$Ano)

#https://stackoverflow.com/questions/58765726/label-a-barplot-by-number-of-values-with-positive-and-negative-bars

#gerar mais de 12 cores  
library(RColorBrewer)
colourCount = length(unique(df$Ano))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

library(ggplot2)

p <-ggplot(data=df, aes(x=Ano, y=Capitacao_Liquida, fill=Ano)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label = Capitacao_Liquida, vjust = -sign(Capitacao_Liquida)), size=3.5) +     
  #geom_text(aes(label=Capitacao_Liquida), vjust=-1.0, size=3.5) + 
  #scale_fill_grey()                                                 #escala em tom de cinza
  #scale_fill_brewer(palette="Blues")                                #escala em tom de azul 
  scale_fill_manual(values = getPalette(colourCount))


print(p)


df_dep <- which(df$Capitacao_Liquida >= 0)
df_saq <- which(df$Capitacao_Liquida <  0)


#a) 


if (length(df_saq) >= length(df_dep))  {
  print("saques superaram os depositos")
} else  {
    print("depositos superaram os saques")
}


   

