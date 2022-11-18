#Estatistica Descritiva 

#Lista 1

#1a) 
#https://stackoverflow.com/questions/18799901/data-frame-group-by-column
#https://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector


atividades <- c("cafe" , "milho", "milho", "oleicultura", 
                "leite", "soja", "cafe", "cafe", 
                "leite", "leite", "laranja", "milho", 
                "milho", "leite", "leite", "cafe", 
                "cafe", "cafe", "cafe", "cafe")

tab_atividades <- table(atividades)

df_atividades <- as.data.frame(tab_atividades)


#renomear as colunas do dataframe 

names(df_atividades)[names(df_atividades) == "Freq"]   <- "frequencia"

#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

#install.packages("ggplot2")
library(ggplot2)


p <- ggplot(data=df_atividades, aes(x=atividades, y=frequencia, fill = atividades)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=frequencia), vjust=-0.5, size=3.5) + 
  theme_classic()

print(p)


cafe <- rep("Cafe", 8)
leite <- rep("leite", 5)
milho <- rep("milho", 4)
soja  <- rep("soja", 1)
laranja <- rep('laranja', 1)
oleicultura <- rep('oleicultura', 1)

#concatenar os vetores 
atividades <- c(cafe, leite, milho, soja, laranja, oleicultura)



#https://www.geeksforgeeks.org/absolute-and-relative-frequency-in-r-programming/

#https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/

freq_abs  <- table(atividades)

print(freq_abs)

freq_relative <- freq_abs / length(atividades)

print(freq_relative)

porcentagem <- freq_relative * 100

print(porcentagem)


#1b) 

#elemento com maior frequencia absoluta 
names(freq_abs)[which.max(freq_abs)]


#ordenar a tabela de frequencia em ordem decrescente 
freq_abs_ord <- sort(freq_abs,decreasing=T)

#obter o elemento e sua frequencia 
freq_abs_ord[1]


#2a) 

class_A <- 0.22
class_B <- 0.18
class_C <- 0.40
class_D <-  1 - (class_A + class_B + class_C)
print(class_D)

#2b)
length_amostra = 200 

freq_abs = class_D * length_amostra
print(freq_abs)


#3a) 

#gerar número randomicos 
length_amostra = 30 
notas <- runif(length_amostra, min = 1, max = 10) 
print(notas)  

#or input values by user 
N <- as.numeric(readline("Informe a quantidade de números: "))
notas <- vector()  # making sure x is empty

for (n in (1:N)) 
{
  new_note <- readline(paste0("Informe o número ", n, " of ", N, ": "))
  notas <- append(notas, as.numeric(new_note))
}

cat("Números digitados:\n")
print(notas)


nota_ini <- 1.0 
nota_fim <- 10.0
interval_notes <- 1.5 

#https://stackoverflow.com/questions/29215589/while-vs-repeat-loops-in-r


while (nota_ini < nota_fim)
{

  print(paste("Notal Inicial:", nota_ini , " Nota Final:", nota_ini + interval_notes))
  
  notas_no_intervalo <- notas[notas >= nota_ini & notas <  nota_ini + interval_notes]

  print(paste("Notas no Intervado: ", format(notas_no_intervalo, digits=2)))
  
  freq_abs_nota <- length(notas_no_intervalo)
  print(paste("Frequência Absoluta: ", freq_abs_nota))
  
  freq_rel_notas <- freq_abs_nota / length(notas)
  print(paste("Frequencia Relativa: ", format(freq_rel_notas, digits=2)))  #só funciona para digitos <> de zero,  Ex: format(1.23456,digits=3) 
  
  porcentagem <- freq_rel_notas * 100
  print(paste("Porcentagem: ", format(porcentagem, digits=2)))
  
  #incrementa o intervalo de notas 
  nota_ini <- nota_ini + interval_notes  
  
  cat("\n")

}
  

#forma obsoleta de ser fazer -> imagine um intervalo de 1 até 100 
"
notas1 <- notas[notas >= 1.0 & notas <  2.5]
notas2 <- notas[notas >= 2.5 & notas <  4.0]
notas3 <- notas[notas >= 4.0 & notas <  5.5]
notas4 <- notas[notas >= 5.5 & notas <  7.0]
notas5 <- notas[notas >= 7.0 & notas <  8.5]
notas6 <- notas[notas >= 8.5 & notas <=  10.0]
"


#uma solução classica em R 
#http://www.r-tutor.com/elementary-statistics/quantitative-data/frequency-distribution-quantitative-data

length_amostra = 30 
notas <- c(6.5, 3.2, 9.3, 4.2, 7.4, 1.2, 8.6, 3.5, 8.0, 3.8,
           1.7, 4.2, 2.1, 4.8, 5.4, 3.3, 3.2, 6.4, 9.1, 5.3, 
           1.9, 4.5, 5.5, 6.1, 7.0, 2.1, 6.2, 5.6, 4.8, 4.7)
print(notas)  

interval <- range(notas)

breaks = seq(1.0, 10.0, by=1.5)

notas.cut <- cut(notas, breaks, right=FALSE)

notas.freq = table(notas.cut)

freq_relative <- notas.freq/length_amostra
freq_relative_format <- format(freq_relative, digits=3)

porcentagem <- freq_relative * 100
porcentagem_format <- format(porcentagem, digits=3)

#exibir o resultado em colunas 
cbind(notas.freq,  freq_relative_format, porcentagem_format)


#4a)

#63 + (x + 54) + 2*x + x/2 = 180

#7x = 126  

x <- solve(7, 126)


#4b

grupos_pessoas <- c(63, 72, 36, 9)
breaks = seq(5.0, 25.0, by=5)

gastos <- vector()


for (i in 1:4)
{
  gastos <- c(gastos , runif(grupos_pessoas[i], min = breaks[i], max = breaks[i+1])) 
} 

print(gastos)  


gastos.cut <- cut(gastos, breaks, right=FALSE)

gastos.freq = table(gastos.cut)

freq_relative <- gastos.freq/length(gastos)
freq_relative_format <- format(freq_relative, digits=2)

porcentagem <- freq_relative * 100
porcentagem_format <- format(porcentagem, digits=2)

#exibir o resultado em colunas 
cbind(gastos.freq,  freq_relative_format, porcentagem_format)

print(porcentagem_format[4])

#4c)


#https://stackoverflow.com/questions/7448881/how-to-access-single-elements-in-a-table-in-r
#se utilizar apenas: freq_relative[1] -> irá retornar o nome da coluna da tabela, como estamos querendo acessar apenas uma celula 
print(freq_relative[[1]][1] + freq_relative[[2]][1])







  






