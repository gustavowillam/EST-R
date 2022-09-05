#comandos basicos no R 

notas <- vector()  # cria um vetor para armazenar as notas
total_aprovado = 0
total_reprovado = 0 

#Informa a quantidade de notas 
N <- as.numeric(readline("Informe a quantidade de notas: "))


#laço de repetição for 
for (i in 1:N) 
{
  nota <- readline(paste0("Informe a nota ", i, " de ", N, ": "))
  
  if (nota >= 6)
  {
    print('Aluno aprovado')
    total_aprovado = total_aprovado + 1
  }
  else
  {
    print('Aluno reprovado')
    total_reprovado = total_reprovado + 1
  }
  
  notas <- append(notas, as.numeric(nota))
  
}

print(paste0("Total de alunos aprovados : ", total_aprovado))
print(paste0("Total de alunos reprovados:", total_reprovado))


#função no R 
calcula_media <- function(x){
  
  soma = 0  
  i = 1
  while (i <= 5)
  {
     soma = soma + x[i]
     i = i + 1
  }
  return(soma/5)
}

media <- calcula_media(notas)
print(paste0("A média da turma é :", media))



