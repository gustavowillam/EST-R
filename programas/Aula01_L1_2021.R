######################################
#Lista 1 - #Somatório 

"
x1 = 3
x2 = 4
x3 = 8
x4 = 7
x5 = 6

y1 = 3
y2 = 8
y3 = 2
y4 = 5
y5 = 6
"


#entrada manual em vetor
x <- c(3, 4, 8, 7, 6)
y <- c(3, 8, 2, 5, 6)


#or input values by user 
N <- as.numeric(readline("Informe a quantidade de números: "))
x <- vector()  # making sure x is empty


for (i in 1:N) 
{
  new_value <- readline(paste0("Informe o número ", i, " of ", N, ": "))
  x <- append(x, as.numeric(new_value))
}


cat("Números digitados:\n")
print("Números digitados:\n")
print(x) 


#1.a) 
#result_1a = y1 + y2 + y3 + y4 + y5  


#or using Loops in R
result_1a <- 0
class(result_1a)
for(i in 1:N)
{  
   # i-th element of `y` 
  result_1a <- result_1a + y[i] 
}
print(result_1a)

#more information about Loops in R
#https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r
#https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/

#or 
i <- 1:5
result_1a <- sum(y[i])

#or 
result_1a = sum(y)

#or 
#or input values by csv file
df = read.csv("Aula01_2021.csv", sep = ",", dec = ".")
head(df)

library(plyr)       #para usar função rename 
df <- rename(df, c("ï..x" = "x"))

#or 
#renomear as colunas do dataframe 
names(df)[names(df) == "ï..x"]   <- "x"

#https://stackoverflow.com/questions/23660094/whats-the-difference-between-integer-class-and-numeric-class-in-r

result_1a = sum(df$x)
class(result_1a)  #verifica o tipo da variavel 


#1.b) 
IndexStart <- 1
i <- seq(1, 3, 1)
result_1b <- 0

result_1b <- (y[1]^2)+(y[2]^2)+(y[3]^2)
print(result1)

#or 
result_1b <- sum(y[i]^2)
print(result_1b)


#1.c) 
i <- 2:4 
#or 
i = setdiff(1:5, c(1,5))

result_1c <- sum(x[i]*y[i])


#or
result_1c <- sum(df$x[2:4]*df$y[2:4])



#1.d) 
i = setdiff(1:5, 3)
result_1d <- sum(x[i])


#or 

result_1d <- 0

for(i in 1:5)
{  
    # i-th element of `x` 
    if (i != 3) 
    {
       result_1d <- result_1d + x[i] 
    }
    else
    {
      
    }
}


print(result_1d)



#1.e) 
i <- 1:5 
result_1e <- sum(4*x[i])
#or
result_1e11 <- 4*sum(x[i])


#1.f) 
i <- 3:5 
result_1f <- sum((x[i]+6)^2)


#1.g) 
i <- 2:4 
result_1g <- sum(2*x[i]-3)


##########################################################################


#2.a) 
i <- 1:3 
result_2a <- sum(1^i)


#2.b) 
i <- c(1:3) 
result_2b <- (sum(i+8))^2


#2.c) 
i <- c(-1:2) 
result_2c <- sum(i^2)

