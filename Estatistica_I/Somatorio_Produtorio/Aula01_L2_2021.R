######################################
#Lista 2 - Produtorio 


"
x1 = 1
x2 = 3
x3 = 4

y1 = 2
y2 = 5
y3 = 0
"

x = c(1, 3, 4)
y = c(2, 5, 0)


#1.a) 
i <- 1:3 
result_1a <- prod(x[i])


#or
i <- 1:3 
result_1a <- prod(x)



#1.b) 
i <- 1:3 
result_1b <- prod(y[i])


#1.c) 
i <- 1:3 
result_1c <- prod(2*x[i])
#or 
result_1c <- 2^3*(prod(x[i]))
#or
result_1c <- 2^length(i)*(prod(x[i]))


#1.d) 
i <- 1:3 
result_1d <- prod(x[i]*y[i])


#1.e) 
i <- 1:2 
result_1e <- (prod(x[i]))^2


#1.f) 
i <- 2:4 
result_1f <- (prod(i+2))^2


#2.a)  exercicio teorico 
#solução: 1x2x3....x n+1 


#suponha n = 5
n = 5
j <- 0:n 
i <- 1:n

num <- prod(j+1)  #1x2x3....x 6  
den <- prod(1^i)  #1x1x1....x 1


result_2a <- num/den