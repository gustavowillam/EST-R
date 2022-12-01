#Lista 1

#https://r-coder.com/matrix-operations-r/
#https://www.geeksforgeeks.org/operations-on-matrices-in-r/

#1) 
# Creating 1st Matrix
A = matrix(c(4, -1, 5,  2, 0, 1), nrow = 3, ncol = 2) 
B = matrix(c(1,  5, -2, 6, 4, 0), nrow = 2, ncol = 3) 

# t(X + A) == B 

#Matrix X 
x11 <- -3
x21 <- -1
x31 <- -1

x12 <-  3
x22 <-  6
x32 <- -1


X = matrix(c(x11,  x21, x31, x12, x22,  x32), nrow = 3, ncol = 2)  


print(A)
print(B)
print(X)

t_X_A <- t(X + A) 
print(t_X_A)

#https://stackoverflow.com/questions/23032387/how-to-compare-two-matrices-to-see-if-they-are-identical-in-r

if (identical(t_X_A, B)) {
  print("Matrix B is igual Matrix t(X+A)") 
} else {
  print("Matrix B is diferente Matrix t(X+A)") 
}



#2
A = matrix(c(5, 2, 7, 3), nrow = 2, ncol = 2) 

x1 = -1
x2 =  1
X = matrix(c(x1, x2), nrow = 2, ncol = 1) 

B = matrix(c(2,  1),  nrow = 2, ncol = 1) 

print(A)
print(X)
print(B)

A_X <- A %*% X  #A.X (multiplicação matricial)

print(A_X)

if (identical(B, A_X)) {
  print("Matrix B is igual Matrix A.X") 
} else {
  print("Matrix B is difference Matrix A.X") 
}



#3
A = matrix(c(3, 5, 4, 7), nrow = 2, ncol = 2) 
A_inverse <- solve(A)

print(A)
print(A_inverse)

#https://stackoverflow.com/questions/27094402/creating-identity-matrices-in-r/27094585
I <- matrix(0, 2, 2)
diag(I) <- 1
print(I)

I_calc <- A %*% A_inverse
print(I_calc)


#https://stackoverflow.com/questions/5453109/how-to-round-all-values-in-a-matrix
I_calc <- round(I_calc,0)


if (identical(I, I_calc)) {
  print("Matrix I is igual Matrix I_calc") 
} else {
  print("Matrix I is difference Matrix I_calc") 
}



#4
x <- 13
A <- matrix(c(1, 4, 6, 2, 9, x, 1, 4, x-7), nrow = 3, ncol = 3) 
print(A)
det_A <- det(A)

if (det_A == 0) {
  print("Determinante de A é igual a zero") 
} else {
  print("Determinante de A é diferente de zero") 
}






