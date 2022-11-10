#http://www.r-tutor.com/elementary-statistics/probability-distributions/student-t-distribution
#http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-known-variance

#http://www.sthda.com/english/wiki/t-test
#https://www.statology.org/generate-normal-distribution-in-r/
#http://www.sthda.com/english/wiki/one-sample-t-test-in-r


#1
# H0 = mu0 = 50.0
# Ha = mu0 < 50.0 (teste unilateral a esquerda)

xbar  = 48.2          # sample mean 
mu0   = 50.0          # hypothesized value 
sigma = 7.81          # sample standard deviation 
n = 10                # sample size 
alpha = 0.01          # 1% of significance 
tcal = (xbar-mu0)/(sigma/sqrt(n)) 
print(tcal)           # test statistic 

ttab = qt(alpha, df=n-1)   # df = degrees of freedom   -> unilateral test 
                           #to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")

} else {
  print("Aceita-se H0")
}

#2
# H0 = mu0 = 8000
# Ha = mu0 > 8000 (teste unilateral a direita)

xbar  = 8250          # sample mean 
mu0   = 8000          # hypothesized value 
sigma = 145           # population standard deviation 
n = 6                 # sample size 
alpha = 0.05          # 5% of significance 

tcal = (xbar-mu0)/(sigma/sqrt(n)) 
print(tcal)           # test statistic 

ttab = qt(alpha, df=n-1)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#3
# H0 = mu0 = 600
# Ha = mu0 < 600 (teste unilateral a esquerda)

xbar  = 596.25        # sample mean 
mu0   = 600           # hypothesized value 
sigma = 14.06         # population standard deviation 
n = 50                # sample size 
alpha = 0.01          # 1% of significance 

tcal = (xbar-mu0)/(sigma/sqrt(n)) 
print(tcal)           # test statistic 

ttab = qt(alpha, df=n-1)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
} else {
  print("Aceita-se H0")
}

#4
# H0 = mu0 =  0.85
# Ha = mu0 != 0.85 (teste bilateral)

xbar  = 0.87          # sample mean 
mu0   = 0.85          # hypothesized value 
sigma = 0.01          # population standard deviation 
n = 8                 # sample size 
alpha = 0.05          # 5% of significance 

tcal = (xbar-mu0)/(sigma/sqrt(n)) 
print(tcal)           # test statistic 

ttab = qt(alpha/2, df=n-1)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
} else {
  print("Aceita-se H0")
}


#or generate data using rnorm

xbar <- 48.2  # or whatever your mean is
sd   <- 7.81
n    <- 10    # the number of random values you wish to generate.

# H0 = mu0 = 50.0
# Ha = mu0 < 50.0 (teste unilateral a esquerda)

#set.seed(1)
x <- rnorm(n, xbar, sd) #the function's default standard deviation is already 1
print(x)
mean(x)
median(x)
sd(x)

#library(ggpubr)
hist(x)
boxplot(x)


#one-side 99% confidence interval for mu 
help(t.test)
TEST <- t.test(x, mu=50, alternative = "less", conf.level = 0.99)

attributes(TEST)
TEST$p.value # get the p-value
TEST$parameter # get the degrees of freedom
TEST$statistic # get the t test statistics
