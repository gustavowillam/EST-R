#1
# H0 = muA = muB
# Ha = muA < muB (teste unilateral a esquerda)

xbar_A  = 136         # sample mean A
xbar_B  = 150         # sample mean B
var_A   = 25          #variance sample A 
var_B   = 100         #variance sample B 
n_A = 10              # sample size A
n_B = 12              # sample size B

alpha = 0.05          # 5% of significance 

#variancia comum 
sc_2 =  ((n_A -1)*var_A + (n_B -1)*var_B)/(n_A+n_B-2)

tcal = (xbar_A-xbar_B)/sqrt(sc_2 * (1/n_A + 1/n_B)) 
print(tcal)           # test statistic 

ttab = qt(alpha, df=n_A+n_B-2)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#2
# H0 = muA = muB
# Ha = muA <> muB (teste bilateral)

xbar_A  = 48         # sample mean A
xbar_B  = 52         # sample mean B
sd_A    = 10         # standard desviation A  
sd_B    = 15         # standard desviation B
var_A   = sd_A^2     # variance sample A 
var_B   = sd_B^2     # variance sample B 
n_A = 15             # sample size A
n_B = 12             # sample size B

alpha = 0.01         # 5% of significance 

#variancia comum 
sc_2 =  ((n_A -1)*var_A + (n_B -1)*var_B)/(n_A+n_B-2)

tcal = (xbar_A-xbar_B)/sqrt(sc_2 * (1/n_A + 1/n_B)) 
print(tcal)           # test statistic 

ttab = qt(alpha/2, df=n_A+n_B-2)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}


#3
# H0 = muF = muM
# Ha = muF > muM (teste unilateral a direita)

F = c(154, 109, 137, 115, 152, 140, 154)
M = c(108, 140, 114, 91, 115, 126, 92)


xbar_F  = mean(F)    # sample mean F
xbar_M  = mean(M)    # sample mean M
var_F   = var(F)     #variance sample F 
var_M   = var(M)     #variance sample M 
n_F = length(F)      # sample size F
n_M = length(M)      # sample size M

alpha = 0.05          # 5% of significance 

tcal = (xbar_F-xbar_M)/sqrt(var_F/n_F + var_M/n_M) 
print(tcal)           # test statistic 

#n* -> degrees of freedom 
df = ((var_F/n_F + var_M/n_M)^2) / ( ((var_F/n_F)^2)/(n_F - 1) + ((var_M/n_M)^2)/(n_M - 1) )  

df = floor(df)

ttab = qt(alpha, df)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 
print(ttab)

if (abs(tcal) > abs(ttab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

#or 

#3
# H0 = muF = muM
# Ha = muF > muM (teste unilateral a direita)


help(t.test)

x = c(154, 109, 137, 115, 152, 140, 154)
y = c(108, 140, 114, 91,  115, 126, 92 )


alpha = 0.05          # 5% of significance 


#var.equal = FALSE -> variancia heterogenea 
#var.equal = TRUE  -> então a estimativa combinada da variância é usada (variância homogenea)


#alternative: c("two.sided", "less", "greater") -> hipotese alternativa 
##"two.sided"  -> teste bilateral 
##"less"       -> media da amostra x < media da amostra y 
##"greater"    -> media da amostra x > media da amostra y 

# paired=TRUE -> Se emparelhado for VERDADEIRO, tanto x como y devem ser especificados e devem ter o mesmo comprimento.

#conf.level   -> nível de confiança do intervalo.

#tcal > ttab -> rejeita-se Ho  -> p-valor < nivel de significancia 
#tcal < ttab -> aceita -se Ho  -> p-valor > nivel de significancia 


TEST <- t.test(x, y, alternative = "greater", var.equal = FALSE, paired=TRUE, conf.level = 0.95)
print(TEST$p.value)


if (TEST$p.value < alpha) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}
