#Exercicio 2 - Lista 1 - Leisa

#Determinar probabilidade de um candidato ser aprovado no vestibular
# Y(admit) = 1 -> aprovado
# Y(admit) = 0 -> reprovado

#Variaveis depedentes
#rank -> ranking da escola de proveniencia do aluno
#gre  -> exame padrao (ENEM) que qualifica para o ensino superior 
#gpa  >- exames previos do aluno

library(tidyverse)
library(caret)
library(ggplot2)


#dataset
df <- read.csv('binary.csv', sep = ',', dec = '.')

# Fit the model
model <- glm(admit ~ gre + gpa + rank, data = df, family = "binomial")
# Summarize the model
summary(model)

print(model)

df$probability <- predict(model, df, type = "response")

df$admit_pred <- ifelse(df$probability > 0.5, 1, 0)

# Plot Predicted data and original data points
ggplot(df, aes(x=gre, y=admit_pred)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))


df$admit_pred <- as.factor(df$admit_pred)
df$admit <- as.factor(df$admit)

cm <- confusionMatrix(df$admit_pred, df$admit)
print(cm$table)

accuracy <- mean(df$admit_pred == df$admit)  #acurracy of model 
print(accuracy)

cm$table



#matriz de confusão utilizando table (frequencia)
confusion_matrix <- as.data.frame(table(df$admit_pred, df$admit))

ggplot(data = confusion_matrix, mapping = aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red") # 


#plot logistic regression 
#https://www.geeksforgeeks.org/how-to-plot-a-logistic-regression-curve-in-r/

df <- read.csv("Sample4.csv")
#df <- read.csv('binary.csv')

# create logistic regression model
logistic_model <- glm(var1 ~ var2, data=df, family=binomial)
#logistic_model <- glm(admit ~ gre, data = df, family = "binomial")

#Data frame with hp in ascending order
Predicted_data <- data.frame(var2=seq(min(df$var2), max(df$var2),len=500))
#Predicted_data <- data.frame(gre=seq(min(df$gre), max(df$gre),len=400))

# Fill predicted values using regression model
Predicted_data$var1 = predict(logistic_model, Predicted_data, type="response")
#Predicted_data$admit = predict(logistic_model, Predicted_data, type="response")

# Plot Predicted data and original data points
plot(var1 ~ var2, data=df)
lines(var1 ~ var2, Predicted_data, lwd=2, col="green")

#plot(admit ~ gre, data=df)
#lines(admit ~ gre, Predicted_data, lwd=2, col="green")


# Plot Predicted data and original data points
ggplot(df, aes(x=var2, y=var1)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))



