
if(!require(randomForest)) install.packages("randomForest")
library(randomForest)


data(iris)
str(iris) 

set.seed(123) 
indices <- sample(1:nrow(iris), 0.8 * nrow(iris))
treino <- iris[indices, ]
teste <- iris[-indices, ]

modelo_rf <- randomForest(Species ~ .,
                          data = treino,
                          ntree = 100,
                          mtry = 2,
                          importance = TRUE)


print(modelo_rf)


previsoes <- predict(modelo_rf, teste)

matriz

if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

data(iris)
str(iris) 

set.seed(123) 
indices <- sample(1:nrow(iris), 0.8 * nrow(iris))
treino <- iris[indices, ]
teste <- iris[-indices, ]

modelo_rf <- randomForest(Species ~ .,
                          data = treino,
                          ntree = 100,
                          mtry = 2,
                          importance = TRUE)

print(modelo_rf)

previsoes <- predict(modelo_rf, teste)

matriz_confusao <- table(Previsoes = previsoes, Real = teste$Species)
print(matriz_confusao)


acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
cat("Acurácia do Modelo:", round(acuracia * 100, 2), "%\n")


importance(modelo_rf)
varImpPlot(modelo_rf)
