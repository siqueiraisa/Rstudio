if(!require(caret)) install.packages("caret")
if(!require(e1071)) install.packages("e1071")
if(!require(pROC)) install.packages("pROC")


library(caret)
library(e1071)
library(pROC)


data(iris)
df <- iris[iris$Species != "setosa", ]
df$Species <- factor(df$Species)

set.seed(123)
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

svm_model <- train(
  Species ~ .,
  data = df,
  method = "svmRadial",          
  trControl = train_control,
  metric = "ROC", 
  tuneLength = 10, 
  preProcess = c("center", "scale") 
)


print(svm_model)
plot(svm_model)

best_model <- svm_model$finalModel
cat("Melhores Parâmetros (C, Sigma):\n")
print(svm_model$bestTune)

roc_obj <- roc(svm_model$pred$obs, svm_model$pred$versicolor)
cat("AUC Final:", auc(roc_obj), "\n")
plot(roc_obj, main="Curva ROC - SVM", col="blue")

