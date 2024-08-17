dataset <- read.csv("D:/FALL2023/IntroToDataScience/Student_Mental_health.csv", header = TRUE, sep = ",")
print(dataset)


missing_values <- is.na(dataset)
print(missing_values)


dataset <- na.omit(dataset)
print(dataset)


catage <- "Age"  
breaks <- c(0, 19, 25, Inf)  
labels <- c("Teenager", "Young Adult", "Adult")
dataset$CatAge <- cut(dataset[[catage]], breaks = breaks, labels = labels, include.lowest = TRUE)
print(dataset)


install.packages(c("e1071", "caret"))
library(e1071)
library(caret)


contingency_table <- table(dataset$What.is.your.CGPA., dataset$Do.you.have.Anxiety.)
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)


fisher_test <- fisher.test(contingency_table)
print(fisher_test)


nb_model <- naiveBayes(Do.you.have.Depression. ~ ., data = dataset)
print(nb_model)


train_indices <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]
print(train_data)
print(test_data)

pred <- predict(nb_model, dataset)
confusion_matrix <- table(dataset$What.is.your.course., pred)
print(confusion_matrix)


set.seed(123)  
folds <- createFolds(dataset$Do.you.have.Depression., k = 10, list = TRUE, returnTrain = TRUE)

for (i in 1:10) {
  train_fold <- dataset[unlist(folds[i]), ]
  test_fold <- dataset[-unlist(folds[i]), ]
  
  nb_model_fold <- naiveBayes(Do.you.have.Depression. ~ ., data = train_fold)

  predictions <- predict(nb_model_fold, test_fold)

  confusion_matrix <- table(test_fold$Do.you.have.Depression., predictions)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  cat("Fold", i, "Accuracy:", accuracy, "\n")
}



metrics <- data.frame(Recall = numeric(10), Precision = numeric(10), F_measure = numeric(10))

for (i in 1:10) {
  train_fold <- dataset[unlist(folds[i]), ]
  test_fold <- dataset[-unlist(folds[i]), ]
  
  nb_model_fold <- naiveBayes(Do.you.have.Depression. ~ ., data = train_fold)
  
  predictions <- predict(nb_model_fold, test_fold)
  
  confusion_matrix <- table(test_fold$Do.you.have.Depression., predictions)
  
  
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  f_measure <- 2 * (precision * recall) / (precision + recall)
  
  metrics[i, ] <- c(Recall = recall, Precision = precision, F_measure = f_measure)
  
  cat("Fold", i, "Recall:", recall, "Precision:", precision, "F-measure:", f_measure, "\n")
}


average_metrics <- colMeans(metrics)
cat("Average Recall:", average_metrics["Recall"], "Average Precision:", average_metrics["Precision"], "Average F-measure:", average_metrics["F_measure"], "\n")




