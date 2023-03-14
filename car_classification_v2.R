library(dplyr)
library(ggplot2)
library(tidyverse)
library(plyr)

options(warn = -1)
set.seed(1234)

path <- "Assignment_2/car_data.csv"
car_data <- read.csv(path)

# Shuffling Data
car_data <- car_data[sample(1 : nrow(car_data)),]


#Correlation
data <- ddply(car_data,.(price,shouldBuy),nrow)
data %>% ggplot(aes(x= price, y = shouldBuy, fill = V1)) +
  geom_tile()
data <- ddply(car_data,.(maintenance,shouldBuy),nrow)
data %>% ggplot(aes(x= maintenance, y = shouldBuy, fill = V1)) +
  geom_tile()
data <- ddply(car_data,.(doors,shouldBuy),nrow)
data %>% ggplot(aes(x= doors, y = shouldBuy, fill = V1)) +
  geom_tile()
data <- ddply(car_data,.(seats,shouldBuy),nrow)
data %>% ggplot(aes(x= seats, y = shouldBuy, fill = V1)) +
  geom_tile()
data <- ddply(car_data,.(safety,shouldBuy),nrow)
data %>% ggplot(aes(x= safety, y = shouldBuy, fill = V1)) +
  geom_tile()
data <- ddply(car_data,.(storage,shouldBuy),nrow)
data %>% ggplot(aes(x= storage, y = shouldBuy, fill = V1)) +
  geom_tile()






#Function created to split the data into Train and Test sets
create_train_test <- function(data, size = 0.8, train = TRUE) {
  # arguments:
  # -data: Dataset used to train the model.
  # -size: Size of the split. By default, 0.8. Numerical value
  # -train: If set to `TRUE`, the function creates the train set, otherwise the test set.
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return(data[train_sample,])
  } else {
    return(data[-train_sample,])
  }
}


#Creating the Train Dataset
train <- create_train_test(car_data, 0.8, train = TRUE)
#Creating the Test Dataset
test <- create_train_test(car_data, 0.8, train = FALSE)

#Checking the dimension of the Train and Test
dim(train)
dim(test)

#Checking the percentage distribution of records according to 'shouldBuy' in Train and Test Dataset.
prop.table(table(train$shouldBuy))
prop.table(table(test$shouldBuy))


#Importing the Libraries
library(rpart)
library(rpart.plot)

#User defined function to plot ROC Curves
plot_multi_roc <- function(roc_arr, title) {
  print(title)
  print("-----------------------------------------------------")
  rs <- roc[['rocs']]
  # defining colors for all variables
  col <- c("salmon", "goldenrod", "lightsteelblue", "green", "purple", "orange")
  # defining legends for all variables
  legends <- c("price", "maintenance", "doors", "seats", "storage", "safety")
  plot.roc(rs[[1]], col = col[1])
  sapply(2:length(rs), function(i) plot(rs[[i]], col = col[i], add = TRUE))
  legend("bottom",
         legend = legends,
         title = title,
         col = col,
         lwd = 4, cex = 0.4, xpd = TRUE, horiz = TRUE)
}

#User Defined fucntion to calculate confusion matrix and accuracy
model_evalution <- function(actual, prediction) {
  # actual - actual list of Y variables
  # predicted - predicted list of Y variables from the model
  confusion_matrix_tree <- table(actual, prediction)
  confusion_matrix_tree

  accuracy <- sum(diag(confusion_matrix_tree)) / sum(confusion_matrix_tree)
  return(accuracy)
}

#Pre-defining Min Split value
control <- rpart.control(minsplit = 20,)

#Training the Data using Decision Tree using 'class' method.
tree <- rpart(shouldBuy ~ ., data = train, method = 'class', control = control)

#Predicting on Test Data
pred <- predict(tree, test, type = 'class')

#Evaluating the accuracy of Decision Tree
model_evalution(test$shouldBuy, pred)

#Importing the Library
library(pROC)

#Plotting ROC Curve
response <- as.numeric(predict(tree, newdata = train, type = "class"))
roc <- multiclass.roc(train$shouldBuy, response)
plot_multi_roc(roc, "Desicion Tree ROC")

# Tuning Desicion Tree
tree <- rpart(shouldBuy ~ ., data = train, method = 'class', control = control, cp = 0.5)

pred <- predict(tree, test, type = 'class')

model_evalution(test$shouldBuy, pred)

response <- as.numeric(predict(tree, newdata = train, type = "class"))
roc <- multiclass.roc(train$shouldBuy, response)
plot_multi_roc(roc, "Desicion Tree ROC (Tuned)")

#Ploting the Desicion Tree
rpart.plot(tree, type = 5, box.palette = "RdBu", shadow.col = "gray", nn = TRUE,)

#Importing the packages for Random Forest
library(randomForest)
library(caret)
library(e1071)


# rf <- train(shouldBuy~.,
#     data = train,
#     method = "rf",
#     metric = "Accuracy",
#     nodesize = 5)

#Training data by RandomForest with default parameter
rf <- randomForest(train[, -1], as.factor(train$shouldBuy))
rf

#Plotting the ROC
response <- as.numeric(predict(rf, newdata = train[, -1]))
roc <- multiclass.roc(as.factor(train$shouldBuy), response)
plot_multi_roc(roc, "Random Forest (Initial)")

#Evaluating the accuracy
pred <- predict(rf, test)
confusionMatrix(pred, as.factor(test$shouldBuy))

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid",
                          allowParallel = TRUE)

# Grid Tuning

# Since mtry = 15 for intitial model, so setting range to 25
tuneGrid <- expand.grid(.mtry = c(1:25))
rf <- train(shouldBuy ~ .,
            train,
            method = "rf",
            metric = "Accuracy",
            tuneGrid = tuneGrid,
            trControl = trControl,
            importance = TRUE,
            nodesize = 5)

#Saving the best mtry from the grid tuning.
best_mtry <- rf$bestTune$mtry
print(best_mtry)

print(rf)


pred <- predict(rf, test)
confusionMatrix(pred, as.factor(test$shouldBuy))
# Finding best max_node
store_maxnode <- 0
tuneGrid <- expand.grid(.mtry = best_mtry)
acc <- 0
max_node <- 0
for (maxnodes in c(10:30)) {
  rf <- train(shouldBuy ~ .,
              train,
              metric = "Accuracy",
              method = "rf",
              tuneGrid = tuneGrid,
              trControl = trControl,
              importance = TRUE,
              nodesize = 5,
              maxnodes = maxnodes)
  acc_irteration <- model_evalution(train$shouldBuy, predict(rf, train))
  if (acc_irteration > acc) {
    acc <- acc_irteration
    max_node <- maxnodes
  }
}

# Finding best n_tree

n_tree <- 0
acc <- 0
for (ntree in (1:30) * 50) {
  rf <- train(shouldBuy ~ .,
              train,
              method = "rf",
              metric = "Accuracy",
              tuneGrid = tuneGrid,
              trControl = trControl,
              importance = TRUE,
              nodesize = 5,
              maxnodes = max_node,
              ntree = ntree)
  acc_irteration <- model_evalution(train$shouldBuy, predict(rf, train))
  if (acc_irteration > acc) {
    acc <- acc_irteration
    n_tree <- ntree
  }
}

print(rf)

#Modifying the train control with "repeatedcv"
tunegrid <- expand.grid(.mtry = best_mtry, .maxnodes = max_node, .ntree = n_tree)
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5,
                          allowParallel = TRUE)

rf <- train(shouldBuy ~ .,
            train,
            method = "rf",
            metric = "Accuracy",
            tuneGrid = tuneGrid,
            trControl = trControl,
            importance = TRUE,
            nodesize = 5,
            tuneLength = 15)

#Plotting the ROC Curves on tuned parameter
response <- as.numeric(predict(rf, newdata = test))
roc <- multiclass.roc(test$shouldBuy, response)
plot_multi_roc(roc, "Random Forest (Final)")
print(rf)
pred <- predict(rf, test)
confusionMatrix(pred, as.factor(test$shouldBuy))

ggplot(varImp(rf))

# Training with optimism_boot and "Logistic Regression"

trControl <- trainControl(method = "optimism_boot",
                          number = 10,
                          search = "random",
                          repeats = 5,
                          classProbs = TRUE,
                          allowParallel = TRUE)
rf <- train(shouldBuy ~ .,
            train,
            metric = "Accuracy",
            method = "LogitBoost",
            preProc = c("center", "scale"),
            trControl = trControl,
            importance = TRUE,
            nodesize = 5,
            tuneLength = 15)

#Plotting the ROC Curves for brnn
response <- as.numeric(predict(rf, newdata = test))
roc <- multiclass.roc(test$shouldBuy, response)
plot_multi_roc(roc, "Logistic Regression")
print(rf)
pred <- predict(rf, test)
confusionMatrix(as.factor(pred), as.factor(test$shouldBuy))




