if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(tidyverse)
library(lubridate)
library(caret)
install.packages("CaretEnsemble")
library(caretEnsemble)
install.packages("SMOTE")
library(SMOTE)
install.packages("ROSE")
library(ROSE)
library(mlbench)
install.packages("DMwR")
library(DMwR)
library(rpart)
install.packages("rattle")
library(rattle)
library(rpart.plot)
library(RColorBrewer)



Portdata <- read_delim("bank-full.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

Portdata <- as.data.frame(Portdata)

Portdata <- Portdata %>% mutate(Cust_group = ifelse(balance <0, "below zero", ifelse(between(balance,1,mean(balance)),"below average", ifelse(between(balance,mean(balance),50000), "Above average", "HNI" ))))

Portdata <- Portdata %>% mutate(day_group = ifelse(between(day,1,10),"Early Month", ifelse(between(day,11,20),"Mid Month", "Month End")))

Portdata <- Portdata %>% mutate(Duration_group = ifelse(between(duration,0,mean(duration)),"Less Duration", ifelse(between(duration,mean(duration),1000),"Good Duration", "High Duration" )))
#Summary on dataset
summary(Portdata)

str(Portdata)

head(Portdata)

mean(Portdata$duration)

Portdata %>% group_by(Duration_group,y) %>% filter (duration > 800) %>% summarise(n())

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = Portdata$y, times = 1, p = 0.1, list = FALSE)
Portdata_main <- Portdata[-test_index,]
Portdata_validation <- Portdata[test_index,]



data.frame(bank_additional_full %>% group_by(y) %>% summarise(count = n()))

data.frame(bank_additional_full %>% group_by(job,y) %>% summarise(count = n()))

data.frame(bank_additional_full %>% group_by(marital,y) %>% summarise(count = n()))

data.frame(bank_additional_full %>% group_by(education,y) %>% summarise(count = n()))

data.frame(bank_additional_full %>% group_by(default,y) %>% summarise(count = n()))

data.frame(bank_additional_full %>% group_by(housing,y) %>% summarise(count = n()))

data.frame(bank_additional_full %>% group_by(loan,y) %>% summarise(count = n()))

data.frame(Portdata %>% group_by(loan,y) %>% summarise(count = n()))

data.frame(Portdata %>% group_by(contact,y) %>% summarise(count = n()))

data.frame(Portdata %>% group_by(day,y) %>% summarise(count = n()))


glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]


models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

models_y_hat <- sapply(fits, function(fit_model){
  predict(fit_model, mnist_27$test)
})
dim(models_y_hat)

model_accuracies <- colMeans(models_y_hat == mnist_27$test$y)
mean(model_accuracies)

votes <- rowMeans(models_y_hat == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

min(fits$results$Accuracy)
fits$results$Accuracy

df<-as.data.frame(table(models_y_hat[11,]), stringsAsFactors =TRUE)
df$Var1

y_hat_maj <- sapply(seq(1,nrow(models_y_hat)), function(index_line){
  df <- as.data.frame(table(models_y_hat[index_line,]))
  df[which.max(df$Freq),]$Var1
  df
})
mean(y_hat_maj == mnist_27$test$y)

ind <- acc_hat >= 0.8
votes <- rowMeans(models_y_hat[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
