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
library(descr)
library(randomForest)

Portdata <- read_delim("bank-full.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
## Duplicate row check
sum(duplicated(Portdata))

## Missing data check
sum(!complete.cases(Portdata))

all.empty = rowSums(is.na(Portdata))==ncol(Portdata)
sum(all.empty)

Portdata.clean = Portdata[!all.empty,]

Portdata.clean = Portdata.clean %>% distinct

nrow(Portdata.clean)

Portdata.clean$missing = !complete.cases(Portdata.clean)

sum(is.na(Portdata))

Portdata <- as.data.frame(Portdata)

Portdata <- Portdata %>% mutate(Cust_group = ifelse(balance <0, "below zero", ifelse(between(balance,1,mean(balance)),"below average", ifelse(between(balance,mean(balance),50000), "Above average", "HNI" ))))

Portdata <- Portdata %>% mutate(day_group = ifelse(between(day,1,10),"Early Month", ifelse(between(day,11,20),"Mid Month", "Month End")))

Portdata <- Portdata %>% mutate(Duration_group = ifelse(between(duration,0,mean(duration)),"Less Duration", ifelse(between(duration,mean(duration),750),"Good Duration", ifelse(between(duration, 750, 1500),"High Duration", "Very High Duration" ))))


Portdata <- Portdata %>% mutate(Campaign_group = ifelse(between(campaign,1,mean(campaign)),"Lean Campaign", ifelse(between(duration,mean(campaign),10),"Average Campaign", ifelse(between(duration, 10, 20),"Ample Campaign", "Heavy Campaign" ))))

Portdata <- Portdata %>% mutate(pdays_group = ifelse(between(pdays,-1,mean(pdays)), "Less gap Pdays", ifelse( between(pdays,mean(pdays), 100),"Medium gap Pdays", "Large gap Pdays")))

Portdata <- Portdata %>% mutate(previous_group = ifelse(previous == 0, "Zero", ifelse(previous==1, "One", "More than once")))

Portdata <- Portdata %>% mutate(y_output = ifelse(y == "no", 0, 1))

Portdata <- Portdata %>% mutate(y_output = as.factor(y_output))

Portdata <- Portdata %>% select(-balance,-day,-duration,-campaign,-pdays,-previous,-y)

Portdata <- Portdata %>% mutate(Cust_group = as.factor(Cust_group),
                                day_group = as.factor(day_group),
                                Duration_group = as.factor(Duration_group),
                                Campaign_group = as.factor(Campaign_group),
                                previous_group = as.factor(previous_group),
                                Duration_group = as.factor(Duration_group),
                                y_output = as.factor(y_output) )

nlevels(Portdata$day_group)
#Summary on dataset
summary(Portdata)

str(Portdata)

head(Portdata)

mean(Portdata$previous)

Portdata %>% group_by(poutcome,y) %>% summarise(n()) 
#%>% filter (duration > 700) 


# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
Valid_index <- createDataPartition(y = Portdata$y, times = 1, p = 0.1, list = FALSE)
Portdata_main <- Portdata[-Valid_index,]
Portdata_validation <- Portdata[Valid_index,]


set.seed(2, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = Portdata_main$y, times = 1, p = 0.2, list = FALSE)
Portdata_main_train <- Portdata_main[-test_index,]
Portdata_main_test <- Portdata_main[test_index,]

summary(Portdata_main_train)


##GLM:

glm_fit <- Portdata_main_train %>% 
    glm(y_output ~ ., data=., family = binomial(link='logit'))
p_hat_logit <- predict(glm_fit, newdata = Portdata_main_test, type = "response") 
y_hat_logit <- ifelse(p_hat_logit > 0.3,1, 0) %>% factor
confusionMatrix(y_hat_logit, Portdata_main_test$y_output)
#$overall[["Accuracy"]]

##RPART
model_rpart = rpart(formula = y_output ~ .,
                   data = Portdata_main_train, method = "class")

# plot
#prp(model_rpart, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")

p_hat_logit <- predict(model_rpart, newdata = Portdata_main_test, type = "class") 
y_hat_logit <- ifelse(p_hat_logit > 0.5,1, 0) %>% factor
confusionMatrix(p_hat_logit, Portdata_main_test$y_output)

fancyRpartPlot(model_rpart , digits=2 , palettes = c("Purples", "Oranges"))

##Knn

model_knn <- train(y_output ~ ., data = Portdata_main_train, method = "knn", 
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))

pred_kNN <- predict(model_knn , Portdata_main_test)
confusionMatrix(pred_kNN , Portdata_main_test$y_output)

### Cross table validation for KNN
CrossTable(Portdata_main_test$y_output, pred_kNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

##Randomforest

model_rf = randomForest(y_output ~ .,
                    data = Portdata_main_train)

pred_rf <- predict(model_rf, newdata = Portdata_main_test, type = "class") 

confusionMatrix(pred_rf, Portdata_main_test$y_output)

plot(model_rf, margin = 0.1)
text(model_rf, cex = 0.75)



library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]


models <- c("glm", "lda", "knn",  "qda", "rf")

# library(caret)
# library(dslabs)
# set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
# data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y_output ~ ., method = model, data = Portdata_main_train)
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
