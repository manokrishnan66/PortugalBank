if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(tidyverse)
library(lubridate)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
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

Portdata <- Portdata %>% mutate(Age_group = ifelse(age < 21, "Below 20", ifelse(between(age, 21,40), "Between 21 and 40", ifelse(between(age,41,60), "Between 41 and 60", "Above 61"))))

Portdata <- Portdata %>% mutate(Cust_group = ifelse(balance <0, "below zero", ifelse(between(balance,1,mean(balance)),"below average", ifelse(between(balance,mean(balance),50000), "Above average", "HNI" ))))

Portdata <- Portdata %>% mutate(day_group = ifelse(between(day,1,10),"Early Month", ifelse(between(day,11,20),"Mid Month", "Month End")))

Portdata <- Portdata %>% mutate(Duration_group = ifelse(between(duration,0,mean(duration)),"Less Duration", ifelse(between(duration,mean(duration),750),"Good Duration", ifelse(between(duration, 750, 1500),"High Duration", "Very High Duration" ))))


Portdata <- Portdata %>% mutate(Campaign_group = ifelse(between(campaign,1,mean(campaign)),"Lean Campaign", ifelse(between(duration,mean(campaign),10),"Average Campaign", ifelse(between(duration, 10, 20),"Ample Campaign", "Heavy Campaign" ))))

Portdata <- Portdata %>% mutate(pdays_group = ifelse(between(pdays,-1,mean(pdays)), "Less gap Pdays", ifelse( between(pdays,mean(pdays), 100),"Medium gap Pdays", "Large gap Pdays")))

Portdata <- Portdata %>% mutate(previous_group = ifelse(previous == 0, "Zero", ifelse(previous==1, "One", "More than once")))

Portdata <- Portdata %>% mutate(y_output = ifelse(y == "no", 0, 1))

Portdata <- Portdata %>% mutate(y_output = as.factor(y_output))

Portdata <- Portdata %>% select(-age,-balance,-day,-duration,-campaign,-pdays,-previous,-y)

Portdata <- Portdata %>% mutate(Age_group = as.factor(Age_group),
                                Cust_group = as.factor(Cust_group),
                                day_group = as.factor(day_group),
                                Duration_group = as.factor(Duration_group),
                                Campaign_group = as.factor(Campaign_group),
                                previous_group = as.factor(previous_group),
                                Duration_group = as.factor(Duration_group),
                                y_output = as.factor(y_output) )


Portdata=Portdata %>% mutate_if(is.character, as.factor)


levels(Portdata$day_group)
#Summary on dataset
summary(Portdata)

str(Portdata)

head(Portdata)

mean(Portdata$previous)

Portdata %>% group_by(poutcome,y) %>% summarise(n()) 
#%>% filter (duration > 700) 
# Age - categ 0.422, 0.3402 4:
# Month - Categ 0.51991, 0.467 2:
# Poutcome - Categ 0.647, 0.166 3:
# Duration group - Categ 0.607, 0.538 - 1:
# pdays_group categ - 0.4797, 0.188 - 5:

# Data Exploration

### Age - categ
  
Age_count <- data.frame(Portdata %>% group_by(Age_group, y_output) %>% summarise(Count = n()))

Age_count <- Age_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Age_count %>% knitr::kable()

Portdata %>% ggplot(aes(Age_group, color = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by Age Count", x="age", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

theme_set(theme_bw())  # pre-set the bw theme.
g <- Portdata %>% ggplot( aes(y_output, Age_group)) + 
  labs(subtitle="Term deposit output vs Age Group",
       Y="Age Group", x="Count of Output")

g + geom_jitter(aes(col=Age_group)) + 
  geom_smooth(aes(col=Age_group), method="lm", se=F)

### Job

job_count <- data.frame(Portdata %>% group_by(job,y_output) %>% summarise(Count = n()))

job_count <- job_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

job_count %>% knitr::kable()

## Marital status

Marital_count <- data.frame(Portdata %>% group_by(marital,y_output) %>% summarise(Count = n()))

Marital_count <- Marital_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Marital_count %>% knitr::kable()

Portdata %>% ggplot(aes(marital, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by Marital status", x="Marital status", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

## Education

Education_count <- data.frame(Portdata %>% group_by(education,y_output) %>% summarise(Count = n()))

Education_count <- Education_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Education_count %>% knitr::kable()

## Default

Default_count <- data.frame(Portdata %>% group_by(default,y_output) %>% summarise(Count = n()))

Default_count <- Default_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Default_count %>% knitr::kable()

## Housing

Housing_count <- data.frame(Portdata %>% group_by(housing,y_output) %>% summarise(Count = n()))

Housing_count <- Housing_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Housing_count %>% knitr::kable()

## Loan

Loan_count <- data.frame(Portdata %>% group_by(loan,y_output) %>% summarise(Count = n()))

Loan_count <- Loan_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Loan_count %>% knitr::kable()

Portdata %>% ggplot(aes(loan, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by Loan count", x="Loan count", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

## Contact

Contact_count <- data.frame(Portdata %>% group_by(contact,y_output) %>% summarise(Count = n()))

Contact_count <- Contact_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Contact_count %>% knitr::kable()

Portdata %>% ggplot(aes(contact, y_output, color = contact)) + 
  geom_jitter()

## Month - Categ

Month_count <- data.frame(Portdata %>% group_by(month,y_output) %>% summarise(Count = n()))

Month_count <- Month_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Month_count %>% knitr::kable()


Portdata %>% ggplot(aes(month, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by contacted month", x="Contacted month", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()


# Poutcome - Categ

Pout_count <- data.frame(Portdata %>% group_by(poutcome,y_output) %>% summarise(Count = n()))

Pout_count <- Pout_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Pout_count %>% knitr::kable()


Portdata %>% ggplot(aes(poutcome, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by previous campaign outcome", x="Previous campaign outcome", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

## Customer Group

Cust_count <- data.frame(Portdata %>% group_by(Cust_group,y_output) %>% summarise(Count = n()))

Cust_count <- Cust_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Cust_count %>% knitr::kable()

Portdata %>% ggplot(aes(Cust_group, y_output, color = Cust_group)) + 
  geom_jitter()

## day_group

Day_count <- data.frame(Portdata %>% group_by(day_group,y_output) %>% summarise(Count = n()))

Day_count <- Day_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Day_count %>% knitr::kable()

Portdata %>% ggplot(aes(day_group, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by campaign period", x="Previous campaign period", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

## Duration group - Categ

Duration_count <- data.frame(Portdata %>% group_by(Duration_group,y_output) %>% summarise(Count = n()))

Duration_count <- Duration_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Duration_count %>% knitr::kable()

Portdata %>% ggplot(aes(Duration_group, y_output, color = Duration_group)) + 
  geom_jitter()

## Campaign_group

Campaign_count <- data.frame(Portdata %>% group_by(Campaign_group,y_output) %>% summarise(Count = n()))

Campaign_count <- Campaign_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Campaign_count %>% knitr::kable()

## pdays_group categ

pdays_count <- data.frame(Portdata %>% group_by(pdays_group,y_output) %>% summarise(Count = n()))

pdays_count <- pdays_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

pdays_count %>% knitr::kable()


Portdata %>% ggplot(aes(pdays_group, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by previous contact gap", x="Previous contact gap", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

## previous_group


previous_count <- data.frame(Portdata %>% group_by(previous_group,y_output) %>% summarise(Count = n()))

previous_count <- previous_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

previous_count %>% knitr::kable()


Portdata %>% ggplot(aes(previous_group, y_output, color = previous_group)) + 
  geom_jitter()


# Validation set will be 10% of bank marketing data
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

# Age - categ 0.422, 0.3402 4:
# Month - Categ 0.51991, 0.467 2:
# Poutcome - Categ 0.647, 0.166 3:
# Duration group - Categ 0.607, 0.538 - 1:
# pdays_group categ - 0.4797, 0.188 - 5:


##GLM:

glm_fit <- Portdata_main_train %>% 
    glm(y_output ~ ., data=., family = binomial(link='logit'))
p_hat_logit <- predict(glm_fit, newdata = Portdata_main_test, type = "response") 
y_hat_logit <- ifelse(p_hat_logit > 0.25,1, 0) %>% factor
conf_glm <- confusionMatrix(y_hat_logit, Portdata_main_test$y_output)
#$overall[["Accuracy"]]
accuracy_glm = conf_glm$overall[["Accuracy"]]
Sensitivity_glm = conf_glm$byClass[["Sensitivity"]]
Specificity_glm = conf_glm$byClass[["Specificity"]]
Precision_glm = conf_glm$byClass[["Precision"]]
F1_glm = conf_glm$byClass[["F1"]]

### Cross table validation for KNN
CrossTable(Portdata_main_test$y_output, y_hat_logit,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

Confusion_table <- data_frame(method = "GLM", Accuracy =accuracy_glm,SensitivityorRecall = Sensitivity_glm, Specificity=Specificity_glm,Precision = Precision_glm, F1= F1_glm  )

Confusion_table <- as.data.frame(Confusion_table)

Confusion_table %>% knitr::kable()


##RPART
model_rpart = rpart(formula = y_output ~ .,
                   data = Portdata_main_train, method = "class")

# plot
#prp(model_rpart, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")

p_hat_logit <- predict(model_rpart, newdata = Portdata_main_test, type = "class") 
#y_hat_logit <- ifelse(p_hat_logit > 0.5,1, 0) %>% factor
conf_rpart <- confusionMatrix(p_hat_logit, Portdata_main_test$y_output)

accuracy_rpart = conf_rpart$overall[["Accuracy"]]
Sensitivity_rpart = conf_rpart$byClass[["Sensitivity"]]
Specificity_rpart = conf_rpart$byClass[["Specificity"]]
Precision_rpart = conf_rpart$byClass[["Precision"]]
F1_rpart = conf_rpart$byClass[["F1"]]

Confusion_table <- bind_rows(Confusion_table,
                          data_frame(method = "Rpart", Accuracy =accuracy_rpart,SensitivityorRecall = Sensitivity_rpart, Specificity=Specificity_rpart,Precision = Precision_rpart, F1= F1_rpart))
# save rmse results in a table
Confusion_table %>% knitr::kable()


fancyRpartPlot(model_rpart ,digits=2, type=2,palettes = c("Purples", "Oranges"))

rpart.plot(model_rpart)

##Knn

model_knn <- train(y_output ~ ., data = Portdata_main_train, method = "knn", 
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))

pred_kNN <- predict(model_knn , Portdata_main_test)
conf_knn <- confusionMatrix(pred_kNN , Portdata_main_test$y_output)


accuracy_knn = conf_knn$overall[["Accuracy"]]
Sensitivity_knn = conf_knn$byClass[["Sensitivity"]]
Specificity_knn = conf_knn$byClass[["Specificity"]]
Precision_knn = conf_knn$byClass[["Precision"]]
F1_knn = conf_knn$byClass[["F1"]]


Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "KNN", Accuracy =accuracy_knn,SensitivityorRecall = Sensitivity_knn, Specificity=Specificity_knn,Precision = Precision_knn, F1= F1_knn))
# save rmse results in a table
Confusion_table %>% knitr::kable()


### Cross table validation for KNN
CrossTable(Portdata_main_test$y_output, pred_kNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Age - categ 0.422, 0.3402 4:
# Month - Categ 0.51991, 0.467 2:
# Poutcome - Categ 0.647, 0.166 3:
# Duration group - Categ 0.607, 0.538 - 1:
# pdays_group categ - 0.4797, 0.188 - 5:

str(Portdata_main_train$Age_group)

#Portdata_main_test=Portdata_main_test %>% mutate_if(is.character, as.factor)

##Randomforest

model_rf = randomForest(y_output ~ .,
                    data = Portdata_main_train)

pred_rf <- predict(model_rf, newdata = Portdata_main_test, type = "class") 

conf_rf <- confusionMatrix(pred_rf, Portdata_main_test$y_output)

plot(model_rf, margin = 0.1)
text(model_rf, cex = 0.75)
str(Portdata_main_train)


accuracy_rf = conf_rf$overall[["Accuracy"]]
Sensitivity_rf = conf_rf$byClass[["Sensitivity"]]
Specificity_rf = conf_rf$byClass[["Specificity"]]
Precision_rf = conf_rf$byClass[["Precision"]]
F1_rf = conf_rf$byClass[["F1"]]


Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "RF", Accuracy =accuracy_rf,SensitivityorRecall = Sensitivity_rf, Specificity=Specificity_rf,Precision = Precision_rf, F1= F1_rf))
# save confusion results in a table
Confusion_table %>% knitr::kable()


### Cross table validation for rf
CrossTable(Portdata_main_test$y_output, pred_rf,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


# ctree

install.packages("partykit")
library("partykit")
better_survival_ctree <- ctree(y_output ~ ., data=Portdata_main_train)
plot(better_survival_ctree)
pred_ctree <- predict(better_survival_ctree, Portdata_main_test, type = "response") 

conf_ctree <- confusionMatrix(pred_ctree, Portdata_main_test$y_output)


accuracy_ctree = conf_ctree$overall[["Accuracy"]]
Sensitivity_ctree = conf_ctree$byClass[["Sensitivity"]]
Specificity_ctree = conf_ctree$byClass[["Specificity"]]
Precision_ctree = conf_ctree$byClass[["Precision"]]
F1_ctree = conf_ctree$byClass[["F1"]]


Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "Ctree", Accuracy =accuracy_ctree,SensitivityorRecall = Sensitivity_ctree, Specificity=Specificity_ctree,Precision = Precision_ctree, F1= F1_ctree))
# save confusion results in a table
Confusion_table %>% knitr::kable()


### Cross table validation for ctree
CrossTable(Portdata_main_test$y_output, pred_ctree,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

