if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(descr)) install.packages("descr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

## Data loading from the bank-full.csv which is present in the current directory

Portdata <- read_delim("bank-full.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
## ------ Data clean------------------  
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


# --- Converting the data into dataframe

Portdata <- as.data.frame(Portdata)

# --- Grouping some attribute into logical segregation
# --Age_group - Grouped age data into 4 categories.
Portdata <- Portdata %>% mutate(Age_group = ifelse(age < 21, "Below 20", ifelse(between(age, 21,40), "Between 21 and 40", ifelse(between(age,41,60), "Between 41 and 60", "Above 61"))))
# --Cust_group - Grouped balance data into 4 categories
Portdata <- Portdata %>% mutate(Cust_group = ifelse(balance <0, "below zero", ifelse(between(balance,1,mean(balance)),"below average", ifelse(between(balance,mean(balance),50000), "Above average", "HNI" ))))
# --day_group - Grouped day data into 4 categories
Portdata <- Portdata %>% mutate(day_group = ifelse(between(day,1,10),"Early Month", ifelse(between(day,11,20),"Mid Month", "Month End")))
# --Duration_group - Grouped duration data into 4 categories
Portdata <- Portdata %>% mutate(Duration_group = ifelse(between(duration,0,mean(duration)),"Less Duration", ifelse(between(duration,mean(duration),750),"Good Duration", ifelse(between(duration, 750, 1500),"High Duration", "Very High Duration" ))))
# --Campaign_group - Grouped campaign data into 4 categories
Portdata <- Portdata %>% mutate(Campaign_group = ifelse(between(campaign,1,mean(campaign)),"Lean Campaign", ifelse(between(duration,mean(campaign),10),"Average Campaign", ifelse(between(duration, 10, 20),"Ample Campaign", "Heavy Campaign" ))))
# --pdays_group - Grouped pdays data into 4 categories
Portdata <- Portdata %>% mutate(pdays_group = ifelse(between(pdays,-1,mean(pdays)), "Less gap Pdays", ifelse( between(pdays,mean(pdays), 100),"Medium gap Pdays", "Large gap Pdays")))
# --previous_group - Grouped previous data into 4 categories
Portdata <- Portdata %>% mutate(previous_group = ifelse(previous == 0, "Zero", ifelse(previous==1, "One", "More than once")))
# --y-output is "no", then 0 and if it is "yes", then 1.
Portdata <- Portdata %>% mutate(y_output = ifelse(y == "no", 0, 1))

Portdata <- Portdata %>% mutate(y_output = as.factor(y_output))
# -- Removing ungrouped columns
Portdata <- Portdata %>% select(-age,-balance,-day,-duration,-campaign,-pdays,-previous,-y)
# -- Converting grouped columns into factors  
Portdata <- Portdata %>% mutate(Age_group = as.factor(Age_group),
                                Cust_group = as.factor(Cust_group),
                                day_group = as.factor(day_group),
                                Duration_group = as.factor(Duration_group),
                                Campaign_group = as.factor(Campaign_group),
                                previous_group = as.factor(previous_group),
                                Duration_group = as.factor(Duration_group),
                                y_output = as.factor(y_output) )

#-- Converting character column into factor 
Portdata=Portdata %>% mutate_if(is.character, as.factor)

# The few rows of the Portdata are presented below:
head(Portdata) 

# Summary Statistics of Portdata
summary(Portdata)

##Portdata %>% group_by(poutcome,y) %>% summarise(n()) 
#%>% filter (duration > 700) 
# Age - categ 0.422, 0.3402 4:
# Month - Categ 0.51991, 0.467 2:
# Poutcome - Categ 0.647, 0.166 3:
# Duration group - Categ 0.607, 0.538 - 1:
# pdays_group categ - 0.4797, 0.188 - 5:

# Data Exploration

### Exploration of Age attribute
  
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

### Exploration of Job atrribute

job_count <- data.frame(Portdata %>% group_by(job,y_output) %>% summarise(Count = n()))

job_count <- job_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

job_count %>% knitr::kable()

### Exploration of Marital status

Marital_count <- data.frame(Portdata %>% group_by(marital,y_output) %>% summarise(Count = n()))

Marital_count <- Marital_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Marital_count %>% knitr::kable()

Portdata %>% ggplot(aes(marital, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by Marital status", x="Marital status", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

### Exploration of Education attribute

Education_count <- data.frame(Portdata %>% group_by(education,y_output) %>% summarise(Count = n()))

Education_count <- Education_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Education_count %>% knitr::kable()

### Exploration of Default attribute

Default_count <- data.frame(Portdata %>% group_by(default,y_output) %>% summarise(Count = n()))

Default_count <- Default_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Default_count %>% knitr::kable()

### Exploration of Housing attribute

Housing_count <- data.frame(Portdata %>% group_by(housing,y_output) %>% summarise(Count = n()))

Housing_count <- Housing_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Housing_count %>% knitr::kable()

### Exploration of Loan attribute

Loan_count <- data.frame(Portdata %>% group_by(loan,y_output) %>% summarise(Count = n()))

Loan_count <- Loan_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Loan_count %>% knitr::kable()

Portdata %>% ggplot(aes(loan, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by Loan count", x="Loan count", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

### Exploration of Contact attribute

Contact_count <- data.frame(Portdata %>% group_by(contact,y_output) %>% summarise(Count = n()))

Contact_count <- Contact_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Contact_count %>% knitr::kable()

Portdata %>% ggplot(aes(contact, y_output, color = contact)) + 
  geom_jitter()

### Exploration of Month attribute

Month_count <- data.frame(Portdata %>% group_by(month,y_output) %>% summarise(Count = n()))

Month_count <- Month_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Month_count %>% knitr::kable()


Portdata %>% ggplot(aes(month, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by contacted month", x="Contacted month", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()


### Exploration of Poutcome attribute

Pout_count <- data.frame(Portdata %>% group_by(poutcome,y_output) %>% summarise(Count = n()))

Pout_count <- Pout_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Pout_count %>% knitr::kable()


Portdata %>% ggplot(aes(poutcome, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by previous campaign outcome", x="Previous campaign outcome", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

### Exploration of Customer attribute

Cust_count <- data.frame(Portdata %>% group_by(Cust_group,y_output) %>% summarise(Count = n()))

Cust_count <- Cust_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Cust_count %>% knitr::kable()

Portdata %>% ggplot(aes(Cust_group, y_output, color = Cust_group)) + 
  geom_jitter()

### Exploration of day_group attributes

Day_count <- data.frame(Portdata %>% group_by(day_group,y_output) %>% summarise(Count = n()))

Day_count <- Day_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Day_count %>% knitr::kable()

Portdata %>% ggplot(aes(day_group, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by campaign period", x="Previous campaign period", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

### Exploration of Duration attributes

Duration_count <- data.frame(Portdata %>% group_by(Duration_group,y_output) %>% summarise(Count = n()))

Duration_count <- Duration_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Duration_count %>% knitr::kable()

Portdata %>% ggplot(aes(Duration_group, y_output, color = Duration_group)) + 
  geom_jitter()

### Exploration of Campaign_group attributes

Campaign_count <- data.frame(Portdata %>% group_by(Campaign_group,y_output) %>% summarise(Count = n()))

Campaign_count <- Campaign_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

Campaign_count %>% knitr::kable()

### Exploration of pdays_group attributes

pdays_count <- data.frame(Portdata %>% group_by(pdays_group,y_output) %>% summarise(Count = n()))

pdays_count <- pdays_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

pdays_count %>% knitr::kable()


Portdata %>% ggplot(aes(pdays_group, fill = y_output)) + 
  geom_bar() + 
  labs(title = "Term Deposits Yes/No by previous contact gap", x="Previous contact gap", y="Count of Output") +
  facet_wrap(y_output ~ .) +
  coord_flip()

### Exploration of previous_group attributes

previous_count <- data.frame(Portdata %>% group_by(previous_group,y_output) %>% summarise(Count = n()))

previous_count <- previous_count %>% spread(y_output,Count) %>% rename(No = `0`, Yes = `1` ) %>% 
  mutate(Percent_of_yes = Yes /(Yes + No)) %>% arrange(desc(Percent_of_yes))  

previous_count %>% knitr::kable()


Portdata %>% ggplot(aes(previous_group, y_output, color = previous_group)) + 
  geom_jitter()

###---- Splitting of datasets

# Validation set will be 10% of bank marketing data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
Valid_index <- createDataPartition(y = Portdata$y, times = 1, p = 0.1, list = FALSE)
Portdata_main <- Portdata[-Valid_index,]
Portdata_validation <- Portdata[Valid_index,]

# Validation set will be 20% of Portdata_main data
set.seed(2, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = Portdata_main$y, times = 1, p = 0.2, list = FALSE)
Portdata_main_train <- Portdata_main[-test_index,]
Portdata_main_test <- Portdata_main[test_index,]

## Data Analysis and modelling

##GLM:


glm_fit <- Portdata_main_train %>% 
  glm(y_output ~ ., data=., family = binomial(link='logit'))

p_hat_logit <- predict(glm_fit, newdata = Portdata_main_test, type = "response") 
y_hat_logit <- ifelse(p_hat_logit > 0.25,1, 0) %>% factor

### Cross table of GLM - All attributes
CrossTable(Portdata_main_test$y_output, y_hat_logit,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of GLM - All attributes
conf_glm <- confusionMatrix(y_hat_logit, 
                            Portdata_main_test$y_output, mode = "prec_recall", positive = '1')

conf_glm

accuracy_glm = conf_glm$overall[["Accuracy"]]
Sensitivity_glm = conf_glm$byClass[["Sensitivity"]]
Specificity_glm = conf_glm$byClass[["Specificity"]]
Precision_glm = conf_glm$byClass[["Precision"]]
F1_glm = conf_glm$byClass[["F1"]]

## creation of confusion table for GLM - All attributes
Confusion_table <- data_frame(method = "GLM - All attributes", 
                              Accuracy =accuracy_glm,SensitivityorRecall = Sensitivity_glm,
                              Specificity=Specificity_glm,Precision = Precision_glm, 
                              F1= F1_glm  )

Confusion_table <- as.data.frame(Confusion_table)

Confusion_table %>% knitr::kable()

# selected attributes

glm_fit_all <- Portdata_main_train %>% 
  glm(y_output ~ Duration_group+month+poutcome+Age_group+pdays_group,
      data=., family = binomial(link='logit'))
p_hat_logit_all <- predict(glm_fit_all, newdata = Portdata_main_test, type = "response") 
y_hat_logit_all <- ifelse(p_hat_logit_all > 0.25,1, 0) %>% factor

### Cross table of GLM - Selected attributes
CrossTable(Portdata_main_test$y_output, y_hat_logit_all,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of GLM - Selected attributes
conf_glm_all <- confusionMatrix(y_hat_logit_all, 
                                Portdata_main_test$y_output, mode = "prec_recall", positive = '1')

conf_glm_all

accuracy_glm_all = conf_glm_all$overall[["Accuracy"]]
Sensitivity_glm_all = conf_glm_all$byClass[["Sensitivity"]]
Specificity_glm_all = conf_glm_all$byClass[["Specificity"]]
Precision_glm_all = conf_glm_all$byClass[["Precision"]]
F1_glm_all = conf_glm_all$byClass[["F1"]]


## Binding confusion table for GLM - All attributes
Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "GLM - Selected attributes", 
                                        Accuracy =accuracy_glm_all,SensitivityorRecall = 
                                          Sensitivity_glm_all, Specificity=Specificity_glm_all,
                                        Precision = Precision_glm_all, F1= F1_glm_all))


Confusion_table %>% knitr::kable()

##RPART
# All attributes

model_rpart = rpart(formula = y_output ~ .,
                    data = Portdata_main_train, method = "class")
p_hat_logit <- predict(model_rpart, newdata = Portdata_main_test, type = "class") 

### Cross table of Rpart - All attributes
CrossTable(Portdata_main_test$y_output, p_hat_logit,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## RPLOT for the model
fancyRpartPlot(model_rpart ,digits=2, type=2,palettes = c("Purples", "Oranges"))

### Confusion matrix of Rpart - All attributes
conf_rpart <- confusionMatrix(p_hat_logit, 
                              Portdata_main_test$y_output, mode = "prec_recall", positive = '1')

conf_rpart

accuracy_rpart = conf_rpart$overall[["Accuracy"]]
Sensitivity_rpart = conf_rpart$byClass[["Sensitivity"]]
Specificity_rpart = conf_rpart$byClass[["Specificity"]]
Precision_rpart = conf_rpart$byClass[["Precision"]]
F1_rpart = conf_rpart$byClass[["F1"]]

## Binding confusion table for Rpart - All attributes
Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "Rpart - All attributes", 
                                        Accuracy =accuracy_rpart,SensitivityorRecall = 
                                          Sensitivity_rpart, Specificity=Specificity_rpart,
                                        Precision = Precision_rpart, F1= F1_rpart))

Confusion_table %>% knitr::kable()


# Selected attributes

model_rpart_select = rpart(formula = y_output ~ Duration_group+month+poutcome+Age_group+pdays_group,
                           data = Portdata_main_train, method = "class")

p_hat_logit_select <- predict(model_rpart_select, newdata = Portdata_main_test, type = "class")

### Cross table of Rpart - Selected attributes
CrossTable(Portdata_main_test$y_output, p_hat_logit_select,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### RPLOT for the model
fancyRpartPlot(model_rpart_select ,digits=2, type=2,palettes = c("Purples", "Oranges"))

### Confusion matrix of Rpart - Selected attributes
conf_rpart_select <- confusionMatrix(p_hat_logit_select, 
                                     Portdata_main_test$y_output,mode = "prec_recall", positive = '1')

conf_rpart_select

accuracy_rpart_select = conf_rpart_select$overall[["Accuracy"]]
Sensitivity_rpart_select = conf_rpart_select$byClass[["Sensitivity"]]
Specificity_rpart_select = conf_rpart_select$byClass[["Specificity"]]
Precision_rpart_select = conf_rpart_select$byClass[["Precision"]]
F1_rpart_select = conf_rpart_select$byClass[["F1"]]

## Binding confusion table for Rpart - Selected attributes
Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "Rpart - Selected attributes", 
                                        Accuracy =accuracy_rpart_select,SensitivityorRecall = 
                                          Sensitivity_rpart_select,Specificity=Specificity_rpart_select
                                        ,Precision = Precision_rpart_select, F1= F1_rpart_select))

Confusion_table %>% knitr::kable()

##Knn

# ALL Attributes

model_knn <- train(y_output ~ ., data = Portdata_main_train, method = "knn", 
                   maximize = TRUE,
                   trControl = trainControl(method = "cv", number = 10),
                   preProcess=c("center", "scale"))

pred_kNN <- predict(model_knn , Portdata_main_test)

### Cross table of KNN-All attributes
CrossTable(Portdata_main_test$y_output, pred_kNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of KNN-All attributes
conf_knn <- confusionMatrix(pred_kNN , Portdata_main_test$y_output, mode = "prec_recall", positive = '1')
conf_knn

accuracy_knn = conf_knn$overall[["Accuracy"]]
Sensitivity_knn = conf_knn$byClass[["Sensitivity"]]
Specificity_knn = conf_knn$byClass[["Specificity"]]
Precision_knn = conf_knn$byClass[["Precision"]]
F1_knn = conf_knn$byClass[["F1"]]

## Binding confusion table for KNN-All attributes
Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "KNN-All attributes", 
                                        Accuracy =accuracy_knn,SensitivityorRecall = Sensitivity_knn,
                                        Specificity=Specificity_knn,Precision = Precision_knn, 
                                        F1= F1_knn))

Confusion_table %>% knitr::kable()

##Randomforest


# All attributes 

model_rf = randomForest(y_output ~ .,
                        data = Portdata_main_train)

pred_rf <- predict(model_rf, newdata = Portdata_main_test, type = "class") 

### Cross table of RF - All attributes
CrossTable(Portdata_main_test$y_output, pred_rf,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of RF - All attributes
conf_rf <- confusionMatrix(pred_rf, Portdata_main_test$y_output, mode = "prec_recall", positive = '1')

conf_rf

accuracy_rf = conf_rf$overall[["Accuracy"]]
Sensitivity_rf = conf_rf$byClass[["Sensitivity"]]
Specificity_rf = conf_rf$byClass[["Specificity"]]
Precision_rf = conf_rf$byClass[["Precision"]]
F1_rf = conf_rf$byClass[["F1"]]

## Binding confusion table for RF - All attributes
Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "RF - All attributes", Accuracy =accuracy_rf,
                                        SensitivityorRecall = Sensitivity_rf, 
                                        Specificity=Specificity_rf,Precision = Precision_rf, F1= F1_rf))

Confusion_table %>% knitr::kable()

# Selected attributes

model_rf_select = randomForest(y_output ~ Duration_group+month+poutcome+Age_group+pdays_group,
                               data = Portdata_main_train)

pred_rf_select <- predict(model_rf_select, newdata = Portdata_main_test, type = "class") 

### Cross table of RF - Selected attributes
CrossTable(Portdata_main_test$y_output, pred_rf_select,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of RF - Selected attributes
conf_rf_select <- confusionMatrix(pred_rf_select, 
                                  Portdata_main_test$y_output, mode = "prec_recall", positive = '1')

conf_rf_select

accuracy_rf_select = conf_rf_select$overall[["Accuracy"]]
Sensitivity_rf_select = conf_rf_select$byClass[["Sensitivity"]]
Specificity_rf_select = conf_rf_select$byClass[["Specificity"]]
Precision_rf_select = conf_rf_select$byClass[["Precision"]]
F1_rf_select = conf_rf_select$byClass[["F1"]]

## Binding confusion table for RF - Selected attributes
Confusion_table <- bind_rows(Confusion_table,
                             data_frame(method = "RF - Selected attributes", 
                                        Accuracy =accuracy_rf_select,SensitivityorRecall = 
                                          Sensitivity_rf_select, Specificity=Specificity_rf_select,
                                        Precision = Precision_rf_select, F1= F1_rf_select))

Confusion_table %>% knitr::kable()

### Final model:

#GLM:

glm_fit_final <- Portdata_main %>% 
  glm(y_output ~ ., data=., family = binomial(link='logit'))
p_hat_logit_final <- predict(glm_fit_final, newdata = Portdata_validation, type = "response") 
y_hat_logit_final <- ifelse(p_hat_logit_final > 0.25,1, 0) %>% factor

### Cross table of GLM - Final - All
CrossTable(Portdata_validation$y_output, y_hat_logit_final,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of GLM - Final - All
conf_glm_final <- confusionMatrix(y_hat_logit_final, 
                                  Portdata_validation$y_output, mode = "prec_recall", positive = '1')

conf_glm_final

accuracy_glm_final = conf_glm_final$overall[["Accuracy"]]
Sensitivity_glm_final = conf_glm_final$byClass[["Sensitivity"]]
Specificity_glm_final = conf_glm_final$byClass[["Specificity"]]
Precision_glm_final = conf_glm_final$byClass[["Precision"]]
F1_glm_final = conf_glm_final$byClass[["F1"]]

## Creating confusion table final for GLM - Final - All
Confusion_table_final <- data_frame(method = "GLM - Final - All", 
                          Accuracy =accuracy_glm_final,
                          SensitivityorRecall = Sensitivity_glm_final, 
                          Specificity=Specificity_glm_final,
                          Precision = Precision_glm_final, F1= F1_glm_final  )

Confusion_table_final <- as.data.frame(Confusion_table_final)

Confusion_table_final %>% knitr::kable()

# Selected attributes

glm_fit_select_final <- Portdata_main %>% 
  glm(y_output ~ Duration_group+month+poutcome+Age_group+pdays_group,
      data=., family = binomial(link='logit'))
p_hat_logit_select_final <- predict(glm_fit_select_final, newdata =
                                      Portdata_validation, type = "response") 
y_hat_logit_select_final <- ifelse(p_hat_logit_select_final > 0.25,1, 0) %>% factor

### Cross table of GLM - Final - selected
CrossTable(Portdata_validation$y_output, y_hat_logit_select_final,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of GLM - Final - selected
conf_glm_select_final <- confusionMatrix(y_hat_logit_select_final, 
                                         Portdata_validation$y_output, mode = "prec_recall", positive = '1')

conf_glm_select_final

accuracy_glm_select_final = conf_glm_select_final$overall[["Accuracy"]]
Sensitivity_glm_select_final = conf_glm_select_final$byClass[["Sensitivity"]]
Specificity_glm_select_final = conf_glm_select_final$byClass[["Specificity"]]
Precision_glm_select_final = conf_glm_select_final$byClass[["Precision"]]
F1_glm_select_final = conf_glm_select_final$byClass[["F1"]]

## Binding confusion table final for GLM - Final - selected
Confusion_table_final <- bind_rows(Confusion_table_final,
                                   data_frame(method = "GLM - Final - selected", 
                                              Accuracy =accuracy_glm_select_final,SensitivityorRecall = 
                                                Sensitivity_glm_select_final, 
                                              Specificity=Specificity_glm_select_final,Precision =      
                                                Precision_glm_select_final, F1= F1_glm_select_final))


Confusion_table_final %>% knitr::kable()


##Randomforest

# All attributes 

model_rf_final = randomForest(y_output ~ .,
                              data = Portdata_main)

pred_rf_final <- predict(model_rf_final, newdata = Portdata_validation, type = "class") 

### Cross table of RF - Final - All
CrossTable(Portdata_validation$y_output, pred_rf_final,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of RF - Final - All
conf_rf_final <- confusionMatrix(pred_rf_final, 
                                 Portdata_validation$y_output, mode = "prec_recall", positive = '1')

conf_rf_final

accuracy_rf_final = conf_rf_final$overall[["Accuracy"]]
Sensitivity_rf_final = conf_rf_final$byClass[["Sensitivity"]]
Specificity_rf_final = conf_rf_final$byClass[["Specificity"]]
Precision_rf_final = conf_rf_final$byClass[["Precision"]]
F1_rf_final = conf_rf_final$byClass[["F1"]]

## Binding confusion table final for RF - Final - All
Confusion_table_final <- bind_rows(Confusion_table_final,
                                   data_frame(method = "RF - Final - All", 
                                              Accuracy =accuracy_rf_final,SensitivityorRecall = 
                                                Sensitivity_rf_final,Specificity=Specificity_rf_final,
                                              Precision = Precision_rf_final, F1= F1_rf_final))

Confusion_table_final %>% knitr::kable()


# Selected attributes

model_rf_select_final = randomForest(y_output ~ Duration_group+month+poutcome+Age_group+pdays_group,
                                     data = Portdata_main)

pred_rf_select_final <- predict(model_rf_select_final, newdata = Portdata_validation, type = "class") 

### Cross table of RF - Final - Selected
CrossTable(Portdata_validation$y_output, pred_rf_select_final,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Confusion matrix of RF - Final - Selected
conf_rf_select_final <- confusionMatrix(pred_rf_select_final, 
                                        Portdata_validation$y_output, mode = "prec_recall", positive = '1')

conf_rf_select_final

accuracy_rf_select_final = conf_rf_select_final$overall[["Accuracy"]]
Sensitivity_rf_select_final = conf_rf_select_final$byClass[["Sensitivity"]]
Specificity_rf_select_final = conf_rf_select_final$byClass[["Specificity"]]
Precision_rf_select_final = conf_rf_select_final$byClass[["Precision"]]
F1_rf_select_final = conf_rf_select_final$byClass[["F1"]]

## Binding confusion table final for RF - Final - Selected
Confusion_table_final <- bind_rows(Confusion_table_final,
                                   data_frame(method = "RF - Final - Selected", 
                                              Accuracy =accuracy_rf_select_final,
                                              SensitivityorRecall = Sensitivity_rf_select_final, 
                                              Specificity=Specificity_rf_select_final,Precision = 
                                                Precision_rf_select_final, F1= F1_rf_select_final))

Confusion_table_final %>% knitr::kable()
