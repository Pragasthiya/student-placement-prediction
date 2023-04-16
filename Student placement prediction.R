setwd("C:/Users/welcome/Downloads")
a=read.csv("placement.csv")

head(a)
tail(a)
summary(a)
str(a)


library(dplyr)
library(ggplot2)
library(caret)


#converting to factor
a$gender =as.factor(a$gender)
a$ssc_b = as.factor(a$ssc_b)
a$hsc_b = as.factor(a$hsc_b)
a$hsc_s = as.factor(a$hsc_s)
a$degree_t = as.factor(a$degree_t)
a$workex = as.factor(a$workex)
a$specialisation = as.factor(a$specialisation)
a$status = as.factor(a$status)

#missing values

is.na(a)
sum(is.na(a))
sum(is.na(a$salary))

#Treating the missing value with median
a$salary <- ifelse(is.na(a$salary), median(a$salary, na.rm = TRUE),a$salary)
sum(is.na(a$salary))

#finding outlier
boxplot(a$ssc_p)
boxplot(a$hsc_p)
boxplot(a$degree_p)
boxplot(a$etest_p)
boxplot(a$mba_p)
boxplot(a$salary)


#treating outliers

a$hsc_p[a$hsc_p > 90] <- 90
a$hsc_p[a$hsc_p < 45] <- 45
boxplot(a$hsc_p)

a$degree_p[a$degree_p > 85] <- 85
boxplot(a$degree_p)

a$salary[a$salary >3e+05] <- 3e+05
a$salary[a$salary < 150000] <- 150000
boxplot(a$salary)

#dplyr

a %>% group_by(gender) %>%
  summarise(n = n())

a %>% select(gender,specialisation,status) %>% 
  group_by(specialisation) %>% 
  summarise(sum(gender == 'M' & status == 'Placed')/sum(status=='Placed'))
  
a %>% select(gender,specialisation,status) %>% 
  group_by(specialisation) %>% 
  summarise(sum(gender == 'F' & status == 'Placed')/sum(status=='Placed'))

a %>% select(status,gender) %>%
  summarise(sum(gender=='M'  & status == 'Placed')/sum(status=='Placed'))

a %>% select(status,gender) %>%
  summarise(sum(gender=='F'  & status == 'Placed')/sum(status=='Placed'))


#ggplot

ggplot(a,aes(gender,mba_p))+geom_bar(stat = "identity")
ggplot(a,aes(specialisation,salary))+geom_bar(stat = "identity")+facet_grid(~gender)
ggplot(a,aes(workex,salary))+geom_bar(stat = "identity")+facet_grid(~gender)

#Data Split
spl = createDataPartition(a$status,p = 0.75,list = FALSE)
spl
atrain = a[spl,]
atest = a[-spl,]

print(dim(atrain));print(dim(atest))

#Model Building

set.seed(123)
model_glm <- train(status~., 
                   data = atrain, 
                   method = "glm",
                   family = "binomial")
summary(model_glm)
prediction = predict(model_glm,atest)
summary(prediction)
conf_mat <- confusionMatrix(prediction, atest$status)
f1_score <- conf_mat$byClass["F1"]
print(f1_score)


model <- train(status ~ ., 
               data = atrain, method = "rpart")
summary(model)
prediction = predict(model,atest)
summary(prediction)
conf_mat <- confusionMatrix(prediction, atest$status)
f1_score <- conf_mat$byClass["F1"]
print(f1_score)


modelrf <- train(status ~ ., data = atrain, method = "rf")
summary(modelrf)
prediction = predict(modelrf,atest)
summary(prediction)
conf_mat <- confusionMatrix(prediction, atest$status)
f1_score <- conf_mat$byClass["F1"]
print(f1_score)

modelxgboost <- train(status ~ ., data = atrain, method = "xgbTree")
summary(modelxgboost)
prediction = predict(modelxgboost,atest)
summary(prediction)
conf_mat <- confusionMatrix(prediction, atest$status)
f1_score <- conf_mat$byClass["F1"]
print(f1_score)

model <- train(status ~ ., 
               data = atrain, method = "nb")
summary(model)
prediction = predict(model,atest)
summary(prediction)
conf_mat <- confusionMatrix(prediction, atest$status)
f1_score <- conf_mat$byClass["F1"]
print(f1_score)



