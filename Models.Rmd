```{r, global_options, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
rm(list = ls())
suppressMessages(library(car))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(corrplot))
# suppressMessages(library(olsrr))
suppressMessages(library(MASS))
# suppressMessages(library(pls))
# suppressMessages(library(lars))
suppressMessages(library(psych))
suppressMessages(library(randomForest))
suppressMessages(library(caret))
suppressMessages(library(pROC))


apps <- read.table(file = "AppsR.csv", sep = ",", header = TRUE,  stringsAsFactors = TRUE)
#summary(apps)
apps <- apps %>% dplyr::select(-Term_Group, -app_id, -first_source, -college, )
sapply(apps, function(x) sum(is.na(x)))
str(apps)

#Convert binary predictors and response columns to categorical vars
apps <- apps %>% mutate_at(c('app_complete', 'admitted', 'deposit_count', 'enrolled','honors_applied','us_hs_flag','postsec_flag','kaplan_flag', 'common_app', 'defer_flag','cold_lead', 'app_month', 'app_comp_month', 'admit_month', 'first_gen_flg','intl_sponsored_flg', 'intl_financial_guarantee_flg', 'first_source_month','above_18'), as.factor)

apps <- apps %>% mutate_at(c('app_appcomp_diff','appcomp_date_diff','namu_amount','app_appcomp_diff', 'app_date_diff'), as.numeric)
str(apps)
apps <- as.data.frame(apps)

# Creating training and test data split
set.seed(93285)
### Split the data into training and testing samples
n = dim(apps)[1]; ### total number of observations
n1 = round(n*0.3); ### number of observations randomly selected for testing data

flag <- sort(sample(1:n, n1));
apps.test <- apps[flag,];
apps.train <- apps[-flag,];

#Correlation between numeric predictors

corrz <- cor(apps[sapply(apps,is.numeric)])

library(reshape)
melted <- melt(corrz)
head(melted)

ggplot(melted, aes(x = X1, y = X2, fill = value)) + 
  labs(title = "Correlation heatmap for first 7 variables of the dataset") + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1), 
                      space = "Lab", name = "Correlation coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 8, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) + 
  geom_text(aes(x = X1, y = X2, label = round(value, 3)), color = "black", size = 3) + 
  coord_fixed()

draw_confusion_matrix <- function(cm) {

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)

  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Not Enrolled', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Enrolled', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Not Enrolled', cex=1.2, srt=90)
  text(140, 335, 'Enrolled', cex=1.2, srt=90)

  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

confusion.glm <- function(data, model) {
  prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  return(confusion)
}

```

```{r Graphs, echo=TRUE, warning=FALSE, cache=TRUE}
# psych::multi.hist(apps[,sapply(apps, is.numeric)])

library(reshape2)
library(ggplot2)
d <- melt(apps[,-5])
ggplot(d,aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x", nrow=2) + 
    geom_histogram()

# ggplot(data = apps) +
#   geom_histogram(mapping = aes(x = home_country_freq), binwidth = 30)

require(gridExtra)
p1 <- ggplot(data = apps) +
  geom_boxplot(mapping = aes(x = enrolled , y = ugrd_intl_gpa)) +
  labs(y = "GPA", x= "Enrolled")

p2 <- ggplot(data = apps) +
  geom_boxplot(mapping = aes(x = enrolled , y = namu_amount)) +

    labs(y = "NAmU Amount", x= "Enrolled")

p3 <- ggplot(data = apps) +
  geom_boxplot(mapping = aes(x = enrolled , y = app_date_diff)) +
  labs(y = "App Date Difference", x= "Enrolled")

p4 <- ggplot(data = apps) +
  geom_boxplot(mapping = aes(x = enrolled , y = appcomp_date_diff)) +
  labs(y = "App Complete Date Difference", x= "Enrolled")

grid.arrange(p1, p2, p3, p4, ncol=2)


ggplot(data = apps) +
  geom_boxplot(mapping = aes(x = enrolled , y = home_city_freq)) +
  labs(y = "GPA", x= "Enrolled")


```

```{r Part 1 Baseline, echo=TRUE, warning=FALSE, cache=TRUE}
#E: A single Tree
library(rpart)
library(rpart.plot)
modE0 <- rpart(enrolled ~ .,data=apps.train, method="class", 
                     parms=list(split="gini"))
opt <- which.min(modE0$cptable[, "xerror"]); 
cp1 <- modE0$cptable[opt, "CP"];
modE <- prune(modE0,cp=cp1);
y2hatE <-  predict(modE, apps.test[,-5],type="class")
mean(y2hatE != apps.test$enrolled)
rpart.plot(modE, extra=1)
summary(modE)

confusionMatrix(data = as.factor(y2hatE), reference = as.factor(apps.test$enrolled), positive='1')
draw_confusion_matrix(confusionMatrix(data = as.factor(y2hatE), reference = as.factor(apps.test$enrolled), positive='1'))


# NAIVE BAYES

library(e1071)
mod3 <- naiveBayes( apps.train[,-5], apps.train[,5])

## Testing Error 
mean( predict(mod3,apps.test[,-5]) != apps.test$enrolled)

y2hat <-  predict(mod3,apps.test[,-5], type="class")
confusionMatrix(data = as.factor(y2hat), reference = as.factor(apps.test$enrolled), positive='1')
draw_confusion_matrix(confusionMatrix(data = as.factor(y2hat), reference = as.factor(apps.test$enrolled), positive='1'))

```

```{r Part 2 RF, echo=TRUE, warning=FALSE, cache=TRUE}

# apps <- apps %>% dplyr::select(-c(deposit_count))
# flag <- sort(sample(1:n, n1));
# apps.test <- apps[flag,];
# apps.train <- apps[-flag,];

### (6) RF
rf <- randomForest(enrolled~., data=apps.train, ntree=500)#, proximity=TRUE) 
print(rf)

# Error rate for training data
p1 <- predict(rf, apps.train)
confusionMatrix(p1, apps.train$enrolled, positive='1')

error1 <- mean( predict(rf, newdata = apps.train[-4]) != apps.train$enrolled)

# Error rate for testing data
p2 <- predict(rf, apps.test)
confusionMatrix(p2, apps.test$enrolled, positive='1')

draw_confusion_matrix(confusionMatrix(p2, apps.test$enrolled, positive='1'))

error2 <- mean( predict(rf, newdata = apps.test[-4]) != apps.test$enrolled)

plot(rf)

# Parameter Tuning
# The number of variables selected at each split is denoted by mtry in randomforest function.
t <- tuneRF(apps.train[,-5], apps.train[,5],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)

best.m <- t[t[, 2] == min(t[, 2]), 1]
print(t)
print(best.m)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

#MeanDecreaseGini
importance(rf)

partialPlot(rf, apps.train, deposit_count, "1")
# The inference should be, if the HPI_change_orig_x is negative then higher chances of classifying into default=1 class.



```

```{r Part 3 Logistic, echo=TRUE, warning=FALSE, cache=TRUE}
# LOGISTIC

 apps <- apps %>% dplyr::select(-c(deposit_count))
 flag <- sort(sample(1:n, n1));
 apps.test <- apps[flag,];
 apps.train <- apps[-flag,];

mod4 <- glm(enrolled~., family=binomial(link=logit), data=apps.train)
summary(mod4)

pred4test <- ifelse(predict(mod4, newdata=apps.test[,-5], type="response") >= 0.5, 1, 0)
## Testing Error of (multinomial) logisitic regression
mean( pred4test != apps.test$enrolled)


# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(pred4test), reference = as.factor(apps.test$enrolled), positive='1')
draw_confusion_matrix(confusionMatrix(data = as.factor(pred4test), reference = as.factor(apps.test$enrolled), positive='1'))

# options(scipen=999)
summary(mod4)$coef[summary(mod4)$coef[,4] <= .01, 4]


# STEPWISE LOGISTIC

suppressMessages(library(psych))
apps_glm <- glm(enrolled~., family=binomial(link=logit), data=apps.train)
mod5 <- stepAIC(apps_glm, trace=T);
summary(mod5)

summary(mod5)$coef[summary(mod5)$coef[,4] <= .01, 4]
# 
pred5test <- ifelse(predict(mod5, newdata=apps.test[,-5], type="response") >= 0.5, 1, 0)
## Testing Error of (multinomial) logisitic regression
mean( pred5test != apps.test$enrolled)

# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(pred5test), reference = as.factor(apps.test$enrolled), positive='1')
draw_confusion_matrix(confusionMatrix(data = as.factor(pred5test), reference = as.factor(apps.test$enrolled), positive='1'))



```

```{r Part 4 Boosting, echo=TRUE, warning=FALSE, cache=TRUE}
library(gbm)

gbm.apps <- gbm((as.integer(enrolled) - 1) ~ ., data=apps.train,
distribution = 'bernoulli',
n.trees = 2000,
shrinkage = 0.05,
interaction.depth = 3,
cv.folds = 10)

## Model Inspection
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.apps, method="cv")
perf_gbm1

## Which variables are important?
summary(gbm.apps)

# ## Training error
# pred1gbm <- predict(gbm.mort, newdata = mort.train[,-18], n.trees=perf_gbm1,
# type="response")
# y1hat <- ifelse(pred1gbm < 0.5, 0, 1)
# mean(y1hat != mort.train$default) 
## Testing error
y2hat <- ifelse(predict(gbm.apps, newdata = apps.test[,-5],
n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
mean(y2hat != apps.test$enrolled)

suppressMessages(library(caret))
confusionMatrix(data = as.factor(y2hat), reference = as.factor(apps.test$enrolled), positive='1')
draw_confusion_matrix(confusionMatrix(data = as.factor(y2hat), reference = as.factor(apps.test$enrolled), positive='1'))

best <- which.min(gbm.apps$cv.error)

#ROC 
library(pROC)
# create roc curve

par = (pty = 's')
roc(apps.test$enrolled, pred5test, plot=TRUE)
# calculate area under curve
auc(roc_object)

```

```{r Monte Carlo CV, echo=TRUE, warning=FALSE, cache=TRUE}
B= 100; ### number of loops
TEALL = NULL; ### Final Confusion Matrices
set.seed(7406)

for (b in 1:B){

tempflag <- sort(sample(1:n, n1));
app.train <- apps[-tempflag,]; ## temp training set for CV
app.test <- apps[tempflag,];

### A single Tree ###

library(rpart)
library(rpart.plot)
modE0 <- rpart(enrolled ~ .,data=app.train, method="class", 
                     parms=list(split="gini"))
opt <- which.min(modE0$cptable[, "xerror"]); 
cp1 <- modE0$cptable[opt, "CP"];
modE <- prune(modE0,cp=cp1);
y2hatE <-  predict(modE, app.test[,-5],type="class")
# Pulling out the F1 score from confusion matrix 
cm <- confusionMatrix(data = as.factor(y2hatE), reference = as.factor(app.test$enrolled), positive='1')
f11 <- cm$byClass[[7]]


### NAIVE BAYES ###

library(e1071)
mod3 <- naiveBayes( app.train[,-5], app.train[,5])
y2hat <-  predict(mod3,app.test[,-5], type="class")
# Pulling out the F1 score from confusion matrix 
cm <- confusionMatrix(data = as.factor(y2hat), reference = as.factor(app.test$enrolled), positive='1')
f12 <- cm$byClass[[7]]


### Logistic Regression ###

mod4 <- glm(enrolled~., family=binomial(link=logit), data=app.train)
pred4test <- ifelse(predict(mod4, newdata=app.test[,-5], type="response") >= 0.5, 1, 0)
# Pulling out the F1 score from confusion matrix 
cm <- confusionMatrix(data = as.factor(pred4test), reference = as.factor(app.test$enrolled), positive='1')
f13 <- cm$byClass[[7]]


### RF ###

rf <- randomForest(enrolled~., data=app.train, ntree=500, mtry=10)#, proximity=TRUE) 
# Error rate for testing data
p2 <- predict(rf, app.test)
# Pulling out the F1 score from confusion matrix 
cm <- confusionMatrix(p2, app.test$enrolled, positive='1')
f14 <- cm$byClass[[7]]

### Boosting ###

gbm.apps <- gbm((as.integer(enrolled) - 1) ~ ., data=app.train,
distribution = 'bernoulli',
n.trees = 5000,
shrinkage = 0.05,
interaction.depth = 3,
cv.folds = 10)
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.apps, method="cv")
## Testing error
y2hat <- ifelse(predict(gbm.apps, newdata = app.test[,-5],
n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
cm <- confusionMatrix(data = as.factor(y2hat), reference = as.factor(app.test$enrolled), positive='1')
f15 <- cm$byClass[[7]]


TEALL = rbind( TEALL, cbind(f11, f12, f13, f14, f15) );

}


dim(TEALL); ### This should be a Bx9 matrices
### if you want, you can change the column name of TEALL
colnames(TEALL) <- c("Single Tree", "Naive Bayes", "Logistic", "RF", "GBM");
## You can report the sample mean/variances of the testing errors so as to compare these models
apply(TEALL, 2, mean)
apply(TEALL, 2, var)

```
