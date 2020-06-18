#######################################################################################
##### TELECOM CHURN PREDICTION REPORT 
#######################################################################################


######################### 
### Introduction
#########################

# The goal of the project is to predict churn of a telecom company and compare some 
# advanced machine learning algorithms by using one of telecom dataset. The telecom 
# dataset was downloaded from www.kaggle.com. It has over 7,000 records and 21 variables. 
# The following models will be explored: Decision tree, Random forest, and Support Vector Machine.


### Installing Packages ###

#installing required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

# opening the libraries
library(tidyverse)
library(dplyr)
library(plyr)
library(DataExplorer)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(scales)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(pROC)
library(e1071)


### Downloading the file ###

# Use the Github link to download data set
churn_set <- read.csv("https://raw.githubusercontent.com/aplatonow/Churn-project/master/Telco-Customer-Churn.csv")


### Dataset and variables  ###

# explore column names
colnames(churn_set)





#########################
### Analysis 
#########################

# In this section, the following steps will be covered:
# 1) Data Cleaning;
# 2) Data Exploration and Visualization;
# 3) Data Wrangling and Structuring;
# 4) Data Modeling.


########################
## 1) Data Cleaning 


# show variables and their type
glimpse(churn_set)

# checking data summary 
summary(churn_set)

# checking for NA values 
apply(is.na(churn_set), 2, sum)

# Identifying how many percentage of NA in data set
plot_missing(churn_set)

# Identifying customer's tenure with NA in Total Charges.
churn_set %>% filter(is.na(TotalCharges)) %>% summarize(customerID, TotalCharges, tenure)

# Changing NA values to zero
churn_set[is.na(churn_set)] <- 0

# double checking for NA values again
plot_missing(churn_set)



###########################################
## 2) Data Exploration and Visualization 


# plot total churn in data set 
churn <- filter(churn_set, Churn == "Yes") #filter churn
non_churn <- filter(churn_set, Churn == "No") #filter non-churn
churn_plot <- ggplot(churn_set, aes(x=factor(Churn))) +
  geom_bar(fill="yellow", width = .75) +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(2),
            size = 5,
            vjust = 3) +
  theme_minimal() +
  ggtitle('Total Churn') +
  xlab('') +
  ylab('Customers')
churn_plot #churn plot

# show structure of data
head(churn_set)


### Continuous Variables analysis ###

# Churn distribution by monthly charges
ggplot(churn_set, aes(MonthlyCharges, color = Churn)) +
  geom_freqpoly(binwidth = 5, size = 1.2) +
  labs(title = "Churn distribution by monthly charges",
       x = "Customer's Monthly Charge", 
       y = "Number of customers")

# Churn distribution by total charges (histogram)
ggplot(churn_set, aes(x=round(TotalCharges, digits=0),
                      y = (..count..)/sum(..count..),
                      fill=Churn))+
  geom_histogram(stat = 'bin',
                 bins = 35, 
                 position=position_dodge()) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle('Churn distribution by total charges') +
  xlab('Total Charge') +
  ylab('Share of customers') +
  theme_minimal()

# creating tenure groups
churn_gr <- churn_set %>%
  mutate(tenure_gr = case_when(tenure <= 6 ~ "0.5 year",
                               tenure > 6 & tenure <= 12 ~ "1 years",  
                               tenure > 12 & tenure <= 24 ~ "2 years",
                               tenure > 24 & tenure <= 36 ~ "3 years",
                               tenure > 36 & tenure <= 48 ~ "4 years",
                               tenure > 48 & tenure <= 60 ~ "5 years",
                               tenure > 60 & tenure <= 72 ~ "6 years"))


# churn distribution by  tenure groups
ggplot(churn_gr, aes(tenure_gr, fill = Churn))+
  geom_bar()+
  coord_flip()+   #rotate the graph horizontally
  labs(y = "Number of customers", x = "Tenure Groups")+
  theme_minimal()



### Service types analysis ###

# plot churn by type of services
options(repr.plot.width = 10, repr.plot.height = 10)
plot_grid(
  
  #plot InternetService
  ggplot(churn_set, aes(x=InternetService, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5)), 
  
  #plot MultipleLines
  ggplot(churn_set, aes(x=MultipleLines, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5)),
  
  #plot PhoneService
  ggplot(churn_set, aes(x=PhoneService, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
  
  #plot OnlineSecurity 
  ggplot(churn_set, aes(x=OnlineSecurity, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
  
  #plot OnlineBackup
  ggplot(churn_set, aes(x=OnlineBackup, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),     
  
  #plot DeviceProtection
  ggplot(churn_set, aes(x=DeviceProtection, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),      
  
  #plot TechSupport     
  ggplot(churn_set, aes(x=TechSupport, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
  
  #plot StreamingTV 
  ggplot(churn_set, aes(x=StreamingTV, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
  
  #plot StreamingMovies
  ggplot(churn_set, aes(x=StreamingMovies, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
  align = "h") #align horizontal reference lines 



### Account data analysis ###

# plot churn rate in Customer account data (Contract, PaymentMethod, PaperlessBilling)
plot_grid(
  ggplot(churn_set, aes(x=Contract, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    coord_flip()+   #rotate the graph horizontally
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5)), 
  
  ggplot(churn_set, aes(x=PaymentMethod, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    coord_flip()+   #rotate the graph horizontally
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5)),
  
  ggplot(churn_set, aes(x=PaperlessBilling, fill=Churn))+ 
    geom_bar(position = 'fill')+ 
    coord_flip()+   #rotate the graph horizontally
    scale_fill_ordinal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
  )



### Demographic data analysis ###

# Rename Senior citizen factors from 1/0 to Yes/No
churn_set$SeniorCitizen = mapvalues(churn_set$SeniorCitizen, 
                                    from = c("0", "1"), to = c("No", "Yes"))

# plot Churn by Gender, Senior Citizen, Availability of Partner and Dependents
plot_grid(
  gender <- ggplot(churn_set) +
    geom_bar(aes(x = gender, fill = Churn), position = "fill", stat = "count"),
  senior <- ggplot(churn_set) +
    geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "fill", stat = "count"),
  partners <- ggplot(churn_set) +
    geom_bar(aes(x = Partner, fill = Churn), position = "fill", stat = "count"),  
  dependents <- ggplot(churn_set) +
    geom_bar(aes(x = Dependents, fill = Churn), position = "fill", stat = "count"))




#######################################
## 3) Data Wrangling and Structuring


### Structuring data ###

# Remove unnecessary columns
model_set <- churn_set %>%
  select( -customerID, -gender,-PhoneService, -MultipleLines)

# change the charactor variables to factors
model_set <- model_set  %>%
  mutate_if(is.character, as.factor)

# check changes
str(model_set)


### Split train/test sets ###

# Set seed
set.seed(333)

# Split data: 80% for train set, 20% for test set 
index <- createDataPartition(y = model_set$Churn, p = 0.8, list = FALSE)

churn_train <- model_set[index,]
churn_test <- model_set[ -index,]




#######################################
## 4) Data Modeling


###########################
### Decision Tree Model ###


# train Decision Tree model on train set
tree_fit <- rpart(Churn ~ ., data = churn_train, 
                  method = "class")

# plot Decision tree
rpart.plot(
  tree_fit,
  type = 4,
  extra = 2,
  under = TRUE,
  fallen.leaves = F)

# predict churn on test set
tree_pred <- predict(tree_fit, churn_test, 
                     type = "class")

# accuracy of model
confusionMatrix(tree_pred, churn_test$Churn)

# plot ROC and find AUC for Decision Tree Model
plot.roc(as.numeric(churn_test$Churn), as.numeric(tree_pred),
         main="Decision Tree", lwd=2, type="b",print.auc=TRUE,col ="green")



###########################
### Random Forest Model ###

# assign train and test sets for Random Forest model
RF_train <- churn_train
RF_test <- churn_test

# set 5-fold cross validation to make the process faster
control <- trainControl(method="cv", number = 5)

# create list of mtry values (as tune parameter)
grid <- data.frame(mtry = c(1, 5, 10, 15, 20))

# cross validation of accuracy with ntree=150 for faster computing
train_rf <-  train(Churn~., RF_train, 
                   method = "rf", 
                   ntree = 150,
                   trControl = control,
                   tuneGrid = grid)
# plot results 
ggplot(train_rf)

# display the best mtry value for the model
train_rf$bestTune

# fiting RF model with using the best tune mtry
fit_rf <- randomForest(Churn~., RF_train, 
                       minNode = train_rf$bestTune$mtry)

# Varplot of different parameters 
varImpPlot(fit_rf)

# predict with RF model
rf_pred <- predict(fit_rf, RF_test)

# check accuracy with confusion matrix
confusionMatrix(RF_test$Churn, rf_pred)

# plot ROC and find AUC for Random Forest Model
plot.roc(as.numeric(RF_test$Churn), as.numeric(rf_pred),
         main="Random Forest", lwd=2, type="b",print.auc=TRUE,col ="blue")




####################################
### Support Vector Machine (SVM) ###


# assign train and test sets for SVM model
SVM_train <- churn_train
SVM_test <- churn_test

# tuning parameters
tune_prm <- tune(svm,factor(Churn)~.,data = SVM_train)

# train SVM
SVM_model <- svm(SVM_train$Churn~., data=SVM_train,
                 type="C-classification", gamma=tune_prm$best.model$gamma,
                 cost=tune_prm$best.model$cost,
                 kernel="radial")

# predict with SMV model
SVM_prd <- predict(SVM_model,newdata=SVM_test)

# check accuracy with confusion matrix
confusionMatrix(SVM_prd,SVM_test$Churn)

# plot ROC and find AUC for SVM model 
SVM_plot <- plot.roc (as.numeric(SVM_test$Churn), as.numeric(SVM_prd),
          main="Support Vector Machine (SVM)", lwd=2, type="b", print.auc=TRUE,col ="orange")





#########################
### RESULTS 
#########################



### Confusion Matrix Comparison ###

# confusion matrix for Decision Tree model
confusionMatrix(tree_pred, churn_test$Churn)

# confusion matrix for Random Forest model
confusionMatrix(RF_test$Churn, rf_pred)

# confusion matrix for SVM model
confusionMatrix(SVM_prd,SVM_test$Churn)

#Based on the comparison of confusion matrix for each model, all three models 
#display almost the same level of Accuracy (around 0.8). If the goal of project 
#is to provide high Accuracy and Sensitivity then SVM model can be preferred. 
#At the same time, the Random Forest model has the best combination of balanced 
#values for other parameters of the confusion matrix (Sensitivity, Specificity and Prevalence).



### ROC plots comparison ###

#  ROC plot of Decision Tree
plot.roc (as.numeric(churn_test$Churn), as.numeric(tree_pred),
          main="Decision Tree", lwd=5, type="b", print.auc=TRUE, col ="green")

# ROC plot of Random Forest
plot.roc (as.numeric(RF_test$Churn), as.numeric(rf_pred), 
          main="Random Forest", lwd=5, type="b", print.auc=TRUE, col ="blue")

# ROC plot of SVM
plot.roc (as.numeric(SVM_test$Churn), as.numeric(SVM_prd),
          main="Support Vector Machine (SVM)", lwd=5, type="b", print.auc=TRUE, col ="orange")

#The goal of the project is to predict churn of customers for telecom company
#in order to retain them by offering specials deals. In this case, the most 
#appropriate model for churn prediction will be the *Random Forest* model due to 
#balanced values of such parameters as Accuracy, Sensitivity, Specificity and Prevalence.






#########################
### CONCLUSION 
#########################



# The AUC all models is around 0.7 which is not too high. 
# Accuracy of all models is not very high as well (~ 0.8).
#
# Based on model results comparison, the Random forest model was identified as the most 
# preferable for churn prediction due to balanced parameters from confusion matrix.

