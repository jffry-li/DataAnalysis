Data Analysis on the Credit Card Fraud Detection dataset from Kaggle.
================
Jeffrey Li
November 7, 2018

# Table of contents  
[K-Nearest Neighbors](#k-nearest-neighbors)  
[Logistic Regression model](#logistic-regression-model)  
[Decision Tree model](#decision-tree-model)  
[Random Forest model](#random-forest-model)
[Gradient Boosted Machine model.](#gradient-boosted-machine-model)  
[Extreme Gradient Boosted model.](#extreme-gradient-boosted-model)  
[Conclusion](#conclusion)


Setting up the required libraries.

``` r
knitr::opts_chunk$set(set.seed(123))

rm(list = ls())
options(warn = -1)
library(data.table) # for fread(), a faster read.table.
library(tidyverse) # ggplot and associated packages.
library(caret) # confusionMatrix() and createDataPartition().
library(corrplot) # corrplot() to visualize correlation between variables.
library(rpart) # rpart() for the decision tree model.
library(rpart.plot) # to plot the rpart() model.
library(DMwR) # SMOTE for balancing of dataset.
library(pROC) # roc() for getting the AUC of the ROC.
library(gbm) # for gbm() and the associated functions.
library(xgboost) # for xgboost().
library(precrec) # for evalmod() to find AUPRC.
library(kableExtra)
library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores() - 1)

fraud_data <- fread("creditcard.csv", header = T)
fraud_data <- as.data.frame(fraud_data)
common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Creating a  table to store accuracy of the models.
AUC_tab <- data.frame("Model"=character(), "AUC"=numeric(), "AUPRC"=numeric(), "Precision"=numeric(), "Recall"=numeric(), "Specificity"  = numeric(), "F1Score"=numeric(), stringsAsFactors = FALSE)
```

Check if any data is missing.

``` r
anyNA(fraud_data)
```

    ## [1] FALSE

A quick look at the data.

``` r
str(fraud_data)
```

    ## 'data.frame':    284807 obs. of  31 variables:
    ##  $ Time  : num  0 0 1 1 2 2 4 7 7 9 ...
    ##  $ V1    : num  -1.36 1.192 -1.358 -0.966 -1.158 ...
    ##  $ V2    : num  -0.0728 0.2662 -1.3402 -0.1852 0.8777 ...
    ##  $ V3    : num  2.536 0.166 1.773 1.793 1.549 ...
    ##  $ V4    : num  1.378 0.448 0.38 -0.863 0.403 ...
    ##  $ V5    : num  -0.3383 0.06 -0.5032 -0.0103 -0.4072 ...
    ##  $ V6    : num  0.4624 -0.0824 1.8005 1.2472 0.0959 ...
    ##  $ V7    : num  0.2396 -0.0788 0.7915 0.2376 0.5929 ...
    ##  $ V8    : num  0.0987 0.0851 0.2477 0.3774 -0.2705 ...
    ##  $ V9    : num  0.364 -0.255 -1.515 -1.387 0.818 ...
    ##  $ V10   : num  0.0908 -0.167 0.2076 -0.055 0.7531 ...
    ##  $ V11   : num  -0.552 1.613 0.625 -0.226 -0.823 ...
    ##  $ V12   : num  -0.6178 1.0652 0.0661 0.1782 0.5382 ...
    ##  $ V13   : num  -0.991 0.489 0.717 0.508 1.346 ...
    ##  $ V14   : num  -0.311 -0.144 -0.166 -0.288 -1.12 ...
    ##  $ V15   : num  1.468 0.636 2.346 -0.631 0.175 ...
    ##  $ V16   : num  -0.47 0.464 -2.89 -1.06 -0.451 ...
    ##  $ V17   : num  0.208 -0.115 1.11 -0.684 -0.237 ...
    ##  $ V18   : num  0.0258 -0.1834 -0.1214 1.9658 -0.0382 ...
    ##  $ V19   : num  0.404 -0.146 -2.262 -1.233 0.803 ...
    ##  $ V20   : num  0.2514 -0.0691 0.525 -0.208 0.4085 ...
    ##  $ V21   : num  -0.01831 -0.22578 0.248 -0.1083 -0.00943 ...
    ##  $ V22   : num  0.27784 -0.63867 0.77168 0.00527 0.79828 ...
    ##  $ V23   : num  -0.11 0.101 0.909 -0.19 -0.137 ...
    ##  $ V24   : num  0.0669 -0.3398 -0.6893 -1.1756 0.1413 ...
    ##  $ V25   : num  0.129 0.167 -0.328 0.647 -0.206 ...
    ##  $ V26   : num  -0.189 0.126 -0.139 -0.222 0.502 ...
    ##  $ V27   : num  0.13356 -0.00898 -0.05535 0.06272 0.21942 ...
    ##  $ V28   : num  -0.0211 0.0147 -0.0598 0.0615 0.2152 ...
    ##  $ Amount: num  149.62 2.69 378.66 123.5 69.99 ...
    ##  $ Class : int  0 0 0 0 0 0 0 0 0 0 ...

A look at the amount of classified cases.

``` r
table(fraud_data$Class)
```

    ## 
    ##      0      1 
    ## 284315    492

``` r
print(prop.table(table(fraud_data$Class)))
```

    ## 
    ##           0           1 
    ## 0.998272514 0.001727486

The dataset is extremely unbalanced with 284315 of the entries being classified as non-fraud and 492 being fraud. Therefore, 99.82725% of the data is non-fraudulent and 0.17275% of the data is fraudulent.

A plot to visualize this difference in amount withdrawn.

``` r
class_plot <- ggplot(fraud_data, aes(x = Class, y = Amount, group = Class)) + geom_boxplot() + ggtitle("Distribution of transaction amount by class") + common_theme
class_plot
```

![](/figure-markdown_github/unnamed-chunk-5-1.png)

There is a larger range in non-fraudulent transactions.

Lets look at the mean, median and variance of these transactions.

``` r
fraud_data %>% group_by(Class) %>% summarize(mean(Amount), median(Amount), var(Amount))
```

    ## # A tibble: 2 x 4
    ##   Class `mean(Amount)` `median(Amount)` `var(Amount)`
    ##   <int>          <dbl>            <dbl>         <dbl>
    ## 1     0           88.3            22           62553.
    ## 2     1          122.              9.25        65886.

Lets look at the correlation between variables in the dataset.

``` r
fraud_data$Class <- as.numeric(fraud_data$Class)
corr_plot <- corrplot(cor(fraud_data[,-grep("Time", colnames(fraud_data))]), method = "square", type = "upper")
```

![](/figure-markdown_github/unnamed-chunk-7-1.png)

There appear to be correlations in the "Amount" and "Class" features of the dataset.

 ### Splitting the data into a train and test set.

``` r
set.seed(123)
#standardize Amount variable.
fraud_data <- subset(fraud_data, select = -c(Time))
#fraud_data$Amount <- scale(fraud_data$Amount, center = TRUE, scale = TRUE)
fraud_data$Class <- as.numeric(fraud_data$Class)
#split the data into a training set and test set.
train_ind <- createDataPartition(fraud_data$Class, times = 1, p = 0.8, list = F)
fraud_train <- fraud_data[train_ind,]
fraud_test <- fraud_data[-train_ind,]
#isolate the Class variable.
class_train <- fraud_data$Class[train_ind]
class_test <- fraud_data$Class[-train_ind]

baseline_non_fraud <- nrow(fraud_data[fraud_data$Class == '0',])/nrow(fraud_data)
baseline_non_fraud
```

    ## [1] 0.9982725

``` r
baseline_fraud <- nrow(fraud_data[fraud_data$Class == '1',])/nrow(fraud_data)
baseline_fraud
```

    ## [1] 0.001727486

``` r
baseline_test_fraud <- length(class_test[class_test == '1'])/length(class_test)
baseline_test_fraud
```

    ## [1] 0.001948702

The baseline accuracy for fraud cases on the data set is 0.0017275.
The baseline accuracy for fraud cases in the test/validation set is 0.0019487.

### 10-fold Cross Validation on the data set.

``` r
data_cv <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = T,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = T)
```

### Using SMOTE to balance the dataset.

``` r
#Making Class a factor again.
fraud_train$Class <- as.factor(fraud_train$Class)
#Using SMOTE to create a balanced dataset.
smote_train <- SMOTE(Class ~., as.data.frame(fraud_train), perc.over = 2000, perc.under = 105)
#Changing Class to a numeric to later use.
smote_train$Class <- as.numeric(as.character(smote_train$Class))
#Checking in the dataset is balanced.
table(smote_train$Class)
```

    ## 
    ##    0    1 
    ## 8001 8001

``` r
class_smote <- smote_train$Class
```

``` r
#Creating a manipulatable dataset.
cc_train <- smote_train 
cc_train$Class <- as.factor(cc_train$Class)
levels(cc_train$Class) <- make.names(c(0, 1))
```


# K-Nearest Neighbors.

``` r
knn_smote_quiet <- capture.output(knn_smote <- (train(Class~., data=cc_train, method ="knn", trControl = data_cv, tuneLength = 20, metric = "ROC")))

knn_pred <- predict(knn_smote, fraud_test, type = "prob")

knn_roc <- roc(response = class_test, predictor = knn_pred$X1, type = "response")
plot(knn_roc, main = paste("AUC:", round(knn_roc$auc, 4)))
```

![](/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
knn_threshold <- coords(knn_roc, "best")
```

``` r
knn_bins <- as.numeric(knn_pred$X1 > knn_threshold[1])
knn_conf_mat <- confusionMatrix(as.factor(knn_bins), as.factor(class_test), mode = 'everything', positive = '1')
knn_conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 55102    13
    ##          1  1748    98
    ##                                           
    ##                Accuracy : 0.9691          
    ##                  95% CI : (0.9676, 0.9705)
    ##     No Information Rate : 0.9981          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.0968          
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.882883        
    ##             Specificity : 0.969252        
    ##          Pos Pred Value : 0.053088        
    ##          Neg Pred Value : 0.999764        
    ##               Precision : 0.053088        
    ##                  Recall : 0.882883        
    ##                      F1 : 0.100153        
    ##              Prevalence : 0.001949        
    ##          Detection Rate : 0.001720        
    ##    Detection Prevalence : 0.032408        
    ##       Balanced Accuracy : 0.926068        
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
knn_prc <- evalmod(scores = knn_pred$X1, labels = class_test, mode = "rocprc")
knn_prc
```

    ## 
    ##     === AUCs ===
    ## 
    ##      Model name Dataset ID Curve type       AUC
    ##    1         m1          1        ROC 0.9423853
    ##    2         m1          1        PRC 0.3170495
    ## 
    ## 
    ##     === Input data ===
    ## 
    ##      Model name Dataset ID # of negatives # of positives
    ##    1         m1          1          56850            111

``` r
autoplot(knn_prc, "PRC")
```

![](/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
knn_pr <- attr(knn_prc, "aucs")

knn_AUC <- round(knn_roc$auc,4)
knn_AUPRC <- round(knn_pr$aucs[2],4)
knn_Prec <- as.numeric(round(knn_conf_mat$byClass["Precision"],4))
knn_Rec <- as.numeric(round(knn_conf_mat$byClass["Recall"],4))
knn_Spec <- as.numeric(round(knn_conf_mat$byClass["Specificity"],4))
knn_F1 <- as.numeric(round(knn_conf_mat$byClass["F1"],4))

AUC_tab <- rbind(AUC_tab, data.frame(Model = "K-Nearest Neighbors", AUC = knn_AUC, AUPRC = knn_AUPRC, Precision = knn_Prec, Recall = knn_Rec, Specificity = knn_Spec, F1Score = knn_F1))
```

Using varImp() to look at the importance of variables in the model.

``` r
plot(varImp(knn_smote))
```

![](/figure-markdown_github/unnamed-chunk-14-1.png)

The KNN model has variables V14, V4, V12 ,V11, V10, V3,V2 and V9 all above 80% importance.

# Logistic Regression model.

``` r
#registerDoParallel(cluster)

levels(fraud_train$Class) <- make.names(c(0, 1))
log_smote_quiet <- capture.output(log_smote <- suppressWarnings(train(Class ~., data = cc_train, method = "glm", trControl = data_cv, family="binomial", metric = "ROC")))

#stopCluster(cluster)
#registerDoSEQ()

log_pred <- predict(log_smote, fraud_test, type = "prob")

log_roc <- roc(response = class_test, predictor = log_pred$X1, type = "response")
plot(log_roc, main = paste("AUC:", round(log_roc$auc,4)))
```

![](/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
log_threshold <- coords(log_roc, "best")
```

``` r
log_bins <- as.numeric(log_pred$X1 > log_threshold[1])
log_conf_mat <- confusionMatrix(as.factor(log_bins), as.factor(class_test), mode = 'everything', positive = '1')
log_conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 55232    10
    ##          1  1618   101
    ##                                         
    ##                Accuracy : 0.9714        
    ##                  95% CI : (0.97, 0.9728)
    ##     No Information Rate : 0.9981        
    ##     P-Value [Acc > NIR] : 1             
    ##                                         
    ##                   Kappa : 0.1071        
    ##  Mcnemar's Test P-Value : <2e-16        
    ##                                         
    ##             Sensitivity : 0.909910      
    ##             Specificity : 0.971539      
    ##          Pos Pred Value : 0.058755      
    ##          Neg Pred Value : 0.999819      
    ##               Precision : 0.058755      
    ##                  Recall : 0.909910      
    ##                      F1 : 0.110383      
    ##              Prevalence : 0.001949      
    ##          Detection Rate : 0.001773      
    ##    Detection Prevalence : 0.030179      
    ##       Balanced Accuracy : 0.940725      
    ##                                         
    ##        'Positive' Class : 1             
    ## 

``` r
log_prc <- evalmod(scores = log_pred$X1, labels = class_test, mode = "rocprc")
log_prc
```

    ## 
    ##     === AUCs ===
    ## 
    ##      Model name Dataset ID Curve type       AUC
    ##    1         m1          1        ROC 0.9789446
    ##    2         m1          1        PRC 0.7268726
    ## 
    ## 
    ##     === Input data ===
    ## 
    ##      Model name Dataset ID # of negatives # of positives
    ##    1         m1          1          56850            111

``` r
autoplot(log_prc, "PRC")
```

![](/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
log_pr <- attr(log_prc, "aucs")

log_AUC <- round(log_roc$auc,4)
log_AUPRC <- round(log_pr$aucs[2],4)
log_Prec <- as.numeric(round(log_conf_mat$byClass["Precision"],4))
log_Rec <- as.numeric(round(log_conf_mat$byClass["Recall"],4))
log_Spec <- as.numeric(round(log_conf_mat$byClass["Specificity"],4))
log_F1 <- as.numeric(round(log_conf_mat$byClass["F1"],4))

AUC_tab <- rbind(AUC_tab, data.frame(Model = "Logistic Regression", AUC = log_AUC, AUPRC = log_AUPRC, Precision = log_Prec, Recall = log_Rec, Specificity = log_Spec, F1Score = log_F1))
```

``` r
plot(varImp(log_smote))
```

![](/figure-markdown_github/unnamed-chunk-17-1.png)

The logistic regression model has variables: V4, V14, V12 and V11 as above 80% importance to the model. It appears that the logistic regression model used a very large portion of the variables to create the model.

# Decision Tree model.

``` r
registerDoParallel(cluster)

tree_smote_quiet <- capture.output(tree_smote <- train(Class ~., data = fraud_train, method = "rpart", trControl = data_cv, metric = "ROC",  control = rpart.control(minbucket =20)))

stopCluster(cluster)
registerDoSEQ()

prp(tree_smote$finalModel)
```

![](/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
tree_predict <- predict(tree_smote$finalModel, fraud_test, type = "prob")

dec_roc <- roc(class_test, tree_predict[,"X1"], type = "response")
plot(dec_roc, main = paste("AUC:", round(dec_roc$auc,4)))
```

![](/figure-markdown_github/unnamed-chunk-18-2.png)

``` r
dec_threshold <- coords(dec_roc, "best")[1]
```

``` r
tree_bins <- as.numeric(tree_predict[,"X1"] > dec_threshold)
tree_conf_mat <- confusionMatrix(as.factor(tree_bins), as.factor(class_test), mode = 'everything', positive = '1')
tree_conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 56835    41
    ##          1    15    70
    ##                                           
    ##                Accuracy : 0.999           
    ##                  95% CI : (0.9987, 0.9993)
    ##     No Information Rate : 0.9981          
    ##     P-Value [Acc > NIR] : 5.825e-09       
    ##                                           
    ##                   Kappa : 0.7138          
    ##  Mcnemar's Test P-Value : 0.0008355       
    ##                                           
    ##             Sensitivity : 0.630631        
    ##             Specificity : 0.999736        
    ##          Pos Pred Value : 0.823529        
    ##          Neg Pred Value : 0.999279        
    ##               Precision : 0.823529        
    ##                  Recall : 0.630631        
    ##                      F1 : 0.714286        
    ##              Prevalence : 0.001949        
    ##          Detection Rate : 0.001229        
    ##    Detection Prevalence : 0.001492        
    ##       Balanced Accuracy : 0.815183        
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
dec_prc <- evalmod(scores = as.numeric(tree_predict[,"X1"]), labels = class_test, mode = "rocprc")
dec_prc 
```

    ## 
    ##     === AUCs ===
    ## 
    ##      Model name Dataset ID Curve type       AUC
    ##    1         m1          1        ROC 0.8151736
    ##    2         m1          1        PRC 0.5225492
    ## 
    ## 
    ##     === Input data ===
    ## 
    ##      Model name Dataset ID # of negatives # of positives
    ##    1         m1          1          56850            111

``` r
autoplot(dec_prc, "PRC")
```

![](/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
dec_pr <- attr(dec_prc, 'aucs')

tree_AUC <- round(dec_roc$auc,4)
tree_AUPRC <- round(dec_pr$aucs[2],4)
tree_Prec <- as.numeric(round(tree_conf_mat$byClass["Precision"],4))
tree_Rec <- as.numeric(round(tree_conf_mat$byClass["Recall"],4))
tree_Spec <- as.numeric(round(tree_conf_mat$byClass["Specificity"],4))
tree_F1 <- as.numeric(round(tree_conf_mat$byClass["F1"],4))

AUC_tab <- rbind(AUC_tab, data.frame(Model = "Decision Tree", AUC = tree_AUC, AUPRC = tree_AUPRC, Precision = tree_Prec, Recall = tree_Rec, Specificity = tree_Spec, F1Score = tree_F1))
```

``` r
plot(varImp(tree_smote))
```

![](/figure-markdown_github/unnamed-chunk-20-1.png)

The decision tree model has variables V12 and V17 above 80% importance. An interesting observation is that the decision tree model only considers 9 variables.

# Random Forest model.
``` r
rf_smote_quiet <- capture.output(rf_smote <- train(Class ~., data = cc_train, method = "rf", trControl = data_cv, verbose = T, metric = "ROC"))

rf_pred <- predict(rf_smote, fraud_test, type = "prob")

rf_roc <- roc(class_test, rf_pred$X1, type = "response")
plot(rf_roc, main = paste("AUC:", round(rf_roc$auc,4)))
```

![](/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
rf_threshold <- coords(rf_roc, "best")
```

``` r
rf_bins <- as.numeric(rf_pred$X1 > rf_threshold[1])
rf_conf_mat <- confusionMatrix(as.factor(rf_bins), as.factor(class_test), mode = 'everything', positive = '1')
rf_conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 54558     9
    ##          1  2292   102
    ##                                          
    ##                Accuracy : 0.9596         
    ##                  95% CI : (0.958, 0.9612)
    ##     No Information Rate : 0.9981         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.078          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.918919       
    ##             Specificity : 0.959683       
    ##          Pos Pred Value : 0.042607       
    ##          Neg Pred Value : 0.999835       
    ##               Precision : 0.042607       
    ##                  Recall : 0.918919       
    ##                      F1 : 0.081437       
    ##              Prevalence : 0.001949       
    ##          Detection Rate : 0.001791       
    ##    Detection Prevalence : 0.042029       
    ##       Balanced Accuracy : 0.939301       
    ##                                          
    ##        'Positive' Class : 1              
    ## 

``` r
rf_prc <- evalmod(scores = rf_pred$X1, labels = class_test, mode = "rocprc")
rf_prc
```

    ## 
    ##     === AUCs ===
    ## 
    ##      Model name Dataset ID Curve type       AUC
    ##    1         m1          1        ROC 0.9760581
    ##    2         m1          1        PRC 0.7444496
    ## 
    ## 
    ##     === Input data ===
    ## 
    ##      Model name Dataset ID # of negatives # of positives
    ##    1         m1          1          56850            111

``` r
rf_pr <- attr(rf_prc, 'aucs')

autoplot(rf_prc, "PRC")
```

![](/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
rf_AUC <- round(rf_roc$auc,4)
rf_AUPRC <- round(rf_pr$aucs[2],4)
rf_Prec <- as.numeric(round(rf_conf_mat$byClass["Precision"],4))
rf_Rec <- as.numeric(round(rf_conf_mat$byClass["Recall"],4))
rf_Spec <- as.numeric(round(rf_conf_mat$byClass["Specificity"],4))
rf_F1 <- as.numeric(round(rf_conf_mat$byClass["F1"],4))

AUC_tab <- rbind(AUC_tab, data.frame(Model = "Random Forest", AUC = rf_AUC, AUPRC = rf_AUPRC, Precision = rf_Prec, Recall = rf_Rec, Specificity = rf_Spec, F1Score =  rf_F1))
```

``` r
plot(varImp(rf_smote))
```

![](/figure-markdown_github/unnamed-chunk-23-1.png)

From the chart, the V14 is the only variable above 80% importance for the

# Gradient Boosted Machine model.

``` r
tuneGrid <- expand.grid(interaction.depth = 3, n.trees = 500,
                   shrinkage =  0.01,
                   n.minobsinnode=100)

gbm_smote_quiet <- capture.output(gbm_smote <- train(Class ~., data = cc_train, method = "gbm", trControl = data_cv, verbose = F, metric = "ROC", tuneGrid = tuneGrid))

gbm_pred <- predict(gbm_smote, fraud_test, type = "prob")

gbm_roc <- roc(class_test, gbm_pred$X1)
plot(gbm_roc, main = paste("AUC:", round(gbm_roc$auc,4)))
```

![](/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
gbm_threshold <- coords(gbm_roc, "best")
```

``` r
gbm_bins <- as.numeric(gbm_pred$X1 >gbm_threshold[1])
gbm_conf_mat <- confusionMatrix(as.factor(gbm_bins), as.factor(class_test), mode = 'everything', positive = '1')
gbm_conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 54918    11
    ##          1  1932   100
    ##                                           
    ##                Accuracy : 0.9659          
    ##                  95% CI : (0.9644, 0.9674)
    ##     No Information Rate : 0.9981          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.09            
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.900901        
    ##             Specificity : 0.966016        
    ##          Pos Pred Value : 0.049213        
    ##          Neg Pred Value : 0.999800        
    ##               Precision : 0.049213        
    ##                  Recall : 0.900901        
    ##                      F1 : 0.093327        
    ##              Prevalence : 0.001949        
    ##          Detection Rate : 0.001756        
    ##    Detection Prevalence : 0.035674        
    ##       Balanced Accuracy : 0.933458        
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
gbm_prc <- evalmod(scores = gbm_pred$X1, labels = class_test, mode = "rocprc")
gbm_prc
```

    ## 
    ##     === AUCs ===
    ## 
    ##      Model name Dataset ID Curve type       AUC
    ##    1         m1          1        ROC 0.9712636
    ##    2         m1          1        PRC 0.7290559
    ## 
    ## 
    ##     === Input data ===
    ## 
    ##      Model name Dataset ID # of negatives # of positives
    ##    1         m1          1          56850            111

``` r
autoplot(gbm_prc, "PRC")
```

![](/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
gbm_pr <- attr(gbm_prc, 'aucs')

gbm_AUC <-  round(gbm_roc$auc,4)
gbm_AUPRC <- round(gbm_pr$aucs[2],4)
gbm_Prec <- as.numeric(round(gbm_conf_mat$byClass["Precision"],4))
gbm_Rec <-  as.numeric(round(gbm_conf_mat$byClass["Recall"],4))
gbm_Spec <- as.numeric(round(gbm_conf_mat$byClass["Specificity"],4))
gbm_F1 <- as.numeric(round(gbm_conf_mat$byClass["F1"],4))


AUC_tab <- rbind(AUC_tab, data.frame(Model = "Gradient Boosted Machine", AUC = gbm_AUC, AUPRC = gbm_AUPRC, Precision = gbm_Prec, Recall = gbm_Rec, Specificity = gbm_Spec, F1Score = gbm_F1))
```

``` r
plot(varImp(gbm_smote))
```

![](/figure-markdown_github/unnamed-chunk-26-1.png)

The gradient boosted machine model heavily places importance on variable V14 as it is both the only variable above 20% importance and is 100% importance.

# Extreme Gradient Boosted model. 

``` r
xgb_grid = expand.grid(nrounds = 500, max_depth =6, eta = 0.1, gamma = 0, colsample_bytree =1 , min_child_weight= 100, subsample =1)

xgb_smote_quiet <- capture.output(xgb_smote <- train(Class~., data=cc_train, method="xgbTree", metric="ROC", trControl=data_cv, tuneGrid=xgb_grid))

xgb_pred <- predict(xgb_smote, fraud_test, type = "prob")

xgb_roc <- roc(class_test, xgb_pred$X1)
plot(xgb_roc, main = paste("AUC:", round(xgb_roc$auc,4)))
```

![](/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
xgb_threshold <- coords(xgb_roc, "best")
```

``` r
xgb_bins <- as.numeric(xgb_pred$X1 > xgb_threshold)
xgb_conf_mat <- confusionMatrix(as.factor(xgb_bins), as.factor(class_test), mode = 'everything', positive = '1')
xgb_conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 56312    19
    ##          1   538    92
    ##                                          
    ##                Accuracy : 0.9902         
    ##                  95% CI : (0.9894, 0.991)
    ##     No Information Rate : 0.9981         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.2458         
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.828829       
    ##             Specificity : 0.990536       
    ##          Pos Pred Value : 0.146032       
    ##          Neg Pred Value : 0.999663       
    ##               Precision : 0.146032       
    ##                  Recall : 0.828829       
    ##                      F1 : 0.248313       
    ##              Prevalence : 0.001949       
    ##          Detection Rate : 0.001615       
    ##    Detection Prevalence : 0.011060       
    ##       Balanced Accuracy : 0.909683       
    ##                                          
    ##        'Positive' Class : 1              
    ## 

``` r
xgb_prc <- evalmod(scores = xgb_pred$X1, labels = class_test, mode = "rocprc")
xgb_prc
```

    ## 
    ##     === AUCs ===
    ## 
    ##      Model name Dataset ID Curve type       AUC
    ##    1         m1          1        ROC 0.9750382
    ##    2         m1          1        PRC 0.7408428
    ## 
    ## 
    ##     === Input data ===
    ## 
    ##      Model name Dataset ID # of negatives # of positives
    ##    1         m1          1          56850            111

``` r
autoplot(xgb_prc, "PRC")
```

![](/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
xgb_pr <- attr(xgb_prc, 'aucs')

xgb_AUC <- round(xgb_roc$auc,4)
xgb_AUPRC <- round(xgb_pr$aucs[2],4)
xgb_Prec <- as.numeric(round(xgb_conf_mat$byClass["Precision"],4))
xgb_Rec <- as.numeric(round(xgb_conf_mat$byClass["Recall"],4))
xgb_Spec <- as.numeric(round(xgb_conf_mat$byClass["Specificity"],4))
xgb_F1 <- as.numeric(round(xgb_conf_mat$byClass["F1"],4))


AUC_tab <- rbind(AUC_tab, data.frame(Model = "Extreme Gradient Boosting", AUC = xgb_AUC, AUPRC = xgb_AUPRC, Precision = xgb_Prec, Recall = xgb_Rec, Specificity = xgb_Spec, F1Score = xgb_F1))
```

``` r
plot(varImp(xgb_smote))
```

![](/figure-markdown_github/unnamed-chunk-29-1.png)

The extreme gradient boosted model places 100% importance on variable V14 and it is the only variable above 20% importance.

# Conclusion

Due to the dataset being unbalanced we had to utilize Synthetic Minority Oversampling Technique (SMOTE) in unorder to balance the dataset. We set up the trainControl() function which will control the nuances of the train() function and allow us to Cross Validate our training set.

We utilized various models to predict the class of each sample:
<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
AUC
</th>
<th style="text-align:right;">
AUPRC
</th>
<th style="text-align:right;">
Precision
</th>
<th style="text-align:right;">
Recall
</th>
<th style="text-align:right;">
Specificity
</th>
<th style="text-align:right;">
F1Score
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
K-Nearest Neighbors
</td>
<td style="text-align:right;">
0.9424
</td>
<td style="text-align:right;">
0.3170
</td>
<td style="text-align:right;">
0.0531
</td>
<td style="text-align:right;">
0.8829
</td>
<td style="text-align:right;">
0.9693
</td>
<td style="text-align:right;">
0.1002
</td>
</tr>
<tr>
<td style="text-align:left;">
Logistic Regression
</td>
<td style="text-align:right;">
0.9789
</td>
<td style="text-align:right;">
0.7269
</td>
<td style="text-align:right;">
0.0588
</td>
<td style="text-align:right;">
0.9099
</td>
<td style="text-align:right;">
0.9715
</td>
<td style="text-align:right;">
0.1104
</td>
</tr>
<tr>
<td style="text-align:left;">
Decision Tree
</td>
<td style="text-align:right;">
0.8152
</td>
<td style="text-align:right;">
0.5225
</td>
<td style="text-align:right;">
0.8235
</td>
<td style="text-align:right;">
0.6306
</td>
<td style="text-align:right;">
0.9997
</td>
<td style="text-align:right;">
0.7143
</td>
</tr>
<tr>
<td style="text-align:left;">
Random Forest
</td>
<td style="text-align:right;">
0.9761
</td>
<td style="text-align:right;">
0.7444
</td>
<td style="text-align:right;">
0.0426
</td>
<td style="text-align:right;">
0.9189
</td>
<td style="text-align:right;">
0.9597
</td>
<td style="text-align:right;">
0.0814
</td>
</tr>
<tr>
<td style="text-align:left;">
Gradient Boosted Machine
</td>
<td style="text-align:right;">
0.9713
</td>
<td style="text-align:right;">
0.7291
</td>
<td style="text-align:right;">
0.0492
</td>
<td style="text-align:right;">
0.9009
</td>
<td style="text-align:right;">
0.9660
</td>
<td style="text-align:right;">
0.0933
</td>
</tr>
<tr>
<td style="text-align:left;">
Extreme Gradient Boosting
</td>
<td style="text-align:right;">
0.9750
</td>
<td style="text-align:right;">
0.7408
</td>
<td style="text-align:right;">
0.1460
</td>
<td style="text-align:right;">
0.8288
</td>
<td style="text-align:right;">
0.9905
</td>
<td style="text-align:right;">
0.2483
</td>
</tr>
</tbody>
</table>
A look at the importance of variables to the models above reveals an interesting observation. Variables V4, V12 and V14 hold a lot of importance in all the models and sometimes even holding 100% importance.

There are many ways to determine the accuracy of the model and the table above shows some of the options. Due to the unbalanced nature of the dataset looking at merely the area under the curve (AUC) of the recieving operating characteristic (ROC) is a poor measurement of accuracy because ROC curves are insensitive to class balance.

A better way to measure accuracy for datasets with class imbalance is to look at the area under the precision-recall curve (AUPRC). Precision also known as positive predicted value (PPV), is a measurement of how many of the observations selected are relevant and recall/sensitivity is the fraction of the relevant selected observations over the total relevant observations.

For an example of precision and recall using our logistic regression model:

    ##           Reference
    ## Prediction     0     1
    ##          0 55232    10
    ##          1  1618   101

Precision = 101/(1618 + 101) = 0.0588 or 5.88%. Recall = 101/(10+101) = 0.9099 or 90.99%.

The AUPRC takes both precision and recall and plots them against each other and calculates the area under it. The best model for AUPRC is the Random Forest.

![](CCFraudAnalysis_files/figure-markdown_github/unnamed-chunk-32-1.png)

However it is not the model that has the best precision or best recall. The decision tree model has by far the best precision:

    ##           Reference
    ## Prediction     0     1
    ##          0 56835    41
    ##          1    15    70

Precision = 70/(70+15) = 0.8235 or 82.35%

The model predicted 85 samples as fraudulent and of those 85, 70 samples were fraudulent cases. However there were 111 fraudulent cases in the test/validation dataset therefore this model was only able to correctly identify 70 out of the 111 fraudulent cases. This measure is called recall or specificity.

The gradient boosted machine model has the best recall/specificity.

    ##           Reference
    ## Prediction     0     1
    ##          0 54918    11
    ##          1  1932   100

Recall/Sensitivity = 104/(104+7) = 0.9369 or 93.69%.

The model predicted 3932 as fraudulent and of those 3932, only 104 of those were fraudulent cases. However in this case, recall/sensitivity is a measured of the number of actual predicted fraudulent cases over the total number of fraudulent cases. Therefore out of the 111 fraudulent cases in the test/validation dataset, the model was able to predict 104 of the 111 fraudulent cases. While this model was able to predict the largest number of fraudulent cases, it incorrectly identified 3828 non-fraudulent cases as fraudulent which gives the model a very poor precision.

Another metric related to sensitivity that can be used to test model accuracy is specificity. Specificity, in our example, is the measure of the number of non-fraudulent cases correctly identified as non-fraudulent. The decision tree model has the best specificity with 99.97%, this model nearly correctly classifies all of the non-fraudulent cases correctly as non-fraud.

    ##           Reference
    ## Prediction     0     1
    ##          0 56835    41
    ##          1    15    70

The F<sub>1</sub> score is a measure of accuracy that uses both precision and recall to compute a score. The F<sub>1</sub> score is a harmonic average of precision and recall which ranges from 0 to 1, with 1 being a model that has perfect precision and recall and 0 being a model that has either precision or recall at 0.

![equation](https://latex.codecogs.com/gif.latex?F_1%20%3D%202*%5Cfrac%7Bprecision*recall%7D%7Bprecision%20+%20recall%7D)

The model with the best F<sub>1</sub> score is the decision tree model with a score of 0.7143.

When considering which model was "best" at predicting fraudulent cases one must consider many factors. The cost of misclassifying that creates both false negatives and false positives might factor into choosing the model. In a real world application, one might prefer the model which minimizes the cost or the model which maximizes the sensitivity and specificity.

In our analysis of the data the decision tree model was the best in precision, specificity and F1 score while still maintaining a decent recall/sensitvity.
