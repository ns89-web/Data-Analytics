# Set Working Directory
setwd("C:/Users/nsrinivn/Desktop/PGDDA/PGDDA/study/assignments/course-3/telecom-churn-casestudy")

# Install and Load the required packages
library("car")
library("Hmisc")
library("ROCR")
library("caret")
library("caTools")
library("MASS")
library("ggplot2")  
library(e1071)

############## Checkpoint-1: Data Understanding and Preparation of Master File.####################
# Load the given files.
# we will use string as factors argument as True to avoid conversions later.
customer_data <- read.csv("customer_data.csv",stringsAsFactors = T)
str(customer_data)
churn_data <- read.csv("churn_data.csv", stringsAsFactors = T)
str(churn_data)
internet_data <- read.csv("internet_data.csv", stringsAsFactors = T)
str(internet_data)

# Collate the 3 files in a single file.
# WE see that there are 7043 observations in each of the files, with a common customerID field.
# so we will go ahead and merge the files to a master data
churn <- merge(customer_data,churn_data)
str(churn)
churn <- merge(churn,internet_data)

# Understand the structure of the collated file.
str(churn)
# We see that, after using the arg stringsAsFactors = T, while reading, the data structure is ok. 
# There are 21 observations in the dataset, with 3 continuous variables (charges related and the tenure).

#############   Checkpoint -2: Exploratory Data Analysis ##########################
# Make bar charts to find interesting relationships between variables.
# First lets do some univariate to understand the distribution of the fields and simultaneuously see the factors affecting churn.

# *** # when to people leave?  early or late?
ggplot(churn, aes(churn$tenure, fill=factor(churn$Churn))) + geom_bar()
# the churn rate decreases with increase in tenure
# looking at the tenure we see that 600+ customers are new or have been for least tenure 
#and the churn rate is also pretty high at this level

# - which gender has the max churn?
ggplot(churn, aes(churn$gender,fill=factor(churn$Churn))) + geom_bar()
ggplot(churn, aes(churn$Churn,fill=factor(churn$gender))) + geom_bar()
#looks like the churn rate is same and not really dependent on the gender

# * # do senior citizens churn more?
ggplot(churn, aes(churn$SeniorCitizen,fill=factor(churn$Churn))) + geom_bar()
# The proportion of senior citizens is very less and looks like the churn rate might be higher amongst them.

# * # who churn more ? Married or single?
ggplot(churn, aes(churn$Partner,fill=factor(churn$Churn))) + geom_bar()
# looks like the churn rate is higher among the people who are single

# - # does having a phone and internet service affect the churn?
ggplot(churn, aes(churn$PhoneService,fill=factor(churn$Churn))) + geom_bar()
# does not look like it, most customers have phone and internet services. So, it does not seem to affect the churn much

# w.r.t having multiple lines of internet connectivity?
ggplot(churn, aes(churn$MultipleLines,fill=factor(churn$Churn))) + geom_bar()
ggplot(churn, aes(churn$Churn,fill=factor(churn$MultipleLines))) + geom_bar()
# no significant effect of holding multiple lines on churn :(

# *** # does the type of internet service chosen by the customer lead to churn?
ggplot(churn, aes(churn$InternetService,fill=factor(churn$Churn))) + geom_bar()
# looks like this might be a significant factor that leads to churn, we see that the number of customers churning 
# is higher among the customers with Fiber Optic connection

# *** # does opting for online security help reduce churn?
ggplot(churn, aes(churn$OnlineSecurity,fill=factor(churn$Churn))) + geom_bar()
# Yes. The churn is more among customers who do not opt for online security.

# ** # does opting for online backup help reduce churn?
ggplot(churn, aes(churn$OnlineBackup,fill=factor(churn$Churn))) + geom_bar()
# Yes. Again, The churn is more among customers who do not opt for online backup.

# ** # does opting for device protection help reduce churn?
ggplot(churn, aes(churn$DeviceProtection,fill=factor(churn$Churn))) + geom_bar()
# Again, the churn is more amongst people who do not opt for device protection

# ** # does opting for tech support help reduce churn?
ggplot(churn, aes(churn$TechSupport,fill=factor(churn$Churn))) + geom_bar()
# Again, the churn is more amongst people who do not opt for tech support
# so, comprehensively, there might be a correlation that, churn is more among people who do not take extra optional services.

# lets see if the streaming services have any effect on the churn
ggplot(churn, aes(churn$StreamingTV,fill=factor(churn$Churn))) + geom_bar()
ggplot(churn, aes(churn$StreamingMovies,fill=factor(churn$Churn))) + geom_bar()
# there is no significant effect on the churn, amonst customers who are opting for Streaming services. 

# *** # does the type of contract affect churn
ggplot(churn, aes(churn$Contract,fill=factor(churn$Churn))) + geom_bar()
# Looks like, the churn rate is highest among people choosing month-to-month contracts.

# though from buisness sense, opting for paperless billing should not affect churn,
ggplot(churn, aes(churn$PaperlessBilling,fill=factor(churn$Churn))) + geom_bar()
# but here we see that number of customers churning is more among people who have opted for paperless billing .

# ** # effect of payment method on the churn
ggplot(churn, aes(churn$PaymentMethod,fill=factor(churn$Churn))) + geom_bar()
# here again, payment method cannot be a detrimental factor. but we see that churn is higher among customers paying through electronic check

# *** # Now, looking at the relation between the charges paid by the customers monthly and churn
ggplot(churn, aes(churn$MonthlyCharges,fill=factor(churn$Churn))) + geom_histogram()
# We can see that customers paying in the price band of 65-105 are churning the most than people paying lower value bills.

# *** # Looking at the total charges, lets see who churns more
ggplot(churn, aes(churn$TotalCharges,fill=factor(churn$Churn))) + geom_histogram()
# Not so surprising, but people who have in all paid very less are only churning. This is in accordance to the tenure pattern we saw earlier.
# Lower the tenure -> lower the charges paid -> higher the churn 
# maybe later, while developing the model we need to see if this shows any multi-collinearity pattern

################### Checkpoint -3: Data Preparation ######################

# Make Box plots for numeric variables to look for outliers. 
# there are 3 numeric variables:
# 1. Tenure 
boxplot(churn$tenure)
quantile(churn$tenure, seq(0,1,0.01))
# no outliers found

#2. Monthly charges 
boxplot(churn$MonthlyCharges)
quantile(churn$MonthlyCharges, seq(0,1,0.01))
# no outliers found

#3. Total charges 
boxplot(churn$TotalCharges)
quantile(churn$TotalCharges, seq(0,1,0.01))
# no outliers found

# From these simple box plots and observing the quantile results, we see there are no outliers

# Perform De-Duplication if required
sum(duplicated(churn))
# 0 - no duplicates 
# also, from the structure of the data we see there are no synonym or homonym problems

# Bring the variables in the correct format
#convert the senior citizen column to a factor
table(churn$SeniorCitizen)
churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

# Impute the missing values, and perform the outlier treatment (if required).
# look for NA values 
sum(is.na(churn))
# There are 11 NA values
#which column has all the NA values?
names(which(colSums(is.na(churn))>0))
sum(is.na(churn$TotalCharges))

## all the NA values are in the total charges column.,
# In This case let us impute the missing values with mean of the column.
churn$TotalCharges[which(is.na(churn$TotalCharges))] <- mean(churn$TotalCharges, na.rm = TRUE)
sum(is.na(churn$TotalCharges))


# Feature transformation
# We do not have any usecase of doing any feature extraction. looks like we can work in the same granularity.
# however, we have a lot of factor variables for which we need to create dummy variables.
str(churn)

# for conract variable 
d1 <- data.frame(model.matrix(~Contract -1, data = churn) [ ,-1])
colnames(churn)
churn<-cbind(churn[,-8], d1)
str(churn)

# for payment Method
d2 <- data.frame(model.matrix(~PaymentMethod-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-9], d2)
str(churn)

# MultipleLines
d3 <- data.frame(model.matrix(~MultipleLines-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d3)
str(churn)

# InternetService
d4 <- data.frame(model.matrix(~InternetService-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d4)
str(churn)

# OnlineSecurity
d5 <- data.frame(model.matrix(~OnlineSecurity-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d5)
str(churn)

# OnlineBackup
d6 <- data.frame(model.matrix(~OnlineBackup-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d6)
str(churn)

# DeviceProtection
d7 <- data.frame(model.matrix(~DeviceProtection-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d7)
str(churn)

# TechSupport
d8 <- data.frame(model.matrix(~TechSupport-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d8)
str(churn)

# StreamingTV
d9 <- data.frame(model.matrix(~StreamingTV-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d9)
str(churn)

# StreamingMovies
d10 <- data.frame(model.matrix(~StreamingMovies-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-12], d10)
str(churn)

# gender
d11 <- data.frame(model.matrix(~gender-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-2], d11)
str(churn)

# Partner
d12 <- data.frame(model.matrix(~Partner-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-3], d12)
str(churn)

# SeniorCitizen
d13 <- data.frame(model.matrix(~SeniorCitizen-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-2], d13)
str(churn)

# Dependents
d14 <- data.frame(model.matrix(~Dependents-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-2], d14)
str(churn)

# PhoneService
d15 <- data.frame(model.matrix(~PhoneService-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-3], d15)
str(churn)

# PaperlessBilling
d16 <- data.frame(model.matrix(~PaperlessBilling-1, data = churn)[,-1])
colnames(churn)
churn<-cbind(churn[,-3], d16)
str(churn)

## Since there are 32variables, we need not perform PCA for reducing the variables, we can hanldle this while model creation

# now but for churn (response variable) and customer id(primary key), all the other variables are converted to numeric.
## we can ignore the customer id field, as it is not really a predictor variable .
churn <- churn[,-1]
masterData_final <- churn

## we will have to standardise the data by scaling them for model building. 
# we will have to ignore the class variable for this..
masterData_scaled <- scale(masterData_final[,-4])
str(masterData_scaled)

#this enables us to move towards model building.
## Now we need to split the data into test and train data 
table(masterData_final$Churn)
#  we'll use the sample.split() function from the caTools package 
#to ensure the same proportion of the class variable in test and train data set
set.seed(100)
split_indices <- sample.split(masterData_final$Churn,SplitRatio = .7)
train <-masterData_final[split_indices==TRUE,]
test <-masterData_final[split_indices==FALSE,]

# Bring the data in the correct format for use in different models .
str(train)
str(test)
train_class_lbls <- train$Churn
test_class_lbls <- test$Churn

########################  Checkpoint 4: Model Building   ################################

######## K-NN Model: ###############
# syntax is :  knn(train data, test data, class labels of train data, k = 1, prob=TRUE)

# Implement the K-NN model for optimal K.
# we shall use the cross validation method to achieve this 
knn_model <- train(Churn~., data=train, method="knn",
                   tuneGrid=expand.grid(.k=1:50),
                   metric="Accuracy",
                   trControl=trainControl(method = 'repeatedcv', number = 10, repeats = 15))
knn_model
plot(knn_model)

# k-Nearest Neighbors 
# 
# 4930 samples
# 30 predictor
# 2 classes: 'No', 'Yes' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 15 times) 
# Summary of sample sizes: 4438, 4437, 4438, 4437, 4436, 4437, ... 
# Resampling results across tuning parameters:
#   
#   k   Accuracy   Kappa    
# 1  0.7245053  0.3020719
# 2  0.7206644  0.2949584
# 3  0.7480503  0.3439426
# 4  0.7498362  0.3509050
# 5  0.7661152  0.3817570
# 6  0.7656028  0.3830244
# 7  0.7767573  0.4088455
# 8  0.7734032  0.4011974
# 9  0.7777312  0.4117935
# 10  0.7784617  0.4146606
# 11  0.7795976  0.4177575
# 12  0.7804373  0.4198939
# 13  0.7811525  0.4221951
# 14  0.7832345  0.4279875
# 15  0.7871299  0.4371762
# 16  0.7865473  0.4362447
# 17  0.7888052  0.4425863
# 18  0.7888874  0.4427900
# 19  0.7896304  0.4451060
# 20  0.7896708  0.4447959
# 21  0.7914838  0.4497820
# 22  0.7915251  0.4498804
# 23  0.7929444  0.4534533
# 24  0.7934718  0.4548166
# 25  0.7934719  0.4547424
# 26  0.7936884  0.4551074
# 27  0.7950944  0.4591503
# 28  0.7952714  0.4594589
# 29  0.7967853  0.4638380
# 30  0.7966633  0.4635214
# 31  0.7967985  0.4640946
# 32  0.7967984  0.4639752
# 33  0.7972446  0.4655863
# 34  0.7967303  0.4642561
# 35  0.7975418  0.4662523
# 36  0.7974469  0.4663102
# 37  0.7969335  0.4651468
# 38  0.7967037  0.4644446
# 39  0.7970009  0.4651999
# 40  0.7959063  0.4621639
# 41  0.7961090  0.4629143
# 42  0.7958926  0.4621630
# 43  0.7956091  0.4617549
# 44  0.7956628  0.4619592
# 45  0.7955547  0.4616521
# 46  0.7957711  0.4623677
# 47  0.7955680  0.4622111
# 48  0.7955271  0.4619630
# 49  0.7957298  0.4631582
# 50  0.7956631  0.4631762
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was *** k = 35. **** 

# so let us create a model for k=35
library(class)
knn_35 <- knn(train[,-4], test[,-4], train_class_lbls, k=35, prob=TRUE)
confusionMatrix(knn_35, test_class_lbls, positive = "Yes")

## Final metrics of our model :
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  1377  262
# Yes  175  299
# 
#             Accuracy    : 0.7932          
#             Sensitivity : 0.5330          
#             Specificity : 0.8872          
#                                           
#        'Positive' Class : Yes             

## looking at the model performance
knn_pred <- prediction(attr(knn_35,"prob"), test_class_lbls)
knn_perf <- performance(knn_pred,"tpr", "fpr")
plot(knn_perf, color="black", lty=3, ltx=3)

# area under the curve 
knn_auc <- performance(knn_pred,"auc")
# 0.2513972  

############# Naive Bayes Model: ##################
library(klaR)
nb_model <- NaiveBayes(Churn~., data = train)

nb_predict <- predict(nb_model,test[,-4], type="raw")

confusionMatrix(nb_predict$class,test_class_lbls)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  No Yes
# No  950  77
# Yes 602 484
# 
# Accuracy : 0.6787          
# 95% CI : (0.6583, 0.6985)
# No Information Rate : 0.7345          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.3656          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.6121          
# Specificity : 0.8627          
# Pos Pred Value : 0.9250          
# Neg Pred Value : 0.4457          
# Prevalence : 0.7345          
# Detection Rate : 0.4496          
# Detection Prevalence : 0.4860          
# Balanced Accuracy : 0.7374          
# 
# 'Positive' Class : No      

# convert to a data frame
nb_predict <- as.data.frame(nb_predict)
str(nb_predict)

## choose only the class column
predprob <- nb_predict[,1]
realvec <- ifelse(test$Churn=="Yes", 1, 0)
predprob <- ifelse(predprob=="Yes", 1,0)

# get the prediction object
nb_pred_obj  <- prediction(predprob, realvec)
perf_nb <- performance(nb_pred_obj, "tpr", "fpr")
# plot the AUC curve
plot(perf_nb)
# area under the curve 
nb_auc <- performance(nb_pred_obj,"auc")
nb_auc
# auc - 0.7374293


############# Logistic Regression: ###############

model_1 <- glm(Churn~.,family = binomial,data=train)
# Using the stepAIC function to filter the  variables for model building
step <- stepAIC(model_1,direction = "both")
summary(step)

step
model_2 <-  glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesNo.phone.service + MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo + OnlineBackupYes + DeviceProtectionYes + 
                  StreamingTVYes + StreamingMoviesYes + model.matrix..SeniorCitizen...1..data...churn.....1. + 
                  model.matrix..PaperlessBilling...1..data...churn.....1., 
                family = binomial, data = train)
summary(model_2)
vif(model_2)

# eliminating MultipleLinesNo.phone.service based on the significance level
model_3 <-  glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                   MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo + OnlineBackupYes + DeviceProtectionYes + 
                  StreamingTVYes + StreamingMoviesYes + model.matrix..SeniorCitizen...1..data...churn.....1. + 
                  model.matrix..PaperlessBilling...1..data...churn.....1., 
                family = binomial, data = train)
summary(model_3)
vif(model_3)

# eliminating OnlineBackupYes based on significance level
model_4 <-  glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo +  DeviceProtectionYes + 
                  StreamingTVYes + StreamingMoviesYes + model.matrix..SeniorCitizen...1..data...churn.....1. + 
                  model.matrix..PaperlessBilling...1..data...churn.....1., 
                family = binomial, data = train)
summary(model_4)
vif(model_4)

# eliminating DeviceProtectionYes based on significance level
model_5 <-  glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo +   
                  StreamingTVYes + StreamingMoviesYes + model.matrix..SeniorCitizen...1..data...churn.....1. + 
                  model.matrix..PaperlessBilling...1..data...churn.....1., 
                family = binomial, data = train)
summary(model_5)
vif(model_5)

# eliminating model.matrix..SeniorCitizen...1..data...churn.....1. based on significance level
model_6 <-  glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo +   StreamingTVYes + StreamingMoviesYes +  
                  model.matrix..PaperlessBilling...1..data...churn.....1., 
                family = binomial, data = train)
summary(model_6)
vif(model_6)

# we see that monthly and total charges might be corelated.
cor(train$MonthlyCharges, train$TotalCharges)
cor(train$tenure, train$TotalCharges)
## 0.825 
cor(train$tenure, train$MonthlyCharges)

## we need to eliminate the TotalCharges variable as it is strongly coorelated to tenure and has a higher P-Value.
# eliminating TotalCharges based on P-value
model_7 <-  glm(formula = Churn ~ tenure + MonthlyCharges +  
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo +   StreamingTVYes + StreamingMoviesYes +  
                  model.matrix..PaperlessBilling...1..data...churn.....1., 
                family = binomial, data = train)
summary(model_7)
vif(model_7)

## we see that paperless billing does not have any sense in the model and has the highest P-value
model_8 <-  glm(formula = Churn ~ tenure + MonthlyCharges +  
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + 
                  InternetServiceNo +   StreamingTVYes + StreamingMoviesYes , 
                family = binomial, data = train)
summary(model_8)
vif(model_8)

# if we consider the streaming services as allied/optional services and since not many opt for it, we can eliminate them to 
# improve precision of our model.
model_9 <-  glm(formula = Churn ~ tenure + MonthlyCharges +  
                  ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + InternetServiceNo  , 
                family = binomial, data = train)
summary(model_9)
vif(model_9)

# We see that monthlycharges variable is still got higher VIF value while, it became insignificant after 
# we removed  consideration of optional streaming services.
cor(train$StreamingMoviesYes, train$MonthlyCharges)
cor(train$StreamingTVYes, train$MonthlyCharges)
# we see a very strong correlation amongst the monthly charges are the streaming. Which can be interpretes as,
# higher monthly charges due to opting for the streaming services.
model_10 <-  glm(formula = Churn ~ tenure + ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check + 
                  MultipleLinesYes + InternetServiceFiber.optic + InternetServiceNo  , 
                family = binomial, data = train)
summary(model_10)
vif(model_10)

# So we choose model_10 as our final model.
final_model_glm <- model_10

# c-statistic and KS -statistic
# Calculating C-statistic for the train data set
train$predicted_prob_glm <- predict(final_model_glm,type = "response")
rcorr.cens(train$predicted_prob_glm,train$Churn) 
#C-train - 83.80%

# Calculating C-statistic for the test data set
test$predicted_prob_glm = predict(final_model_glm, newdata = test,type = "response")
rcorr.cens(test$predicted_prob_glm,test$Churn)
#Ctest- 83.49%

# Calculating the KS-statistic for the train data set
model_score <- prediction(train$predicted_prob_glm,train$Churn)
model_perf <- performance(model_score, "tpr", "fpr")
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
ks = max(ks_table)
ks
# 0.5310
which(ks_table == ks)
# 311
nrow(train)
# 4930
#311/4930 = 0.0630 => 1st decile. 


# Calculating the K-statistic for the test data set
model_score_test <- prediction(test$predicted_prob_glm,test$Churn)
model_perf_test <- performance(model_score_test, "tpr", "fpr")
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test <- max(ks_table_test)
ks_test
# 0.5307
which(ks_table_test == ks_test)
#209
nrow(test)
#209/2113 = 0.0989 => 1st decile. Which is great performance.

# Selecting threshold value
# Plot the ROC curve
plot(model_perf,col = "red", lab = c(10,10,10))
# From the above ROC curve we can can see that the threshold value of 0.5 is appropriate
auc <- performance(model_score_test, "auc")
#0.8349786

# Generating the confusion matrix for the train data set
levels(train$Churn)
levels(train$Churn) <- c(0,1)
confusionMatrix(as.numeric(train$predicted_prob_glm > 0.20),train$Churn, positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2378  209
# 1 1244 1099
# 
# Accuracy : 0.7053         
# 95% CI : (0.6923, 0.718)
# No Information Rate : 0.7347         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.3965         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.8402         
# Specificity : 0.6565         
# Pos Pred Value : 0.4691         
# Neg Pred Value : 0.9192         
# Prevalence : 0.2653         
# Detection Rate : 0.2229         
# Detection Prevalence : 0.4753         
# Balanced Accuracy : 0.7484         
# 
# 'Positive' Class : 1     

# we have chosen a value of 0.20 as threshold as we want to increase our TPR (identify most of the customers who churn), to avoid risks.
# This means we will have to minimize False Negative rate, ie. choose lower thresholds.
## NOTE: We are looking at a sensitivity of 80% atleast for this model. Hence the threshold.
levels(test$Churn) <- c(0,1)
# Generating the confusion matrix for the test data set
confusionMatrix(as.numeric(test$predicted_prob_glm > 0.20),test$Churn, positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 1012   99
# 1  540  462
# 
# Accuracy : 0.6976          
# 95% CI : (0.6775, 0.7171)
# No Information Rate : 0.7345          
# P-Value [Acc > NIR] : 0.9999          
# 
# Kappa : 0.3802          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.8235          
# Specificity : 0.6521          
# Pos Pred Value : 0.4611          
# Neg Pred Value : 0.9109          
# Prevalence : 0.2655          
# Detection Rate : 0.2186          
# Detection Prevalence : 0.4742          
# Balanced Accuracy : 0.7378          
# 
# 'Positive' Class : 1  

########### SVM: ##############
tune.svm <- tune(svm,Churn~.,data=train, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,0.5,1,10,100)))
summary(tune.svm)
model_svm <- tune.svm$best.model
summary(model_svm)
# Call:
#   best.tune(method = svm, train.x = Churn ~ ., data = train, ranges = list(cost = c(0.001, 0.01, 0.1, 
#                                                                                     0.5, 1, 10, 100)), kernel = "linear")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.1 
# gamma:  0.03333333 
# 
# Number of Support Vectors:  2275
# 
# ( 1141 1134 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   No Yes

svm_pred <- predict(model_svm, test)
confusionMatrix(svm_pred, test$Churn)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  1397  272
# Yes  155  289
# 
# Accuracy : 0.7979          
# 95% CI : (0.7802, 0.8149)
# No Information Rate : 0.7345          
# P-Value [Acc > NIR] : 6.718e-12       
# 
# Kappa : 0.4449          
# Mcnemar's Test P-Value : 1.981e-08       
#                                           
#             Sensitivity : 0.9001          
#             Specificity : 0.5152          
#          Pos Pred Value : 0.8370          
#          Neg Pred Value : 0.6509          
#              Prevalence : 0.7345          
#          Detection Rate : 0.6611          
#    Detection Prevalence : 0.7899          
#       Balanced Accuracy : 0.7076          
#                                           
#        'Positive' Class : No            

str(svm_pred)
svm_pred <- ifelse(svm_pred=="Yes",1,0)
test_class_lbls_bin <- ifelse(test_class_lbls=="Yes",1,0) 
svm_pred_obj  <- prediction(svm_pred, test_class_lbls_bin)
perf_svm <- performance(svm_pred_obj, "tpr", "fpr")
# plot the AUC curve
plot(perf_svm)
# area under the curve 
svm_auc <- performance(svm_pred_obj,"auc")
svm_auc
#0.7001879

