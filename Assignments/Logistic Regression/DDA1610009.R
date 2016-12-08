# setwd("C:/Users/nsrinivn/Desktop/PGDDA/PGDDA/study/assignments/course-3/logistic regression")

# Loading required packages
library("car")
library("caTools")
library("MASS")
library("ROCR")
library("caret")
library("Hmisc")
library("ggplot2")  

###########chkeckpoint -1###############
# Download the data set as german_credit
# Since there are many Factor type of variables we will set stringsAsFactors as TRUE
german_credit <- read.csv("german.csv",stringsAsFactors = TRUE)

#explore the data 
str(german_credit)
summary(german_credit)

# Univariate plots
# The below plot shows the distribution of various Purposes for which the loan was taken.
# top reasons why loans are taken : A40(new car), A42(furniture/equipment) and A43(radio/television)
ggplot(german_credit,aes(Purpose))+geom_bar()

# Most of the loans are taken for the property type A123(car or other)
ggplot(german_credit,aes(Property))+geom_bar()

# The distribution of various Age groups of people who have taken the loans. 
# Most of the loans are taken within the age group 25-40.
ggplot(german_credit,aes(Age.in.Years))+geom_histogram(binwidth = 5)

# The distribution of housing status of people who have taken the loans. 
# Most of the loans are taken by the people with own houses.
ggplot(german_credit,aes(Housing.))+geom_bar()

# The  distribution of foreign status of personal who have taken the loans. 
# Most of the loans are taken by the people who are foreign workers.
ggplot(german_credit,aes(foreign.worker))+geom_bar()


################ Checkpoint 2 ######################
# Data prepartion and feature transformation
# No  NA values in the dataset
nrow(german_credit)
ncol(german_credit)
sum(is.na(german_credit))

# Outlier treatment
quantile(german_credit$Duration.in.month, seq(0,1,0.01))
# We ca nsee a jump at 99%,hence capping to 48.00(98th percentile)
german_credit$Duration.in.month[which(german_credit$Duration.in.month>48.00)] <- 48.00

quantile(german_credit$Credit.amount, seq(0,1,0.01))
# We ca nsee a jump at 98%,hence capping to  12169.70(98th percentile)
german_credit$Credit.amount[which(german_credit$Credit.amount>12169.70)] <- 12169.70

quantile(german_credit$Age.in.Years, seq(0,1,0.01))
# We ca nsee a jump at 100%,hence capping to 67(99th percentile)
german_credit$Age.in.Years[which(german_credit$Age.in.Years>67)] <- 67

# Following variables are of interger type. But they are supposed to be factor type variables.
# Hence converting following variables to factors 
unique(german_credit$Installment.rate.in.percentage.of.disposable.income)
german_credit$Installment.rate.in.percentage.of.disposable.income <- as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income)

unique(german_credit$Present.residence.since)
german_credit$Present.residence.since <- as.factor(german_credit$Present.residence.since)

unique(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
german_credit$Number.of.people.being.liable.to.provide.maintenance.for. <- as.factor(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)

unique(german_credit$Number.of.existing.credits.at.this.bank.)
german_credit$Number.of.existing.credits.at.this.bank. <- as.factor(german_credit$Number.of.existing.credits.at.this.bank.)

# There are 17 variables which are of factor type for which we shall create dummy variables
levels(german_credit$Status.of.existing.checking.account)
dummy_1 <- model.matrix(~Status.of.existing.checking.account -1 , data = german_credit)
dummy_1 <- dummy_1[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-1], dummy_1)

levels(german_credit$Credit.history)
dummy_2 <- model.matrix(~Credit.history -1 , data = german_credit)
dummy_2 <- dummy_2[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-2], dummy_2)


levels(german_credit$Purpose)
dummy_3 <- model.matrix(~Purpose -1 , data = german_credit)
dummy_3 <- dummy_3[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-2], dummy_3)


levels(german_credit$Savings.account.bonds)
dummy_4 <- model.matrix(~Savings.account.bonds -1 , data = german_credit)
dummy_4 <- dummy_4[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_4)

levels(german_credit$Present.employment.since.)
dummy_5 <- model.matrix(~Present.employment.since. -1 , data = german_credit)
dummy_5 <- dummy_5[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_5)

levels(german_credit$Installment.rate.in.percentage.of.disposable.income)
dummy_6 <- model.matrix(~Installment.rate.in.percentage.of.disposable.income -1 , data = german_credit)
dummy_6 <- dummy_6[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_6)

levels(german_credit$Personal.status.and.sex)
dummy_7 <- model.matrix(~Personal.status.and.sex -1 , data = german_credit)
dummy_7 <- dummy_7[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_7)


levels(german_credit$Other.debtors...guarantors)
dummy_8 <- model.matrix(~Other.debtors...guarantors -1 , data = german_credit)
dummy_8 <- dummy_8[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_8)


levels(german_credit$Present.residence.since)
dummy_9 <- model.matrix(~Present.residence.since -1 , data = german_credit)
dummy_9 <- dummy_9[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_9)


levels(german_credit$Property)
dummy_10 <- model.matrix(~Property -1 , data = german_credit)
dummy_10 <- dummy_10[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-3], dummy_10)


levels(german_credit$Other.installment.plans)
dummy_11 <- model.matrix(~Other.installment.plans -1 , data = german_credit)
dummy_11 <- dummy_11[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_11)


levels(german_credit$Housing.)
dummy_12 <- model.matrix(~Housing. -1 , data = german_credit)
dummy_12 <- dummy_12[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_12)


levels(german_credit$Number.of.existing.credits.at.this.bank.)
dummy_13 <- model.matrix(~Number.of.existing.credits.at.this.bank. -1 , data = german_credit)
dummy_13 <- dummy_13[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_13)


levels(german_credit$Job_status)
dummy_14 <- model.matrix(~Job_status -1 , data = german_credit)
dummy_14 <- dummy_14[,-1]
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_14)


levels(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
dummy_15 <-data.frame(model.matrix(~ Number.of.people.being.liable.to.provide.maintenance.for. -1, data = german_credit) [ ,-1])
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_15)


levels(german_credit$Telephone.)
dummy_16 <-data.frame(model.matrix(~ Telephone. -1, data = german_credit) [ ,-1])
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_16)

levels(german_credit$foreign.worker)
dummy_17 <-data.frame(model.matrix(~ foreign.worker -1, data = german_credit) [ ,-1])
str(german_credit)
german_credit<-cbind(german_credit[,-4], dummy_17)


############# Checkpoint 3: #######
table(german_credit$Default_status)
# We can see that the different values for Default_status(0,1) is in the ratio 7:3.
# Thus, we'll use the sample.split() function from the caTools package 
#to ensure the same proportion of the class variable in test and train data set
set.seed(100)
split_indices <- sample.split(german_credit$Default_status,SplitRatio = .7)
train <-german_credit[split_indices==TRUE,]
test <-german_credit[split_indices==FALSE,]

############ Checkpoint 4 ##############

# Initial Model with all variables
# Creating the initial model with all the variables
model_1 <- glm(Default_status~.,family=binomial,data=train)

# Using the stepAIC function to filter the  variables for model building
step <- stepAIC(model_1,direction = "both")
summary(step)

# Now building our model with the result of the StepAIC function
model_2 <- glm(formula = Default_status ~ Duration.in.month + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + PurposeA45 + PurposeA49 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                 Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 + model.matrix..foreign.worker...1..data...german_credit.....1., 
               family = binomial, data = train)
summary(model_2)
# There are still many insignificant variables, lets chk VIF values
vif(model_2)

# VIF of all the variables is less than 3 except Credit.historyA32, but Credit.historyA32 has higher significance
# Other.debtors...guarantorsA103 is the least significant among all, hence omitting this
model_3 <- glm(formula = Default_status ~ Duration.in.month + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + PurposeA45 + PurposeA49 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 + model.matrix..foreign.worker...1..data...german_credit.....1., 
               family = binomial, data = train)
summary(model_3)
vif(model_3)

# Installment.rate.in.percentage.of.disposable.income3 is the least significant among all, hence omitting this
model_4 <- glm(formula = Default_status ~ Duration.in.month + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + PurposeA45 + PurposeA49 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 + model.matrix..foreign.worker...1..data...german_credit.....1., 
               family = binomial, data = train)
summary(model_4)
vif(model_4)

# PurposeA45 is the least significant among all, hence omitting this
model_5 <- glm(formula = Default_status ~ Duration.in.month + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + PurposeA49 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 + model.matrix..foreign.worker...1..data...german_credit.....1., 
               family = binomial, data = train)
summary(model_5)
vif(model_5)

# Age.in.Years is the least significant among all, hence omitting this
model_6 <- glm(formula = Default_status ~ Duration.in.month + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + PurposeA49 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 + model.matrix..foreign.worker...1..data...german_credit.....1., 
               family = binomial, data = train)
summary(model_6)
vif(model_6)

# PurposeA49 is the least significant among all, hence omitting this
model_7 <- glm(formula = Default_status ~ Duration.in.month + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 +
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 + model.matrix..foreign.worker...1..data...german_credit.....1., 
               family = binomial, data = train)
summary(model_7)
vif(model_7)

# model.matrix..foreign.worker...1..data...german_credit.....1. is the least significant among all, hence omitting this
model_8 <- glm(formula = Default_status ~ Duration.in.month + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 +
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 , 
               family = binomial, data = train)
summary(model_8)
vif(model_8)


# PurposeA42 is the least significant among all, hence omitting this
model_9 <- glm(formula = Default_status ~ Duration.in.month + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA43 +
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                 Housing.A152 , 
               family = binomial, data = train)
summary(model_9)
vif(model_9)

# PurposeA43 is the least significant among all, hence omitting this
model_10 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  PurposeA41 + Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                  Housing.A152 , 
                family = binomial, data = train)
summary(model_10)
vif(model_10)

# PurposeA41 is the least significant among all, hence omitting this
model_11 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                  Housing.A152 , 
                family = binomial, data = train)
summary(model_11)
vif(model_11)

# Present.employment.since.A74 is the least significant among all, hence omitting this
model_12 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Savings.account.bondsA64 + Savings.account.bondsA65 +
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                  Housing.A152 , 
                family = binomial, data = train)
summary(model_12)
vif(model_12)

# Savings.account.bondsA64 is the least significant among all, hence omitting this
model_13 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Savings.account.bondsA65 +
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                  Housing.A152 , 
                family = binomial, data = train)
summary(model_13)
vif(model_13)

# Savings.account.bondsA65 is the least significant among all, hence omitting this
model_14 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143 + 
                  Housing.A152 , 
                family = binomial, data = train)
summary(model_14)
vif(model_14)

# Housing.A152 is the least significant among all, hence omitting this
model_15 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2 + Other.installment.plansA143, 
                family = binomial, data = train)
summary(model_15)
vif(model_15)

# Other.installment.plansA143 is the least significant among all, hence omitting this
model_16 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93 + Present.residence.since2, 
                family = binomial, data = train)
summary(model_16)
vif(model_16)

# Present.residence.since2 is the least significant among all, hence omitting this
model_17 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93, 
                family = binomial, data = train)
summary(model_17)
vif(model_17)

# Status.of.existing.checking.accountA13 is the least significant among all, hence omitting this
model_18 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Personal.status.and.sexA93, 
                family = binomial, data = train)
summary(model_18)
vif(model_18)

# Personal.status.and.sexA93 is the least significant among all, hence omitting this
model_19 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  Installment.rate.in.percentage.of.disposable.income4, 
                family = binomial, data = train)
summary(model_19)
vif(model_19)

# Installment.rate.in.percentage.of.disposable.income4 is the least significant among all, hence omitting this
model_20 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34, 
                family = binomial, data = train)
summary(model_20)
vif(model_20)

# Credit.historyA33 is the least significant among all, hence omitting this
model_21 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA34, 
                family = binomial, data = train)
summary(model_21)
vif(model_21)

# Credit.historyA32 is the least significant among all, hence omitting this
model_22 <- glm(formula = Default_status ~ Duration.in.month + 
                  Status.of.existing.checking.accountA14 + Credit.historyA34, 
                family = binomial, data = train)
summary(model_22)
vif(model_22)


final_model <- model_22
summary(final_model)
# final_model details
# glm(formula = Default_status ~ Duration.in.month + Status.of.existing.checking.accountA14 + 
#               Credit.historyA34, family = binomial, data = train)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 726.40  on 696  degrees of freedom
# AIC: 734.4

########### Checkpoint 5 ############
# c-statistic and KS -statistic
# Calculating C-statistic for the train data set
train$predicted_prob <- predict(final_model,type = "response")
rcorr.cens(train$predicted_prob,train$Default_status) 
#C-train - 75.58%
# Calculating C-statistic for the test data set
test$predicted_prob = predict(final_model, newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$Default_status)
#Ctest- 73.18%

# Calculating the KS-statistic for the train data set
model_score <- prediction(train$predicted_prob,train$Default_status)
model_perf <- performance(model_score, "tpr", "fpr")
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
ks = max(ks_table)
ks
# 0.3952381
which(ks_table == ks)
#35/700 = 0.05= 1st decile


# Calculating the K-statistic for the test data set
model_score_test <- prediction(test$predicted_prob,test$Default_status)
model_perf_test <- performance(model_score_test, "tpr", "fpr")
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test <- max(ks_table_test)
ks_test
# 0.4095238
which(ks_table_test == ks_test)
 #27/300 = 0.09 = 1st decile

############# Checkpoint 6 ###############
# Selecting threshold value
# Plot the ROC curve
plot(model_perf,col = "red", lab = c(10,10,10))
# From the above ROC curve we can can see that the threshold value of 0.5 is appropriate

# Generating the confusion matrix for the train data set
confusionMatrix(as.numeric(train$predicted_prob > 0.5),train$Default_status, positive = "1")
# Generating the confusion matrix for the test data set
confusionMatrix(as.numeric(test$predicted_prob > 0.5),test$Default_status, positive = "1")

