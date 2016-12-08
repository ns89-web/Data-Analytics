##------------------------------------------------##
#         Checkpoint - 1                           #
##------------------------------------------------##
### go to the working directory
setwd("C:/Users/nsrinivn/Desktop/PGDDA/PGDDA/study/assignments/course-3/car-mlr")

## import the data set from the file.
carMpg <- read.csv("carMPG.csv", stringsAsFactors = FALSE)

## view and understand the imported data set 
str(carMpg)

##
# note that the variables like horsepower, and car_name are characters,
# which cannot be used for model building.
# Also, other variables like, MPG, Cylinders, Model Year, origin need to be converted to factors.
# We shall do these as part of data preparation.
##

## View the data 
View(carMpg)

##------------------------------------------------##
#               Checkpoint - 2                     #
##------------------------------------------------##

## Stage 1 - Variable formatting :
# convert cylinders as factor
carMpg$Cylinders <- as.factor(carMpg$Cylinders)
str(carMpg)

# Note that there are 5 levels, we will create dummy variables in the next stage


# we can treat the model_year variable as a factor
carMpg$Model_year <-as.factor(carMpg$Model_year)
str(carMpg)
levels(carMpg$Model_year)

# note that there are 13 levels of this factor variable, we can reduce this to 3 categories 
# in blocks of 5 years. This will also be done in the next stage of the Data preparation.

carMpg$Origin <- as.factor(carMpg$Origin)
str(carMpg$Origin)
# there are 3 levels in origin, which we shall create dummy variables for in the next stage...

# the horsepower variable is a chr format, we need to convert this to numeric, as it is a continuous variable,
carMpg$Horsepower <- as.numeric(carMpg$Horsepower)
str(carMpg)
# Also, the other variables like Displacement, Weight, Acceleration are continuous variables,
# so, we shall retain them in the same format for building our model.


### Stage-2 : Data Cleaning
## check for duplicates 
sum(duplicated(carMpg))
# (or 
unique(carMpg)
# We see that there are no duplicates

## Next,check for NA values.
sum(is.na(carMpg))
sum(is.na(carMpg$Horsepower))
# There are 6 NA values in the dataset and all of them in the Horsepower variable.
# we can choose to remove the na values as it is only 1% of the data set; instead of imputing
# eliminating NA values
carMpg$Horsepower[which(is.na(carMpg$Horsepower))] <- mean(carMpg$Horsepower, na.rm = TRUE)
#carMpg[] <- carMpg[-which(is.na(carMpg$Horsepower)),]
str(carMpg)
# we now see that there are 392 observations, ie the six entries with NA value is removed.

## Check for Outliers in the continuous variables and treat them

# frist let us treat the independent variables.
quantile(carMpg$Displacement, seq(0,1,0.01))
# we see that there are no  major jumps,  and no outliers in this variable

# next the horsepower variable
quantile(carMpg$Horsepower,seq(0,1,0.01))
# at the higher end at 98-99%  ; capping to 210.00
carMpg$Horsepower[which(carMpg$Horsepower>210.00)] <- 210.00

# for the weight variable
quantile(carMpg$Weight,seq(0,1,0.01))
# no outliers 

# for acceleration variable
quantile(carMpg$Acceleration,seq(0,1,0.01))
# here there is a jump at lower end at 0-1%; flooring values at 9.485
carMpg$Acceleration[which(carMpg$Acceleration<9.485)] <-9.485
# next jump is at the higher end at 99-100%; capping values at 22.239
carMpg$Acceleration[which(carMpg$Acceleration>22.239)] <- 22.239

## backup the dataset  - temp safetynet
carMpg_bkp <- carMpg

## stage3: variable transformation
# dummy variables need to be created for Cylinders, Model_year, Origin and car_name
str(carMpg$Cylinders)

# we shall create 4 dummy variables for this as it has 5levels
dummy_cylinders <- model.matrix(~Cylinders -1 , data = carMpg)
# remove the first column as 4 columns are sufficient to depict all these variables.
dummy_cylinders <- dummy_cylinders[,-1]

# bind this to carMpg, removing the original 
carMpg_1 <- cbind(carMpg[,-2], dummy_cylinders)
View(carMpg_1)

# transform the model_year variable into 3 categories of 5years from 2000; using equal binning
# 2001-2005 > y1
# 2006-2010 > y2
# 2011-2015 > y3
levels(carMpg_1$Model_year)[1:3] <- "y1"
levels(carMpg_1$Model_year)
levels(carMpg_1$Model_year)[2:6] <- "y2"
levels(carMpg_1$Model_year)
levels(carMpg_1$Model_year)[3:7] <- "y3"

# next create dummy variables for this model_year
dummy_modelYear <- model.matrix(~Model_year -1,data = carMpg_1)
dummy_modelYear <- dummy_modelYear[,-1]

# bind this to the data set
carMpg_2 <- cbind(carMpg_1[,-6],dummy_modelYear)
str(carMpg_2)

# transform the origin variable
dummy_origin <- model.matrix(~Origin-1, data = carMpg_2)[,-1]
carMpg_3 <- cbind(carMpg_2[,-6],dummy_origin)
str(carMpg_3)

# only car name needs to be transformed now; we will make it a factor based on the make of the car.
# we will use the str_split_fixed function from Stringr package
library(stringr)
car_make <- as.data.frame(str_split_fixed(carMpg_3$Car_Name," ",2))
str(car_make)
colnames(car_make) <- c("make", "model")
# now the first column of this car_make is what we need; which has 37 levels of factor.
unique(car_make$make)

# here we can see some data quality issues like : incorrect spellings fo VW 
# replace VW, vokswagen and volkswagen
levels(car_make$make)[37] <- "volkswagen"
levels(car_make$make)[34] <- "volkswagen"

# chevy, chevrolet, chevroelt
levels(car_make$make)
levels(car_make$make)[7] <- "chevrolet"
levels(car_make$make)[8] <- "chevrolet"

#maxda to mazda
levels(car_make$make)[15] <- "mazda"

# mercedes and mercedes-benz
levels(car_make$make)
levels(car_make$make)[16] <-"mercedes-benz"

#toyouta to toyota
levels(car_make$make)[28]<- "totyota"

unique(car_make$make)

# now there are only 31 levels in this variable.we can bind the first column of car_make to our prepared dataset.
car_make <- car_make[,-2]
carMpg_4 <- cbind(carMpg_3[,-6],car_make)
str(carMpg_4)

# we see that the str is as expected. Now we can create dummy variables and transform the car_make 
# into numeric variable by making dummy variables for it.
car_make_dummy <- model.matrix(~car_make -1, data = carMpg_4)[,-1]
View(car_make_dummy)
ncol(car_make_dummy) # this is 30 so, this is the dummy cols we need.

# now bind the dummy cols back to the main dataset.
carMpg_5 <- cbind(carMpg_4[,-14],car_make_dummy)
str(carMpg_5)

# Now this carMpg_5 will be our final dataset which will be used for model building.
# let us split it into test and train dataset in 30:70 ratio,respectively
set.seed(100)
indices= sample(1:nrow(carMpg_5), 0.7*nrow(carMpg_5))

train=carMpg_5[indices,]
test = carMpg_5[-indices,]

nrow(test)
nrow(train)

##------------------------------------------------##
#               Checkpoint - 3                     #
##------------------------------------------------##

# since there are 43 variables in the dataset, lets build the first model
model_1<-lm(MPG~., data=train)
summary(model_1)

# we see that there are a lot of insignificant variable, note Adjusted R-squared:  0.8789 
# looks like  model_year2,3, weight  seem most significat of all for now.

# let us first perform stepwise reduction to arrive at the initial model.
library(MASS)
step<- stepAIC(model_1, direction = "both")

# look at the step variable to check the model to be built after application of stepwise function,
#  thereby removing the extremely insignificant variables.
step

#store the call used by step function as our model to now proceed with development
model_2 <- lm(formula = MPG ~ Horsepower + Weight + Cylinders4 + Cylinders5 + 
                Cylinders6 + Cylinders8 + Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makemazda + car_makeoldsmobile + 
                car_makepontiac + car_makerenault, data = train)

summary(model_2)

# now Rsquared value has increased - Adjusted R-squared:  0.886, 
# Next, we will apply the VIF function to check the multicollinearity of the independent variables and remove the variables
# with VIF>2 in order of their insignificance.
library(car)

vif(model_2)

# cylinders8,cylinders4,cylinders6 has highest vif but its pretty significant,
# lets check for correlations
cor(carMpg_5$Horsepower, carMpg_5$Weight)
#0.86676 
model_3 <- lm(formula = MPG ~  Weight + Cylinders4 + Cylinders5 + 
                Cylinders6 + Cylinders8 + Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makemazda + car_makeoldsmobile + 
                car_makepontiac + car_makerenault, data = train)

summary(model_3)
vif(model_3)

cor(carMpg_5$Cylinders4,carMpg_5$Cylinders8)
cor(carMpg_5$Cylinders6,carMpg_5$Cylinders8)
cor(carMpg_5$Cylinders4,carMpg_5$Cylinders6)

# remove cylinders4
model_4 <- lm(formula = MPG ~  Weight +  Cylinders5 + 
                Cylinders6 + Cylinders8 + Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makemazda + car_makeoldsmobile + 
                car_makepontiac + car_makerenault, data = train)

summary(model_4)
vif(model_4)

# remove cylinders8
model_5 <- lm(formula = MPG ~  Weight +  Cylinders5 + 
                Cylinders6 +  Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makemazda + car_makeoldsmobile + 
                car_makepontiac + car_makerenault, data = train)

summary(model_5)
vif(model_5)

# now all vif values are below 2. Let us remove based on p-value.
# remove carmake_mazda
model_6 <- lm(formula = MPG ~  Weight +  Cylinders5 + 
                Cylinders6 +  Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makeoldsmobile + 
                car_makepontiac + car_makerenault, data = train)

summary(model_6)
vif(model_6)

#remove car_makerenault
model_7 <- lm(formula = MPG ~  Weight +  Cylinders5 + 
                Cylinders6 +  Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makeoldsmobile + 
                car_makepontiac , data = train)

summary(model_7)
vif(model_7)

#remove cylinders5
model_8 <- lm(formula = MPG ~  Weight + Cylinders6 +  Model_yeary2 + Model_yeary3 + car_makecadillac + 
                car_makedatsun + car_makefiat + car_makeoldsmobile + 
                car_makepontiac , data = train)

summary(model_8)
vif(model_8)

#remove car_makecadillac
model_9 <- lm(formula = MPG ~  Weight + Cylinders6 +  Model_yeary2 + Model_yeary3 +
                car_makedatsun + car_makefiat + car_makeoldsmobile + 
                car_makepontiac , data = train)

summary(model_9)
vif(model_9)

#remove car_makefiat
model_10 <- lm(formula = MPG ~  Weight + Cylinders6 +  Model_yeary2 + Model_yeary3 +
                car_makedatsun +  car_makeoldsmobile + 
                car_makepontiac , data = train)

summary(model_10)
vif(model_10)

#remove car_makeoldsmobile
model_11 <- lm(formula = MPG ~  Weight + Cylinders6 +  Model_yeary2 + Model_yeary3 +
                 car_makedatsun +  car_makepontiac , data = train)

summary(model_11)
vif(model_11)

#remove car_makepontiac
model_12 <- lm(formula = MPG ~  Weight + Cylinders6 +  Model_yeary2 + Model_yeary3 +
                 car_makedatsun  , data = train)
summary(model_12)
vif(model_12)

# Now we see that the VIF values have reduced significantly for all while,
# the Adj. Rsq value has not dropped much. So, model_12 will be our final model with Adjusted R-squared:  0.8336!

##------------------------------------------------##
#               Checkpoint - 4                     #
##------------------------------------------------##

# lets apply the model to the test data 
prediction <- predict(model_12,test[,-1])

# now finding the r-squared value of the predicted values
rsq <- (cor(test$MPG,prediction))^2
rsq
# 0.8324436 --> which is more than 80% accurate (uff!) (nice assignment, enjoyed it!)

##------------------------------------------------##
#               Checkpoint - 5                     #
##------------------------------------------------##

# now since the model satisfies all the three criteria, we can Accept the model!


## plots for the results:
library(ggplot2)
plot(test$MPG,prediction)


# residual plots
par(mar = rep(2, 4))
par ( mfrow =c(2 ,2) )
plot(model_12)


