setwd("C:\\Users\\nsrinivn\\Desktop\\PGDDA\\PGDDA\\study\\assignments\\course-2\\casestudy")

#read the csv files
loan <- read.csv("loan.csv", stringsAsFactors = F)
# understand the data
str(loan)

############# checkpoint-1 ###################
###impute NA values
# On exploring , we see there are 7 rows with NA values in all columns, lets remove them
loan<-loan[-which(is.na(loan$loan_amnt)),]

#further treat every column for NA values one-by-one
loan[which(is.na(loan$annual_inc)), "annual_inc"] <- mean(loan$annual_inc, na.rm = T)
loan[which(is.na(loan$loan_amnt)),"loan_amnt"] <- mean(loan$loan_amnt, na.rm = T)
loan[which(is.na(loan$funded_amnt)),"funded_amnt"] <- mean(loan$funded_amnt, na.rm = T)
loan[which(is.na(loan$dti)),"dti"] <- mean(loan$dti, na.rm = T)

# int_rate variable needs to be cleaned for % character and convert the type to numeric
loan$int_rate_org <- loan$int_rate
loan$int_rate <- sub("%","",loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)
str(loan$int_rate)

#treat grade variable as a factor.
loan$grade <- factor(loan$grade)
summary(loan$grade)

# treat the emp_length with the mode value
loan$emp_length <- as.factor(loan$emp_length)
summary(loan$emp_length)
loan$emp_length[loan$emp_length %in% c("n/a")]<-names(which.max(summary(loan$emp_length)))

# convert some of the categorical variables as a factors
loan$purpose <- factor(loan$purpose)
summary(loan$purpose)

loan$home_ownership <- factor(loan$home_ownership)
summary(loan$home_ownership)

loan$loan_status <- factor(loan$loan_status)
summary(loan$loan_status)

###remove fully paid loan details
loan_org <- loan ## backup
loan <- loan[-which(loan$loan_status %in% c("Fully Paid")),]

### create new column loan_status_1
loan$loan_status_1[loan$loan_status %in% c("Current","In Grace Period")] <- "current_new"
loan$loan_status_1[loan$loan_status %in% c("Default", "Charged Off")]<- "default_new"
loan$loan_status_1[loan$loan_status %in% c("Late (16-30 days)","Late (31-120 days)")]<-"late"

### create new column int_rate_grp
loan$int_rate_grp[loan$int_rate < 10] <- "Low"
loan$int_rate_grp[loan$int_rate >= 10 & loan$int_rate <=18] <- "Medium"
loan$int_rate_grp[loan$int_rate > 18] <- "High"

### create new column emp_length_grp , after treating emp_length
loan$emp_length <- sub("[a-z].*","", loan$emp_length)
loan$emp_length <- gsub("\\W","", loan$emp_length)
loan$emp_length <- as.numeric(loan$emp_length)

loan$emp_length_grp[loan$emp_length<=4]<- "Junior"
loan$emp_length_grp[loan$emp_length>=5 & loan$emp_length<=8]<- "Mid-level"
loan$emp_length_grp[loan$emp_length>8]<- "Senior"

############ checkpoint-2 ################
## univariate analysis
summary(loan$annual_inc)
quantile(loan$annual_inc)
# we see that the mean and median are not nearly the same. The mean is on the right side of the median.
# so, there is a possibility of the distribution to be right skewed.
# lets treat outliers as the max and mean have no releavance
outliers <- boxplot.stats(loan$annual_inc)
#remove outliers from the dataframe
loan <- loan[-which(loan$annual_inc %in% outliers$out),]
summary(loan$annual_inc)
quantile(loan$annual_inc)

boxplot(loan$annual_inc)
#we still see that there are some outliers , however we decide to keep them
library(ggplot2)
ggplot(loan, aes(annual_inc))+geom_histogram(binwidth = 2000)
qqnorm(loan$annual_inc)
qqline(loan$annual_inc, col='red')


# goto next variable
summary(loan$loan_amnt)
boxplot(loan$loan_amnt)

# we see there are outliers which should be removed.
outliers<-boxplot.stats(loan$loan_amnt)
loan<- loan[-which(loan$loan_amnt %in% outliers$out),]
boxplot(loan$loan_amnt)
#we still see that there are some outliers , however we decide to keep them

ggplot(loan, aes(loan$loan_amnt)) + geom_histogram(binwidth = 500)
qqnorm(loan$loan_amnt)
qqline(loan$loan_amnt, col='red')

## next variable is funded amt
summary(loan$funded_amnt)
boxplot(loan$funded_amnt)
outliers<-boxplot.stats(loan$funded_amnt)
loan<- loan[-which(loan$funded_amnt %in% outliers$out)]
boxplot(loan$funded_amnt)
ggplot(loan, aes(funded_amnt)) + geom_histogram(binwidth = 500)

summary(loan$dti)
boxplot(loan$dti)
# no outlier treatement required. No plots required as this is a ratio.

##univariate for dimesion atributes
# for emp_length_grp
loan$emp_length_grp <- factor(loan$emp_length_grp)
summary(loan$emp_length_grp)
ggplot(loan, aes(emp_length_grp)) + geom_bar()

# chk bar charts to understand the distribution of the other variables.
loan$int_rate_grp <- factor(loan$int_rate_grp)
summary(loan$int_rate_grp)
ggplot(loan,aes(int_rate_grp)) + geom_bar()

summary(loan$grade)
ggplot(loan,aes(grade)) + geom_bar()

summary(loan$home_ownership)
ggplot(loan,aes(home_ownership)) + geom_bar()

loan$loan_status_1 <- factor(loan$loan_status_1)
summary(loan$loan_status_1)
ggplot(loan, aes(loan_status_1)) + geom_bar()

#write the contents to a csv and export for analysis in Tableau
loan2 <- loan[,c("annual_inc","loan_amnt","funded_amnt","int_rate_grp","grade","dti","emp_length_grp","purpose","home_ownership","loan_status_1")]
write.csv(loan2,"loan_tab.csv")

## Multivatriate Analysis
#correlations between variables 

#1.  we see no correlation between dti and annual income,
#select the continuous variables into a df for checking their correlation
cont_var_loan <- data.frame(loan2$annual_inc,loan2$loan_amnt,loan2$funded_amnt,loan2$dti)
cor(cont_var_loan)
#now from the correlation matrix we see the following:
# 1. strong correlation b/w : loan_amt & funded_amt, 
# 2. positive correlation between annual_inc and (loan_amt,funded_amt) -- this can be probed further.
## we can also view the same graphically using ggally package.
library(GGally)
ggpairs(cont_var_loan)

## let us get into analyzing the categoriacal variables - multivariate
# analyzing loan_status_1 against the continuous variables

ggplot(loan2, aes(x=loan_status_1,y=annual_inc, fill=loan_status_1)) + geom_boxplot()
# Two observations from this plot :
## 1. mean annual_income of people in current_new is higher implies loan is already being given to
##    people with a higher annual income.
## 2. The late payments distribution is skewed.Which shows most higher annual_income people 
##    have a tendency to delay payment. 

ggplot(loan2, aes(x=loan_status_1,y=loan_amnt, fill=loan_status_1)) + geom_boxplot()
## 3. We get an interesting insight that, on an average the defaulted loan amounts are lesser 
##    compared to the ones in current and late stages.

ggplot(loan2, aes(x=loan_status_1,y=funded_amnt, fill=loan_status_1)) + geom_boxplot()
# no significant insights here as the insights are similar to loan_amnt. 
# Due to strong correlation between loan_amnt and funded_amnt, we need not continue with 
# analysis of this any more, all trends will be same as witnessed for loan_amnt.

ggplot(loan2, aes(x=loan_status_1,y=dti, fill=loan_status_1)) + geom_boxplot()
# no significant insights or impact of dti on loan_status.

# analyzing int_rate_grp against the continuous variables
ggplot(loan2, aes(x=int_rate_grp,y=annual_inc, fill=int_rate_grp)) + geom_boxplot()
## 4. Interest rates given by the bank seem to be proportional to the annual_inc.

ggplot(loan2, aes(x=int_rate_grp,y=loan_amnt, fill=int_rate_grp)) + geom_boxplot()
## 5. Interest rates given by the bank seem to be proportional to the loan amount as well.

ggplot(loan2, aes(x=int_rate_grp,y=dti, fill=int_rate_grp)) + geom_boxplot()
## We see that there was a positive correlation between annual_inc and loan amount. 
## This plot confirms the above mentioned fact.


# The rest of the multi-variate analysis is done visually by plotting in Tableau as well.
# The insights obtained/ observations for every variable is recorded there.

############## checkpoint-3 ########
## hypothesis test 1 for loan_status_1 
# get a subset of the loan status for current_new and default_new
loan_def <- subset(loan, loan$loan_status_1=="default_new")
loan_curr <- subset(loan, loan$loan_status_1=="current_new")

## RUN T-Tests!!
# insights from the hypothesis test is provided in the insights.docx
t.test( loan_curr$annual_inc,loan_def$annual_inc,alternative = "two.sided",mu=0,conf.level = .95)
t.test(loan_def$loan_amnt, loan_curr$loan_amnt,alternative = "two.sided",mu=0,conf.level = .95)
t.test(loan_def$funded_amnt, loan_curr$funded_amnt,alternative = "two.sided",mu=0,conf.level = .95)
t.test(loan_def$dti, loan_curr$dti,alternative = "two.sided",mu=0,conf.level = .95)

## hypothesis test 2 for int_rate_grp 
loan_int_high<- subset(loan, loan$int_rate_grp=="High")
loan_int_low <- subset(loan,loan$int_rate_grp=="Low")

# insights from the hypothesis test is provided in the insights.docx
t.test(loan_int_high$annual_inc, loan_int_low$annual_inc,alternative = "two.sided",mu=0,conf.level = .95)
t.test(loan_int_high$loan_amnt, loan_int_low$loan_amnt,alternative = "two.sided",mu=0,conf.level = .95)
t.test(loan_int_high$funded_amnt, loan_int_low$funded_amnt,alternative = "two.sided",mu=0,conf.level = .95)
t.test(loan_int_high$dti, loan_int_low$dti,alternative = "two.sided",mu=0,conf.level = .95)
