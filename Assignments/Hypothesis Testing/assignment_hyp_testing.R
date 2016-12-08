setwd("C:\\Users\\nsrinivn\\Desktop\\PGDDA\\PGDDA\\study\\assignments\\course-2\\hypothesis testing")

# import the data set.
popularity <- read.csv("Article popularity dataset.csv", stringsAsFactors = F)

#understand the structure of the data set
View(popularity)
str(popularity)

#check for NA values.
length(is.na(popularity$shares))


mean(popularity$shares)
summary(popularity$shares)

#outlier treatment of the popularity shares.
outliers <- boxplot.stats(popularity$shares)
new_shares <- popularity$shares[!popularity$shares %in% outliers$out]
mean(new_shares)
popularity1 <- popularity[which(popularity$shares %in% new_shares),]

quantile(popularity1$shares)
boxplot(popularity1$shares)

# extracting date and day of publishing from the website
library(stringr)
str_extract(popularity1$URL[1], "(?:[0-9]{2})?[0-9]{2}.[0-3]?[0-9].[0-3]?[0-9]")
popularity1$pubdate <- unlist(lapply(popularity1$URL,function(x)str_extract(x, "(?:[0-9]{2})?[0-9]{2}.[0-3]?[0-9].[0-3]?[0-9]")))
View(popularity1)
library(lubridate)
popularity1$pubdate <- as.Date(popularity1$pubdate) 
str(popularity1$pubdate)
popularity1$pubday <- as.factor(weekdays(popularity1$pubdate))
popularity1$pubday

# make the categories of the article 
popularity1$Types_of_Articles <- paste(popularity1$data_channel_is_lifestyle,popularity1$data_channel_is_entertainment,popularity1$data_channel_is_bus,popularity1$data_channel_is_socmed,popularity1$data_channel_is_tech,popularity1$data_channel_is_world,sep="")
View(popularity1)
popularity1$Types_of_Articles <- as.factor(popularity1$Types_of_Articles)
levels(popularity1$Types_of_Articles) <- c("Others","World","Technology","Social_Media","Business","Entertainment","Lifestyle")

# fill the table
sm_weekday <- subset(popularity1,popularity1$Types_of_Articles %in% c("Social_Media") & popularity1$pubday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
sm_weekend <- subset(popularity1,popularity1$Types_of_Articles %in% c("Social_Media") & popularity1$pubday %in% c("Sunday","Saturday"))
soc_med <- subset(popularity1,popularity1$Types_of_Articles %in% c("Social_Media"))
mean(sm_weekend$shares)
mean(sm_weekday$shares)
mean(soc_med$shares)


others_weekday <- subset(popularity1,!(popularity1$Types_of_Articles %in% c("Social_Media")) & popularity1$pubday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
others_weekend <- subset(popularity1,!(popularity1$Types_of_Articles %in% c("Social_Media")) & popularity1$pubday %in% c("Sunday","Saturday"))
others <- subset(popularity1, !(popularity1$Types_of_Articles %in% c("Social_Media")))
mean(others_weekday$shares)
mean(others_weekend$shares)
mean(others$shares)

tot_weekday <- subset(popularity1,popularity1$pubday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
mean(tot_weekday$shares)

tot_weekend <- subset(popularity1,popularity1$pubday %in% c("Sunday","Saturday"))
mean(tot_weekend$shares)

mean(popularity1$shares)

#q1
t.test(tot_weekday$shares, tot_weekend$shares,alternative = "two.sided",mu=0,conf.level = .99)

#q2
t.test(sm_weekend$shares,others_weekend$shares,mu=0,conf.level = .99)

#q3
t.test(sm_weekday$shares,others_weekday$shares,mu=0,conf.level = .99)

#q4
t.test(sm_weekend$shares,sm_weekday$shares,mu=0,conf.level = .95)
