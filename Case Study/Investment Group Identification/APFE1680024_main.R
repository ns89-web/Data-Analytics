#Load the two files companies.txt and rounds2.csv into two data frames and name them companies and rounds2 respectively.
companies <- read.delim("companies.txt", header=TRUE, sep="\t",stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)

#Clean the permalink columns in rounds2 and companies data frame
rounds2$company_permalink <- tolower(rounds2$company_permalink)
rounds2$company_permalink <- trimws(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)
companies$permalink <- trimws(companies$permalink)

########################## Start of Checkpoint - 1 ##########################
#How many unique companies are present in rounds2?
no_of_unique_companies_rounds2 <- length(unique(rounds2$company_permalink))
#66368

#How many unique companies are present in companies ?
no_of_unique_companies_companies <- length(unique(companies$permalink))
#66368

#In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column.
length(unique(companies$permalink))
nrow(companies)
#companies$permalink can be used as the unique key

#Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
permalink_match <- subset(companies, companies$permalink %in% rounds2$company_permalink)
nrows_in_permalink_match <- nrow(permalink_match)
#Answer : N
# nrows_in_permalink_match == length(unique(rounds2$company_permalink)) == length(unique(companies$permalink))

#Merge the two data frames so that all  variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame. How many observations are present in master_frame ?
master_frame <- merge(rounds2,companies,by.x="company_permalink",by.y="permalink",all.x=T)
nrow(master_frame)
#114949 number of observations are present in master_frame

########################## Start of Checkpoint - 2 ##########################

#How many NA values are present in the column raised_amount_usd ?
length(which(is.na(master_frame$raised_amount_usd)))
#Answer : 19990

#What do you replace NA values of raised_amount_usd  with? Enter a numeric value.
subsitute_na <- mean(master_frame$raised_amount_usd, na.rm=T)
master_frame$raised_amount_usd[which(is.na(master_frame$raised_amount_usd))] <- subsitute_na
# NA will be relaced by 10426869( mean of raised_amount_usd)


########################## Start of Checkpoint - 3 ##########################
# Average funding amount of venture type : 11,623,493
mean(master_frame[master_frame$funding_round_type == "venture", "raised_amount_usd"])

#Average funding amount of angel type : 2,875,946
mean(master_frame[master_frame$funding_round_type == "angel", "raised_amount_usd"])

#Average funding amount of seed type : 2,920,791
mean(master_frame[master_frame$funding_round_type == "seed", "raised_amount_usd"])

#Average funding amount of private equity type :63,704,339
mean(master_frame[master_frame$funding_round_type == "private_equity", "raised_amount_usd"])

# Alternate method : aggregate(raised_amount_usd~funding_round_type,master_frame,mean)

# Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, 
# which investment type is the most suitable for them?
# Answer: "venture" (venture has the nearest highest mean in the range 5-15 million among the above four investment round types)


########################## Start of Checkpoint - 4 ##########################
#Spark Funds wants to see the top 9 countries which have received the highest total funding 
# first subset all the rows which are of funding type "venture"
top9_1 <- subset(master_frame, master_frame$funding_round_type =="venture")

# then aggregate this subset based on country code
top9_2 <- aggregate(raised_amount_usd~country_code,top9_1,sum)

# sort the aggregated data frame
sorted_top9_2 <- top9_2[order(top9_2$raised_amount_usd, decreasing = T),]

#replace the blank country name with NA
sorted_top9_2$country_code[which(sorted_top9_2$country_code == "")] <- NA

# Get the top 9 countries for the investment type VENTURE
top9 <- na.omit(sorted_top9_2[1:10, ])

# Refer : http://www.emmir.org/fileadmin/user_upload/admission/Countries_where_English_is_an_official_language.pdf
# The top 3 english speaking countries
# 1. USA (United States of America)
# 2. GBR (Great Britain)
# 3. IND (India)


########################## Start of Checkpoint - 5 ##########################
sectors <- read.csv("mapping_file.csv", header = T, stringsAsFactors = F)

#check for NA
length(which(is.na(sectors$category_list)))
length(which(is.na(sectors$main_sector)))
# There are no NA values

# Create a new column prime_category from category_list
master_frame$prime_category <- sub("\\|.*$",replacement = "", master_frame$category_list)

# Treat the NA values to blank values - to match it with the "Blanks" value in sectors data frame -  for ease of merging
master_frame$prime_category[which(is.na(master_frame$prime_category))] <- ""

# Merge master_frame with the sector data
master_frame <- merge(master_frame, sectors, by.x ="prime_category", by.y="category_list")

########################## Start of Checkpoint - 6 ##########################
## Create a dataframe subset for USA with funding_round_type = venture
D1 <- subset(master_frame, (master_frame$funding_round_type=="venture") &(master_frame$country_code=="USA"))

# Getting the sector wise count and sum of investments and merging them
temp_sector_inv <- aggregate(raised_amount_usd~main_sector,D1,sum)
temp_sector_count <- aggregate(funding_round_type~main_sector,D1,length)
sector_count_inv_usa <- merge(temp_sector_inv,temp_sector_count, by="main_sector")

# renaming the column names for better readability
colnames(sector_count_inv_usa)[which(colnames(sector_count_inv_usa) == "raised_amount_usd")] <- "total_investment_in_sector"
colnames(sector_count_inv_usa)[which(colnames(sector_count_inv_usa) == "funding_round_type")] <- "number_of_inv"
D1 <- merge(D1,sector_count_inv_usa, by="main_sector", all.x=T)


# For the information asked in the table :
#6.1.1  Total number of investments (count)
sum(sector_count_inv_usa$number_of_inv)
#6.1.2 Total amount of investment (USD)
sum(sector_count_inv_usa$total_investment_in_sector)

sector_count_inv_usa[order(sector_count_inv_usa$number_of_inv,decreasing = T),]
# 6.1.3 Top sector name (no. of  investment-wise) 6.1.6 Number of investments in top  sector (3)
#Answer : Others (8767) - First column in the above sector_count_inv_usa after ordering
# 6.1.4 Second sector name (no. of  investment-wise) 6.1.7 Number of investments in second sector (4)
#Answer : Cleantech / Semiconductors (8268) - Second column in the above sector_count_inv_usa after ordering
# 6.1.5 Third sector name (no. of  investment-wise) 6.1.8 Number of investments in third sector (5)
#Answer : Social, Finance, Analytics, Advertising (7822) - Third column in the above sector_count_inv_usa after ordering

#6.1.9
# First get all the companies in others category for USA, Venture type
df_others_usa <- D1[which(D1$main_sector=="Others"),]
# We have to aggregate company wise now as there are more than one funding round for the company
df_others_usa_byCompany <- aggregate(raised_amount_usd~company_permalink, df_others_usa,sum)
# get the max funded company - we will get the permalink here
max_funded_others <- df_others_usa_byCompany[which.max(df_others_usa_byCompany$raised_amount_usd), "company_permalink"]
# with the permalink get the name of the company
max_funded_others_name <- unique(D1[which(D1$company_permalink == max_funded_others), "name"])
max_funded_others_name
#SoFi

#6.1.10	
df_Cleantech_usa <- D1[which(D1$main_sector=="Cleantech / Semiconductors"),]
df_Cleantech_usa_byCompany <- aggregate(raised_amount_usd~company_permalink, df_Cleantech_usa,sum)
max_funded_Cleantech <- df_Cleantech_usa_byCompany[which.max(df_Cleantech_usa_byCompany$raised_amount_usd), "company_permalink"]
max_funded_Cleantech_name <- unique(D1[which(D1$company_permalink == max_funded_Cleantech), "name"])
max_funded_Cleantech_name
#Freescale Semiconductor

## For GBR
## Create a dataframe subset for GBR with funding_round_type = venture
D2 <- subset(master_frame, (master_frame$funding_round_type=="venture") &(master_frame$country_code=="GBR"))

# Getting the sector wise count and sum of investments and merging them
temp_sector_inv <- aggregate(raised_amount_usd~main_sector,D2,sum)
temp_sector_count <- aggregate(funding_round_type~main_sector,D2,length)
sector_count_inv_gbr <- merge(temp_sector_inv,temp_sector_count, by="main_sector")

# renaming the column names for better readability
colnames(sector_count_inv_gbr)[which(colnames(sector_count_inv_gbr) == "raised_amount_usd")] <- "total_investment_in_sector"
colnames(sector_count_inv_gbr)[which(colnames(sector_count_inv_gbr) == "funding_round_type")] <- "number_of_inv"
D2 <- merge(D2,sector_count_inv_gbr, by="main_sector", all.x=T)


# For the information asked in the table :
#6.1.1  Total number of investments (count)
sum(sector_count_inv_gbr$number_of_inv)
#6.1.2 Total amount of investment (USD)
sum(sector_count_inv_gbr$total_investment_in_sector)


sector_count_inv_gbr[order(sector_count_inv_gbr$number_of_inv,decreasing = T),]
# 6.1.3 Top sector name (no. of  investment-wise) 6.1.6 Number of investments in top  sector (3)
#Answer : Others (580) - First column in the above sector_count_inv_gbr after ordering
# 6.1.4 Second sector name (no. of  investment-wise) 6.1.7 Number of investments in second sector (4)
#Answer : Social, Finance, Analytics, Advertising (481) - Second column in the above sector_count_inv_gbr after ordering
# 6.1.5 Third sector name (no. of  investment-wise) 6.1.8 Number of investments in third sector (5)
#Answer : Cleantech / Semiconductors (466) - Third column in the above sector_count_inv_gbr after ordering

#6.1.9
# First get all the companies in others category for GBR, Venture type
df_others_gbr <- D2[which(D2$main_sector=="Others"),]
# We have to aggregate company wise now as there are more than one funding round for the company
df_others_gbr_byCompany <- aggregate(raised_amount_usd~company_permalink, df_others_gbr,sum)
# get the max funded company - we will get the permalink here
max_funded_others <- df_others_gbr_byCompany[which.max(df_others_gbr_byCompany$raised_amount_usd), "company_permalink"]
# with the permalink get the name of the company
max_funded_others_name <- unique(D2[which(D2$company_permalink == max_funded_others), "name"])
max_funded_others_name
#OneWeb


#6.1.10	
df_Social_gbr <- D2[which(D2$main_sector=="Social, Finance, Analytics, Advertising"),]
df_Social_gbr_byCompany <- aggregate(raised_amount_usd~company_permalink, df_Social_gbr,sum)
max_funded_Social <- df_Social_gbr_byCompany[which.max(df_Social_gbr_byCompany$raised_amount_usd), "company_permalink"]
max_funded_Social_name <- unique(D2[which(D2$company_permalink == max_funded_Social), "name"])
max_funded_Social_name
#Powa Technologies

## For IND
## Create a dataframe subset for IND with funding_round_type = venture
D3 <- subset(master_frame, (master_frame$funding_round_type=="venture") &(master_frame$country_code=="IND"))

# Getting the sector wise count and sum of investments and merging them
temp_sector_inv <- aggregate(raised_amount_usd~main_sector,D3,sum)
temp_sector_count <- aggregate(funding_round_type~main_sector,D3,length)
sector_count_inv_ind <- merge(temp_sector_inv,temp_sector_count, by="main_sector")

# renaming the column names for better readability
colnames(sector_count_inv_ind)[which(colnames(sector_count_inv_ind) == "raised_amount_usd")] <- "total_investment_in_sector"
colnames(sector_count_inv_ind)[which(colnames(sector_count_inv_ind) == "funding_round_type")] <- "number_of_inv"
D3 <- merge(D3,sector_count_inv_ind, by="main_sector", all.x=T)


# For the information asked in the table :
#6.1.1  Total number of investments (count)
sum(sector_count_inv_ind$number_of_inv)
#6.1.2 Total amount of investment (USD)
sum(sector_count_inv_ind$total_investment_in_sector)


sector_count_inv_ind[order(sector_count_inv_ind$number_of_inv,decreasing = T),]
# 6.1.3 Top sector name (no. of  investment-wise) 6.1.6 Number of investments in top  sector (3)
#Answer : Others (332) - First column in the above sector_count_inv_ind after ordering
# 6.1.4 Second sector name (no. of  investment-wise) 6.1.7 Number of investments in second sector (4)
#Answer : Social, Finance, Analytics, Advertising (193) - Second column in the above sector_count_inv_ind after ordering
# 6.1.5 Third sector name (no. of  investment-wise) 6.1.8 Number of investments in third sector (5)
#Answer : News, Search and Messaging (154) - Third column in the above sector_count_inv_ind after ordering

#6.1.9
# First get all the companies in others category for IND, Venture type
df_others_ind <- D3[which(D3$main_sector=="Others"),]
# We have to aggregate company wise now as there are more than one funding round for the company
df_others_ind_byCompany <- aggregate(raised_amount_usd~company_permalink, df_others_ind,sum)
# get the max funded company - we will get the permalink here
max_funded_others <- df_others_ind_byCompany[which.max(df_others_ind_byCompany$raised_amount_usd), "company_permalink"]
# with the permalink get the name of the company
max_funded_others_name <- unique(D3[which(D3$company_permalink == max_funded_others), "name"])
max_funded_others_name
#Flipkart


#6.1.10	
df_Social_ind <- D3[which(D3$main_sector=="Social, Finance, Analytics, Advertising"),]
df_Social_ind_byCompany <- aggregate(raised_amount_usd~company_permalink, df_Social_ind,sum)
max_funded_Social <- df_Social_ind_byCompany[which.max(df_Social_ind_byCompany$raised_amount_usd), "company_permalink"]
max_funded_Social_name <- unique(D3[which(D3$company_permalink == max_funded_Social), "name"])
max_funded_Social_name
#ShopClues.com

########################## Start of checkpoint - 7 ##########################
library(ggplot2)
#plot 1
df_fun_type <- aggregate(raised_amount_usd~funding_round_type, master_frame, mean)
df_fun_type <- df_fun_type[which(df_fun_type$funding_round_type %in% list("angel", "seed", "venture", "private_equity")),]

plot1 <- ggplot(df_fun_type, aes(x=factor(1), y=raised_amount_usd, fill=factor(funding_round_type))) 
plot1 <- plot1 +  geom_bar(stat="identity")
plot1 <- plot1 + coord_polar(theta = "y" )
plot1

#plot 2
# We will use the top9 data frame that we created before for this plot
plot2 <- ggplot(top9, aes(x=country_code, y=raised_amount_usd, fill="red")) + geom_bar(stat="identity")
plot2

#plot 3
# In the previous checklist we got to know that the top 3 English speaking countries are USA, GRB, IND
# Top 3 sectors in USA : "Others" , "Cleantech / Semiconductors" and "Social, Finance, Analytics, Advertising"
top3_sectors_in_usa <- subset(D1, D1$main_sector %in% c("Others","Cleantech / Semiconductors","Social, Finance, Analytics, Advertising"))
# Top 3 sectors in GRB : "Others" , "Social, Finance, Analytics, Advertising" and "Cleantech / Semiconductors"
top3_sectors_in_gbr <- subset(D2, D2$main_sector %in% c("Others","Social, Finance, Analytics, Advertising","Cleantech / Semiconductors"))
# Top 3 sectors in IND : "Others" , "Social, Finance, Analytics, Advertising" and "News, Search and Messaging"
top3_sectors_in_ind <- subset(D3, D3$main_sector %in% c("Others","Social, Finance, Analytics, Advertising","News, Search and Messaging"))

# Merge all the above 3 dataframes into one data frame which will result in info about top 3 sectors of top 3 countries
top3_sectors_countries <- rbind(top3_sectors_in_usa,top3_sectors_in_gbr,top3_sectors_in_ind)

plot3 <- ggplot(top3_sectors_countries,aes(x=country_code,fill=factor(main_sector))) + geom_bar(position = "dodge")
plot3 <- plot3 + geom_text(stat="count",aes(label=..count..), position = position_dodge(width=1), vjust="inward")
plot3

