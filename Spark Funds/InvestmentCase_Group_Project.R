install.packages(c("dplyr", "tidyr", "stringr"))
library(dplyr)
library(tidyr)
library(stringr)

#------------------------------------------------------------------------------------------
# Authors: Vijay Narayanan, Shriram Venkat Peddhibhotla, Jayanti Tripathi and Devesh Joshi
#------------------------------------------------------------------------------------------

#setwd("~/Downloads/Upgrad-IIIT/Course/Group Project 1")
#change the above line to point to the director where the companies.txt, rounts2.csv and mapping.csv files are

# Checkpoint 1: Data Cleaning
# ---------------------------

# Load companies dataset
# Note - Delimiter in the file is tab. There are some columns that have double quotes. So, retain them.
companies <- read.table("companies.txt", sep = "\t", quote = "", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
# Load rounds2 dataset
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE, header = TRUE)
# Load mapping dataset
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE, header = TRUE)

# Convert chr columns in companies.txt file that have duplicates to factors
companies$category_list <- factor(companies$category_list)
companies$status <- factor(companies$status)
companies$country_code <- factor(companies$country_code)
companies$state_code <- factor(companies$state_code)
companies$region <- factor(companies$region)
companies$city <- factor(companies$city)
str(companies)

# Convert chr columns in rounds2.csv file that have duplicates to factors
rounds2$funding_round_type <- factor(rounds2$funding_round_type)
rounds2$funding_round_code <- factor(rounds2$funding_round_code)
str(rounds2)

# Column permalink in companies.txt file is the unique key and links to company_permalink column 
# in rounds2.csv file. But, company_permalink column has same company data in both upper and lower case. 
# So, it is better to convert both permalink and company_permalink to lower case to avoid double counting of company

# Convert permalink to lower case
companies$permalink <- str_to_lower(companies$permalink)
# Convert company_permalink to lower case
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink)

# Table 1.1  Questions
# ---------------------
# 1.How many unique companies are present in rounds2?
nrow(distinct(rounds2, company_permalink))
# 2.How many unique companies are present in the companies file?
nrow(distinct(companies, permalink))
# 3.In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
# Ans - permalink is the unique key for each company
# 4. Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
# Ans - Y
# 5. Merge the two data frames so that all variables (columns) 
# in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame.
# Assumption : Some companies (76) in rounds 2 file that are not found in companies file. It is best to retain them.
# These records are useful to have when calculating country wise and sector wise investments
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink", all = TRUE)

# Checkpoint 2: Funding Type Analysis
# -----------------------------------

# Table 2.1 Questions
# -------------------
# 1. Average funding amount of venture type
# Slice on venture type - Filter records matching funding_round_type == venture
venture_funding_type <- filter(master_frame, funding_round_type == "venture")
# Calculate average amount raised. 
# Some companies have not raised any amount and hence raised_amount_usd column has NA.
# Assumption - For calculation of mean, NA values are ignored
mean(venture_funding_type$raised_amount_usd, na.rm = TRUE)

# 2.Average funding amount of angel type
# Slice on angel type. Filter records matching funding_round_type == angel
angel_funding_type <- filter(master_frame, funding_round_type == "angel")
# Calculate average amount raised.
# Assumption - For calculation of mean, NA values are ignored
mean(angel_funding_type$raised_amount_usd, na.rm = TRUE)

# 3.Average funding amount of seed type
# Slice on seed type. Filter records matching funding_round_type == seed
seed_funding_type <- filter(master_frame, funding_round_type == "seed")
# Calculate average amount raised.
# Assumption - For calculation of mean, NA values are ignored
mean(seed_funding_type$raised_amount_usd, na.rm = TRUE)

# 4.Average funding amount of private equity type
# Slice on private_quity type. Filter records matching funding_round_type == private_equity
private_equity_funding_type <- filter(master_frame, funding_round_type == "private_equity")
# Calculate average amount raised.
# Assumption - For calculation of mean, NA values are ignored
mean(private_equity_funding_type$raised_amount_usd, na.rm = TRUE)

# Choose investment type
# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment 
# round, which investment type is the most suitable for them?
# Group by funding type, calculate average raised amount and filter out only between 5 and 15 million. Select the top most.
summarise(group_by(master_frame, funding_round_type), mean_inv = mean(raised_amount_usd, na.rm = TRUE)) %>% 
  filter(mean_inv >= 5000000 & mean_inv <= 15000000) %>% arrange(desc(mean_inv)) %>% head(1)
# Answer - venture investment type is best suited for Spart Funds

# Checkpoint 3: Country Analysis
# ------------------------------

# Table 3.1 Questions
# ---------------------

# Assumption - venture is the chosen investment type

# Filter records from data set that has "venture" funding type.
# Country code is blank or NA in 2917 cases. If the country code is missing then this data will
# not be useful for country analysis. Also, the number of records with NA is not many.
# 1. Assumption - ignore records that have no country code i.e NA or BLANK
# 2. Assumption - All sectors are included. It is useful to include data that has no sector name
venture_with_country_code <- filter(master_frame, funding_round_type == "venture" & !is.na(country_code) & country_code != "")

# 1. Top nine countries which have received the highest total funding 
top9 <- aggregate(raised_amount_usd ~ country_code, venture_with_country_code, FUN = sum) %>% arrange(desc(raised_amount_usd)) %>% head(9)
top9

# Identify the top three English-speaking countries in the data frame top9.
# Top 3 are USA, GBR and IND
# Note - CHN has the second highest investments in venture type but it is ignored as it is not an
# Englist speaking country.

# Checkpoint 4: Sector Analysis 1
# -------------------------------

# Looking at the data in mapping.csv file, it can be seen that the sectors appear in wide format
# in individual columns with values of 0 and 1. This is not an useful format for analysis.
# It is better to convert these multiple columns into a key column called main_sector and value column 
# called main_sector_value

# Convert wide format of sectors into long format
sectors <- gather(mapping, key="main_sector", value="main_sector_value", Automotive...Sports : Social..Finance..Analytics..Advertising)
head(sectors)

# Column category_list has 0 or more sector values separated by a | character.
# Assumption - Split values category_list column on separator | into primary_sector and other_sectors
# Assumption - If there are multiple sectors separated by | then take first value to be primary sector
# Assumption - All other values are merged and stored in other_sectors
# Assumption - If a sector value is missing then leave it as blank. This is important as blank sector 
# will later be mapped to a sector called Blanks

# Separate column category_list into primary_sector and other_sectors using | as separator
master_frame <- separate(master_frame, category_list, into = c("primary_sector", "other_sectors"), sep = "\\|", remove = FALSE, extra = "merge", fill = "right")

# Map primary sector to main sector 
master_frame <- merge(master_frame, sectors, by.x = "primary_sector", by.y = "category_list", all = TRUE)

# Only select records that has main_sector_value = 1 i.e has a main sector
master_frame <- master_frame[-which(master_frame$main_sector_value == 0), ]

# Checkpoint 5: Sector Analysis 2
# -------------------------------
# Assumption 1 - Country 1 is USA, Country 2 is GBR and Country 3 is IND
# Assumption 2 - Funding type is venture
# Assumption 3 - Funding range is 5 to 15 million USD

# Country USA
# Slice records matching type venture and country code USA and funding range between 5 and 15 million
D1 <- filter(master_frame, funding_round_type == "venture" & country_code == "USA" & !is.na(raised_amount_usd) & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
# Count number of investments in each sector
d1_sector_wise_num_of_investments <- aggregate(raised_amount_usd ~ main_sector, D1, FUN = length) %>% arrange(desc(raised_amount_usd))
# Name the column total_no_of_invs_per_sector
d1_sector_wise_num_of_investments <- rename(d1_sector_wise_num_of_investments, total_no_of_invs_per_sector = raised_amount_usd)
# Sum invesment amount for each sector
d1_sector_wise_amt_invested <- aggregate(raised_amount_usd ~ main_sector, D1, FUN = sum) %>% arrange(desc(raised_amount_usd))
# Name the column total_amt_inv_per_sector
d1_sector_wise_amt_invested <- rename(d1_sector_wise_amt_invested, total_amt_inv_per_sector = raised_amount_usd)
# Merge columns total_no_of_invs_per_sector and total_amt_inv_per_sector into D1 data frame
D1 <- merge(D1, d1_sector_wise_num_of_investments, by = "main_sector", all = TRUE)
D1 <- merge(D1, d1_sector_wise_amt_invested, by = "main_sector", all = TRUE)
# D1 contains all columns of master_frame, two new columns (total num of investments per sector and total amt invested per sector)
str(D1)
head(D1)

# Country GBP
D2 <- filter(master_frame, funding_round_type == "venture" & country_code == "GBR" & !is.na(raised_amount_usd) & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
d2_sector_wise_num_of_investments <- aggregate(raised_amount_usd ~ main_sector, D2, FUN = length) %>% arrange(desc(raised_amount_usd))
d2_sector_wise_num_of_investments <- rename(d2_sector_wise_num_of_investments, total_no_of_invs_per_sector = raised_amount_usd)
d2_sector_wise_amt_invested <- aggregate(raised_amount_usd ~ main_sector, D2, FUN = sum) %>% arrange(desc(raised_amount_usd))
d2_sector_wise_amt_invested <- rename(d2_sector_wise_amt_invested, total_amt_inv_per_sector = raised_amount_usd)
D2 <- merge(D2, d2_sector_wise_num_of_investments, by = "main_sector", all = TRUE)
D2 <- merge(D2, d2_sector_wise_amt_invested, by = "main_sector", all = TRUE)
# D2 contains all columns of master_frame, two new columns (total num of investments per sector and total amt invested per sector)
str(D2)
head(D2)

# Country IND
D3 <- filter(master_frame, funding_round_type == "venture" & country_code == "IND" & !is.na(raised_amount_usd) & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
d3_sector_wise_num_of_investments <- aggregate(raised_amount_usd ~ main_sector, D3, FUN = length) %>% arrange(desc(raised_amount_usd))
d3_sector_wise_num_of_investments <- rename(d3_sector_wise_num_of_investments, total_no_of_invs_per_sector = raised_amount_usd)
d3_sector_wise_amt_invested <- aggregate(raised_amount_usd ~ main_sector, D3, FUN = sum) %>% arrange(desc(raised_amount_usd))
d3_sector_wise_amt_invested <- rename(d3_sector_wise_amt_invested, total_amt_inv_per_sector = raised_amount_usd)
D3 <- merge(D3, d3_sector_wise_num_of_investments, by = "main_sector", all = TRUE)
D3 <- merge(D3, d3_sector_wise_amt_invested, by = "main_sector", all = TRUE)
# D3 contains all columns of master_frame, two new columns (total num of investments per sector and total amt invested per sector)
str(D3)
head(D3)

# Table 5.1 Questions
# ---------------------
# For Country 1 - USA
# ---------------------
# 1. Total number of Investments (count) in USA
summarise(d1_sector_wise_num_of_investments, total = sum(total_no_of_invs_per_sector))
# 2. Total amount of investment (USD) by USA
summarise(d1_sector_wise_amt_invested, total = sum(total_amt_inv_per_sector))
# 3. Top Sector name (no. of investment-wise) in USA
d1_sector_wise_num_of_investments[1,1]
# 4. Second Sector name (no. of investment-wise) in USA
d1_sector_wise_num_of_investments[2,1]
# 5. Third Sector name (no. of investment-wise) in USA
d1_sector_wise_num_of_investments[3,1]
# 6. Number of investments in top sector (3) in USA
d1_sector_wise_num_of_investments[1,2]
# 7. Number of investments in second sector (4) in USA
d1_sector_wise_num_of_investments[2,2]
# 8. Number of investments in third sector (5) in USA
d1_sector_wise_num_of_investments[3,2]
# 9. For the top sector count-wise (point 3), which company received the highest investment?
# Note - Others sector received the highest investments in USA
# Assume - Filter main sector by value Others as it recieved the highest investments
d1_sector_others <- filter(D1, main_sector == "Others")
summarise(group_by(d1_sector_others, name), total = sum(raised_amount_usd)) %>% arrange(desc(total)) %>% head(1)
# 10. For point 4 (second best sector count-wise), which company received the highest investment?
# Note - Cleantech...Semiconductors sector received the second highest investments in USA
# Assume - Filter main sector by value Cleantech...Semiconductors as it received the second highest investments
d1_sector_others <- filter(D1, main_sector == "Cleantech...Semiconductors")
summarise(group_by(d1_sector_others, name), total = sum(raised_amount_usd)) %>% arrange(desc(total)) %>% head(1)

# ---------------------
# For Country 2 - GBR
# ---------------------
# 1. Total number of Investments (count) in GBR
summarise(d2_sector_wise_num_of_investments, total = sum(total_no_of_invs_per_sector))
# 2. Total amount of investment (USD) by GBR
summarise(d2_sector_wise_amt_invested, total = sum(total_amt_inv_per_sector))
# 3. Top Sector name (no. of investment-wise) in GBR
d2_sector_wise_num_of_investments[1,1]
# 4. Second Sector name (no. of investment-wise) in GBR
d2_sector_wise_num_of_investments[2,1]
# 5. Third Sector name (no. of investment-wise) in GBR
d2_sector_wise_num_of_investments[3,1]
# 6. Number of investments in top sector (3) in GBR
d2_sector_wise_num_of_investments[1,2]
# 7. Number of investments in second sector (4) in GBR
d2_sector_wise_num_of_investments[2,2]
# 8. Number of investments in third sector (5) in GBR
d2_sector_wise_num_of_investments[3,2]
# 9. For the top sector count-wise (point 3), which company received the highest investment?
# Note - Others sector received the highest investments in GBR
# Assume - Filter main sector by value Others as it recieved the highest investments
d2_sector_others <- filter(D2, main_sector == "Others")
summarise(group_by(d2_sector_others, name), total = sum(raised_amount_usd)) %>% arrange(desc(total)) %>% head(1)
# 10. For point 4 (second best sector count-wise), which company received the highest investment?
# Note - Cleantech...Semiconductors sector received the second highest investments in GBR
# Assume - Filter main sector by value Cleantech...Semiconductors as it received the second highest investments
d2_sector_others <- filter(D2, main_sector == "Cleantech...Semiconductors")
summarise(group_by(d2_sector_others, name), total = sum(raised_amount_usd)) %>% arrange(desc(total)) %>% head(1)

# ---------------------
# For Country 3 - IND
# ---------------------
# 1. Total number of Investments (count) in IND
summarise(d3_sector_wise_num_of_investments, total = sum(total_no_of_invs_per_sector))
# 2. Total amount of investment (USD) by IND
summarise(d3_sector_wise_amt_invested, total = sum(total_amt_inv_per_sector))
# 3. Top Sector name (no. of investment-wise) in IND
d3_sector_wise_num_of_investments[1,1]
# 4. Second Sector name (no. of investment-wise) in IND
d3_sector_wise_num_of_investments[2,1]
# 5. Third Sector name (no. of investment-wise) in IND
d3_sector_wise_num_of_investments[3,1]
# 6. Number of investments in top sector (3) in IND
d3_sector_wise_num_of_investments[1,2]
# 7. Number of investments in second sector (4) in IND
d3_sector_wise_num_of_investments[2,2]
# 8. Number of investments in third sector (5) in IND
d3_sector_wise_num_of_investments[3,2]
# 9. For the top sector count-wise (point 3), which company received the highest investment?
# Note - Others sector received the highest investments in IND
# Assume - Filter main sector by value Others as it recieved the highest investments
d3_sector_others <- filter(D3, main_sector == "Others")
summarise(group_by(d3_sector_others, name), total = sum(raised_amount_usd)) %>% arrange(desc(total)) %>% head(1)
# 10. For point 4 (second best sector count-wise), which company received the highest investment?
# Note - News..Search.and.Messaging sector received the second highest investments in IND
# Assume - Filter main sector by value  News..Search.and.Messaging as it received the second highest investments
d3_sector_others <- filter(D3, main_sector == "News..Search.and.Messaging")
summarise(group_by(d3_sector_others, name), total = sum(raised_amount_usd)) %>% arrange(desc(total)) %>% head(1)

#---------END---------
