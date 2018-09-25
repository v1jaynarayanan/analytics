##########################################################################################
############################## Gramener - Group Case Study ###############################
##########################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

##########################################################################################
#### Authors : Vijay Narayanan, Shriram Venkat Peddhibhotla,                          ####
####           Jayanti Tripathi and Devesh Joshi                                      ####
##########################################################################################

# Load dataset
# Set to working directory where the data file is
setwd("~/Downloads/Upgrad-IIIT/Course/Group Project 2")
loan_original <- read.csv("loan.csv", stringsAsFactors = FALSE, header = TRUE)

# View first few rows
head(loan_original)
# Check structure of dataset
str(loan_original)
# Summary of dataset
summary(loan_original)

############################################################################################
################################## Problem Statement #######################################
############################################################################################
# 1. Online Lending Company faces a risk of losing money by lending to risky loan applicants
# 2. Understand the consumer and loan attributes that influence the tendency to default
# 3. Identify risky loan applicants
# 4. Minimise risk of losing money while lending money to customers
############################################################################################

################################## Metadata of loan data ###################################
# 1. There are 39717 rows with 111 columns in the dataset
# 2. Analysis of columns reveals that they can be grouped into Customer, Loan and 
#    Other atrributes
############################################################################################

### What are the Consumer attributes in the dataset?
###
# 1. emp_title
# 2. emp_length
# 3. home_ownership
# 4. annual_inc
# 5. url
# 6. desc
# 7. purpose
# 8. title
# 9. zip_code
# 10. addr_state
###

### What are the Loan attributes in the dataset?
###
# 1. loan_amt
# 2. term
# 3. funded_amnt
# 4. funded_amnt_inv
# 5. int_rate
# 6. issue_d
# 7. loan_status
# 8. grade
# 9. sub_grade
# 10. installment
# 11. dti
# 12. verification_status
# 13. pub_rec
# 14. pub_rec_bankruptcies
###

#############################################################################################################
### Objective in this case study is to identify risky loan applicants and reduce credit loss for the company.
### This can be acheived by analysing customer and loan attributes. Both customer and loan attributes 
### have been identified above. These attributes will be analysed to determine the driving factors for 
### defaulting on a loan. Other attributes in the dataset will not be considered in this analysis.
#############################################################################################################

###
# Select required customer and loan attributes for analysis
required_attributes <- c("id", "emp_title", "emp_length", "home_ownership", "annual_inc", "url", "desc", "purpose", 
                         "title","zip_code", "addr_state", "loan_amnt", "term", "funded_amnt",  "funded_amnt_inv", 
                         "int_rate",  "issue_d", "loan_status","grade", "sub_grade", "installment", "dti", 
                         "verification_status", "pub_rec", "pub_rec_bankruptcies")

loan <- select(loan_original, required_attributes)
str(loan)

##########################################################################################
##################################### Data Cleaning ######################################
##########################################################################################

# Check for NAs
sapply(loan, function(x) sum(is.na(x)))
# 1 record has NA for title. This NA value can be retained as it cannot be replaced with anything meaningful.
# There are 697 NAs, which are missing values in pub_rec_bankruptcies. 

# Can these missing values be replaced? What are the other values in pub_rec_bankruptcies?
unique(loan$pub_rec_bankruptcies)
# Column pub_rec_bankruptcies represents number of public record bankruptcies. Other values include 0, 1 and 2. 
# All NAs can be set to a value of 0 to indicate that there are no public record bankruptcies.

# Replace NA with 0 in pub_rec_bankruptcies column
loan$pub_rec_bankruptcies[is.na(loan$pub_rec_bankruptcies)] <- 0

# Convert characters to factor variables
loan$loan_status <- factor(loan$loan_status)
loan$term <- factor(loan$term) 
loan$purpose <- factor(loan$purpose) 

# Convert issue date to a standard date
# As day is missing, add day 01 to mon-yy
loan$issue_date <- paste("01-",loan$issue_d,sep="")
# Convert to date format
loan$issue_date <- as.Date(loan$issue_date,"%d-%b-%y")
# Extract year from date
loan$issue_year <- format(loan$issue_date, "%Y")

# Convert loan status values to numbers i.e "Charged Off" to 2, "Current" to 1 and "Fully Paid" to 0
loan$loan_status_num <- ifelse(loan$loan_status == "Charged Off", 2, ifelse(loan$loan_status == "Current", 1, 0))

# Remove a few variables such as emp_title, url, desc, zip_code from our analysis as there
# is no meaningful information that can gleaned from the values present in these variables
exclude_columns <- c("emp_title", "url", "desc", "zip_code")
loan <- loan[, !(colnames(loan) %in% exclude_columns)]

# Summarise Dataset
summary(loan)

##########################################################################################
#################################### Common Functions ####################################
##########################################################################################

groupByAndSummarise <- function(loan, var_name) {
  loan %>% group_by(!! var_name) %>% summarise(count = n()) %>% mutate(percentage = (count / sum(count)) * 100) %>% arrange(desc(percentage))
}

plotBarChartWithPercentages <- function(loan, x_var, status, x_lab, y_lab, title) {
  ggplot(loan, aes(x_var, fill = status)) + geom_bar() + stat_count(aes(label = scales::percent((..count..)/sum(..count..))), geom = "text", position=position_stack(vjust=0.5)) + labs(x=x_lab, y=y_lab, title = title)
}

plotBarChartWithColourFillAndPercentages <- function(loan, x_var, col, x_lab, y_lab, title) {
  ggplot(loan, aes(x=x_var)) + geom_bar(fill = col) + stat_count(aes(label = scales::percent((..count..)/sum(..count..))), geom = "text", position=position_stack(vjust=0.5)) + labs(x=x_lab, y=y_lab, title = title)
}

plotBarChartWithXandYColourFill <- function(loan, x_var, y_var, x_lab, y_lab, title) {
  ggplot(loan, aes(as.factor(x_var), y_var)) + geom_bar(stat = "identity", fill = "#228B22") + labs(x=x_lab, y=y_lab, title = title)
}

scatterPlotWithRegressionLine <- function(x_var, y_var, title, x_lab, y_lab) {
  plot(x_var, y_var, main = title, xlab = x_lab, ylab = y_lab)
  lines(lowess(x_var, y_var), col="red")
}

plotHistogramWithColourFill <- function(loan, x_var, hcol, fcol, bin, x_lab, y_lab, title) {
  ggplot(loan, aes(x_var)) + geom_histogram(col = hcol, fill = fcol, binwidth = bin) + labs(x=x_lab, y=y_lab, title = title)
}

generateHeatMap <- function(mcorm) {
  plot <- ggplot(data = mcorm, aes(x=X1, y=X2, fill=value, label= value))
  plot_tile <- plot + geom_tile()
  plot_fill_color <- plot_tile + scale_fill_gradient2(low = "yellow",high ="red",mid = "orange")
  heatmap <- plot_fill_color + geom_text()
  heatmap
}

##########################################################################################

##########################################################################################
################################## Univariate Analysis ###################################
##########################################################################################

# 1. Loan status is an important categorical attribute as it confirms if a person has paid or defaulted on a loan.
# How many records are there in each status? What is the percentage of people who defaulted or fully paid loan?
groupByAndSummarise(loan, quo(loan_status))
# 83% of the total loans were Fully Paid by customers. 
# 14.2% of total customers defaulted on their loans. 

# Frequency plot of number of loans in each status shown as percentage
plotBarChartWithColourFillAndPercentages(loan, as.factor(loan$loan_status), "#FF6666", "Status", "Number of Loans", "Loan Status of all Applications")
# So, overall default rate is 14.2%

##
# 2. Loan term is an unordered categorical attribute that describes number payments on a loan.
# How does the number of payments vary for customers. What is the proportion of customers in each loan term?
summary(loan$term)
groupByAndSummarise(loan, quo(term))
# 73% of people chose a loan term of 3 years i.e 36 months
# 27% of people chose a long term loan of 5 years ie 60 months
plotBarChartWithColourFillAndPercentages(loan, as.factor(loan$term), "#1399c6", "Term in months", "Number of Loans", "Loan Terms of All Loan Applications")

##
# 3. What is the main purpose stated by customers when taking out a loan?
purposes <- groupByAndSummarise(loan, quo(purpose))
top_10_purposes <- head(purposes, 10)
top_10_purposes
# Plot
plotBarChartWithXandYColourFill(top_10_purposes, as.factor(top_10_purposes$purpose), top_10_purposes$percentage, "Purpose", "% of Total Loan Applications", "Top 10 Purpose for Loan Applications")
# Most popular reason for loans is consolidation of debts, which is the purpose for nearly 47% of customers
# Next best reason is to pay credit card bills followed by Other, Home Improvement and Major Purchase

##
# 4. What is typically the loan amount?
boxplot(loan$loan_amnt, main = "Loan Amount")
# Average loan amount is $10000. But, people have borrowed above $15000. Some cusomers have
# borrowed higher loan amounts in excess of $30000. 

# How many loans have loan amount in excess of $30000
loan_amnt_gt_30000 <- filter(loan, loan_amnt >= "30000")
nrow(loan_amnt_gt_30000)
# There are 17043 loans with a high loan amount

# Are these loan amounts beyond $30000 outliers and skewing the mean loan amount?
# Filter loans less than $30000 and check the mean
loan_amnt_lt_30000 <- filter(loan, loan_amnt < 30000)
summary(loan_amnt_lt_30000$loan_amnt)
# It can be seen the mean loan amount has not changed much even with large loan amounts.
# So, high loan amounts are not outliers and it is best NOT to remove these high loan amounts

# Is there a trend in the loan amount year on year?
scatterPlotWithRegressionLine(loan$issue_year, loan$loan_amnt, "Loan Amount Every Year", "Year", "Loan Amount in $")
# Average Loan amount has been increasing each year and shows a steady positive trend.

##
# 5. What is average interest rate for a loan
# Remove % character
loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate))
summary(loan$int_rate)
# Interest rate ranges from a minumum of 5.42 to a maximum of 24.59 with a mean of 12 and a median of 11.86
boxplot(loan$int_rate, main = "Loan Interest Rates")
# Box plot shows that mean is 12 and vast majority of customers are in the interest rate between 1st quartile
# value of 9.2 and third quartile value of 14.6. 
# However there are some customers i.e outliers with high interest rates

# How many loans have high interst rates > 14.6 ?
high_int_rate_loans <- filter(loan, int_rate > 14.6)
nrow(high_int_rate_loans)
# There are 9832 people who have loans on interest rate in excess of 14.6.
# Nearly 25% of loans have a high interest rate. Clearly, these cannot be regarded as outliers.

##
# 6. What is the ratio of verified vs unverified customer's income
plotBarChartWithColourFillAndPercentages(loan, as.factor(loan$verification_status), "#0e8c7f", "Income Verfication", "Number of Loans", "Verfication Status of Customer's Income")
# 43% of loans did not have their customer's income or source verified
# 25% of loans had both the customer’s income and its source verified
# 32% of loans had the customer’s income verified but not its source 

##
# 7. What is the average Debt To Income ratio of a customer?
plotHistogramWithColourFill(loan, loan$dti, "black", "#228B22", 1, "DTI", "Number of Loans", "DTI Distribution Plot") 
# Average DTI of customers applying for a loan is 13
boxplot(loan$dti, main = "DTI")
# Is there a trend in DTI over the years?
scatterPlotWithRegressionLine(loan$issue_year, loan$dti, "Debt To Income Each Year", "Year", "DTI")
# There is a steady increase in DTI every year indicating that there is a growing debt in the population

##########################################################################################
###################### Summary of findings from Univariate Analysis ######################
##########################################################################################
## 1. 14.2% of total customers defaulted on their loans. 
## 2. 73% of people chose a loan term of 3 years and 27% of people a terms of 5 years
## 3. 47% of people applied for a loan for the purpose of Debt Consolidation
## 4. Average Loan amount has been increasing each year and shows a steady positive trend
## 5. Nearly 25% of loans have a high interest rate of above 14.6
## 6. 43% of loans did not have either the customer's income or its source verified
## 7. Debt in the population is growing as there is a steady increase in DTI every year

## Using the preliminary findings from the univariate analyis an indepth study of the   
## impact of these variables on loan status will be carried out in the following sections.
## This wll help us identify which variables are siginificant drivers for loan default.

##########################################################################################
################################### Bivariate Analysis ###################################
##########################################################################################

##
# 1. In the top 5 loan purposes what is the percentage of customers who default?
# Purpose - Unordered categorical attrtibute
# Define Top 5 purposes
top_5_purposes <- c("debt_consolidation", "credit_card", "other", "home_improvement", "major_purchase")
# Filter loans that are in Top 5 purposes
top_5_purpose_loans <- filter(loan, loan$purpose %in% top_5_purposes)
# Plot bar chart with status for each purpose
plotBarChartWithPercentages(top_5_purpose_loans, as.factor(top_5_purpose_loans$purpose), top_5_purpose_loans$loan_status, "Loan Purpose", "Number of Loans", "Top 5 Loan Purposes")
# Highest defaulters were when the purpose for loan was Debt Consolidation
# 57% of people borrowed loan for debt consolidation and 8% of those defaulted on their loans
# A total of 10% people defaulted when purpose for loan was either debt consolidation or credit cards

##
# 2. How does number of loan payments i.e Term impact loan status? Is there a linear relationship between the two variables?
# Remove "months" from term
loan$term <- as.numeric(gsub("months", "", loan$term))
# Find out the correlation between two variables
cor(loan$term,as.numeric(loan$loan_status))
# There is a negative relationship between loan term and loan status. This means that if loan term in months 
# increases there is a tendency for customers to default on their loans.
# So, customers on a longer duration loan term tend to be risky

# Frequency plot of number of loans in each term showing the loan status as percentage
plotBarChartWithPercentages(loan, as.factor(loan$term), loan$loan_status, "Term", "Number of Loans", "Loan Terms")
# 73% of applicants applied for a 3 year loan compared to 27% people who required a 5 year loan.
# 8.1% of people defaulted when loan term was 36 months and only 6% of people defaulted when it was 60 months
# Term is a significant factor and if the loan term increases the probability of default increases

##
# 3. Does loan amount have an impact on loan status?
# Group loan amounts into small, medium, large and extra large categories
loan$loan_amnt_group <-ifelse(loan$loan_amnt <=5000, "Small", ifelse(loan$loan_amnt > 5000 & loan$loan_amnt <= 15000, "Medium",
                          ifelse(loan$loan_amnt > 15000 & loan$loan_amnt <= 25000, "Large", "Extra Large")))

plotBarChartWithPercentages(loan, loan$loan_amnt_group, loan$loan_status, "Loan Amount", "Number of Loans", "Loan Amount")
# More that 50% of the loan amount was between $5000 and $15000
# Nearly 7% of the customers in the medium loan group defaulted

cor(as.numeric(loan$loan_status_num), as.numeric(loan$loan_amnt))
# There is a positive relationship between the two variables. 
# As the loan amount increases there is tendency for customers to default

##
# 4.Interest Rate - Quantitative attribute
# It will be useful to group customers by interest rates
# Let us create three groups of low interest rate (< 10), medium (>=10 and <15) and high (>=15)
loan$int_rate_grp <- ifelse(loan$int_rate < 10, "Low Rate", ifelse(loan$int_rate < 15, "Medium Rate", "High Rate"))

plotBarChartWithPercentages(loan, loan$int_rate_grp, loan$loan_status, "Interest Rate", "Number of Loans", "Loan Interest Rates")
# Nearly 48% of customers have a medium loan rate i.e between 10% and 15% interest rate.
# Only 2% of customers with low interest rate defaulted on their loans.
# Default rate is highest (6.9%) when loan rate is medium. 
# Around 20% of customers were on a high loan rate i.e above 15%. Nearly 5% of the high loan rate payers defaulted.

# Does Loan Amount and Interest Rate have any relationship?
scatterPlotWithRegressionLine(loan$loan_amnt, loan$int_rate, "Loan Amount vs Interest Rate", "Loan Amount", "Interest Rate")
# When Loan amount increases after $5000 the interest rate increases steadily

##
# 5. Debt To Income - Quantitative attrtibute
summary(loan$dti)
# DTI ranges from a low of 0 to 29.99 with median at 13.40
# Let us create three groups of low dti rate (< 10), medium (>=10 and <20) and high (>=20)
loan$dti_grp <- ifelse(loan$dti < 10, "Low DTI", ifelse(loan$dti < 20, "Medium DTI", "High DTI"))

plotBarChartWithPercentages(loan, loan$dti_grp, loan$loan_status, "DTI", "Number of Loans", "Debt To Income")
# Nearly 48% of loans had a Medium DTI. Around 7% of these defaulted on their loans.
# Only 3% of people with a High Debt To Income percentage defaulted on their loans.

# Is there a correlation between dti and loan interest rate
cor(loan$dti, loan$int_rate)
# There is a slight positive correlation
scatterPlotWithRegressionLine(loan$dti, loan$int_rate, "Debt to Income vs Interest Rate", "DTI", "Interest Rate")
# When the DTI of a customer increases then the loan rate tends to marginally increase

# As medium debt to income customers defaulted the most, it is worth finding out if there is any
# correlation to their annual incomes

##
# 6. Annual Income - Quantitative attrtibute
summary(loan$annual_inc)

#Let us group incomes based on values
loan$annual_inc_grp <- ifelse(loan$annual_inc < 50000, "Very Low", ifelse(loan$annual_inc >= 50000 & loan$annual_inc < 100000, "Low",
                          ifelse(loan$annual_inc >= 100000 & loan$annual_inc < 150000, "Medium", "High")))

# How do the borrowers vary in each group?
plotBarChartWithPercentages(loan, loan$annual_inc_grp, loan$loan_status, "Income Group", "Number of Loans", "Income Groups")
# 84% of the borrowers were in very low and low income groups. 
# Default rate was 6% in both very and low income groups
# Risk of default is high in very low and low income groups and low in High income group

# Now, Is there a correlation between annual income and dti?
options("scipen"=100, "digits"=2)
cor(x=loan$annual_inc, y=loan$dti)
# There appears to be a negative correlation between annual income and dti
scatterPlotWithRegressionLine(loan$annual_inc, loan$dti, "Debt to Income vs Annual Income", "Annual Income", "DTI")
# It can be seen that when annual income increases the debt to income tends to decrease

##
# 7. Grade - Ordered Categorical attribute
loan$grade <- factor(loan$grade, ordered = TRUE)
loan %>% group_by(grade) %>% summarise(count = n()) %>% mutate(percentage = (count / sum(count)) * 100) %>% arrange(desc(percentage))
# Maximum number of loans were in grade B, which accounts to 31% of all loans.
plotBarChartWithPercentages(loan, loan$grade, loan$loan_status, "Grades", "Number of Loans", "Loan Grades")
# Most number of defaults were in Loan Grades B and C

# Convert Grades into numbers to determine if there is a correlation with loan status
# Assume A to be the higest grade and G to be the lowest
# Assigning number 1 to G and 7 to A, so 1 (G) is lowest and 7 (A) is highest grade.
loan$grade_num <- ifelse(loan$grade == "G", 1, ifelse(loan$grade == "F", 2, ifelse(loan$grade == "E", 3,
                      ifelse(loan$grade == "D", 4, ifelse(loan$grade == "C", 5, ifelse(loan$grade == "B", 6, 
                          ifelse(loan$grade == "A", 7, 0)))))))
  
# Determine correlation
cor(loan$grade_num, loan$loan_status_num)
# There is a negative correlation between grade and loan status. When grade decreases then
# chances for defaulting on loan tends to increase.

# Does loan grade have any correlation with loan interest rate
scatterPlotWithRegressionLine(loan$grade, loan$int_rate, "Grade vs Interest Rate", "Grade", "Interest Rate")
# When loan grade decreases interest rate increases steeply 

##
# 8. Home Ownership - Unordered categorical attrtibute
# What are the types of home ownership?
unique(loan$home_ownership)

plotBarChartWithPercentages(loan, loan$home_ownership, loan$loan_status, "Home Ownership", "Number of Loans", "Home Ownership")
# 47.6% of loan applicants lived in rental homes and 7.1% of these customers defaulted
# 44.5% of customers were currently paying mortgage and out of which 5.9% defaulted on their loans
# Only 1.1% of customers who owned their property defaulted on their loans
# So, customers living on rent or paying mortgages have more tendency to default than people who own a property

# Convert Home Ownership into numbers to get a correlation with Loan Status
# Assign 0 to "NONE", 1 to "RENT", 2 to "MORTGATE", 3 to "OTHER" and 4 to "OWN"
loan$home_own_num <- ifelse(loan$home_ownership == "NONE", 0, ifelse(loan$home_ownership == "RENT", 1, 
                              ifelse(loan$home_ownership == "MORTGATE", 2, ifelse(loan$home_ownership == "OTHER", 3,
                                  ifelse(loan$home_ownership == "OWN", 4, 5)))))

# Determine Correlation
cor(loan$home_own_num, loan$loan_status_num)
# There is a negative correlation. If customers are owners of a property then the chance of defaulting decreases

##
# 9. Verification Status - Unordered categorical attrtibute
plotBarChartWithPercentages(loan, loan$verification_status, loan$loan_status, "Verification Status", "Number of Loans", "Verification of Customers Income and Source")
# 5.4% Defaulted when customer's income was not verified
# Low default rate of 3.6% when the customer's source of income was verified

##
# 10. Installment Amount - Quantitative attrtibute
summary(loan$installment)
# Monthly payments vary from a minimum of 16 to 1310

# Group installments into small, medium, high and very high categories
loan$installment_grp <-ifelse(loan$installment < 100, "Small", ifelse(loan$installment >= 100 & loan$installment < 500,"Medium",
                          ifelse(loan$installment >= 500 & loan$installment < 1000, "High", "Very High")))

# Does installment amount impact loan status?
plotBarChartWithPercentages(loan, loan$installment_grp, loan$loan_status, "Payment per month", "Number of Loans", "Loan Payment Per Month")
# 70% of loans had a medium payment i.e between $100 and $500 per month 
# Nearly 10% of medium installment customers defaulted on their loans
# Only 1.5% defaulted when installment amount was small i.e less that $500

##
# 11. State - Unordered Categorical attrtibute
# What are the top 5 states that defaulted most?
states <- groupByAndSummarise(loan, quo(addr_state))
top_5_states <- head(states, 5)
top_5_states

# Filter loans from the top 5 states
loans_from_top_5_states <- filter(loan, addr_state %in% top_5_states$addr_state)
# Top 5 states for loan applications were CA, NY, FL, TX and NJ.

# Is there a pattern for state on loan status?
plotBarChartWithPercentages(loans_from_top_5_states, loans_from_top_5_states$addr_state, loans_from_top_5_states$loan_status, "State", "Number of Loans", "Distribution of loans across top 5 States")
# CA had the most defaulters of 6.1% followed by NY of 2.7% and FL of 2.7%

##
# 12. Employee Length - Ordered Categorical attrtibute
# Employee Length

unique(loan$emp_length)

# Group into bins. 0.5, 1, 3, 4, 5, 6, 7, 8, 9, 10.
# There are 1075 records with a value of n/a for employee length. This amounts to 2.7% of total data.
# The annual income for these records ranges from 4200 to 648000. So, assigning a median value of 5
# as employee length for n/a records.

loan$emp_length_num <- ifelse(loan$emp_length == "< 1 year", 0.5, ifelse(loan$emp_length == "1 year", 1, 
                          ifelse(loan$emp_length == "3 years",3, ifelse(loan$emp_length == "4 years", 4,
                              ifelse(loan$emp_length == "5 years", 5, ifelse(loan$emp_length == "6 years", 6, 
                                  ifelse(loan$emp_length == "7 years", 7, ifelse(loan$emp_length == "8 years", 8, 
                                      ifelse(loan$emp_length == "9 years", 9, ifelse(loan$emp_length == "10+ years", 10, 5))))))))))

plotBarChartWithPercentages(loan, as.factor(loan$emp_length_num), loan$loan_status, "Employee Length in Years", "Number of Loans", "Employee Length")
# For 50% of loans employee length was below 5 years and 22% of loans were taken by customers 
# who had over 10 years of experience
# There is a higher tendency to default on the loans when employees length of work is less than 5 years  


############################################################################################
################################## Multi Variate Analysis ################################## 
############################################################################################

# Display correlation of selected attributes with loan status using a Correlation Matrix
corr_data <- loan[c("loan_status_num", "annual_inc", "loan_amnt", "term", "funded_amnt", "int_rate", "installment", "dti", "pub_rec", "emp_length_num", "grade_num", "home_own_num")]

# Correlation between variables
corm <- round(cor(corr_data),2)

# Melt
mcorm <- melt(corm)
head(mcorm)

# Graph
#ggplot(mcorm, aes(x=X1, y=X2, fill = value)) + geom_tile() + scale_fill_gradient2(low = "yellow",  high = "red")
generateHeatMap(mcorm)

################################################################################################
########### Summary of Key observations from Bivariate analysis and Heat Map ###################
################################################################################################
# 1. Risk of default increases when Interest Rate on the loan increases
# 2. Tendency to default on a loan increases when the loan Term increases
# 3. When loan Grade decreases then the potential to default on the loan increases
# 4. Higher loan amounts are riskier and increase the chances of default
# 5. Customers with a higher Debt To Income percentage are more riskier
# 6. Customers with derogatory Public Records are more riskier
# 7. Customers with a low or very low Annual Income have a higher tendency to default
# 8. Customers who own their Home have a lesser tendency to default compared to living on Rent
#    or paying Mortgage
# 9. Customers with a low employment length have a higher tendency to default
################################################################################################

################################################################################################
####################### Key Customer attributes for Loan Default ###############################
################################################################################################
#Debt To Income  +0.05 Higher the DTI more riskier the customer becomes
#Public Record   +0.05 A customer with derogatory public record has a higher chance of defaulting
#Annual Income   -0.04 Customer with a high annual income tends to be less riskier
#Employee Length +0.02 Customer with a low employee length tends to be more riskier
#Home Ownership  -0.02 Home owners are less riskier then living on rent or paying mortgage

################################################################################################
####################### Key Loan attributes for Loan Default ###################################
################################################################################################
#Interest Rate   +0.23 Higher interest rates increases the chances of default
#Term            +0.21 Longer the term higher the chances are of default
#Grade           -0.21 Lower grade loans have a higher chance of default
#Loan Amount     +0.08 Higher the loan amount higher the chances of default
#Installment     +0.04 Higher the installment amount higher the chances of default

################################################################################################
################# Top 5 variables which are strong indicators of default #######################
################################################################################################
# 1. Interest Rate
# 2. Term
# 3. Grade
# 4. Public Record
# 5. Annual Income
####
