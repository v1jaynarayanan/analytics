#############################################################################################
############################# HR Analytics - Group Case Study ###############################
#############################################################################################

library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(MASS)
library(car)
library(caret)
library(ROCR)

#############################################################################################
####    Author  : Vijay Narayanan                                                        ####
#############################################################################################

#############################################################################################
################################### Problem Statement #######################################
#############################################################################################
# Company XYZ is currently facing an employee attrition rate of 15% annually. This issue,
# 1. Leads to projects not delivering on time, hence earning bad reputation amongst customers
# 2. Forces company to maintain a sizeable department to cover for leaving employees
# 3. Increases training costs for the company to train new employees
#############################################################################################

#############################################################################################
################################### Project Objectives ######################################
#############################################################################################
# Objectives of this assignment are to,
# 1. Understand the significant attrbutes that are linked to attrition
# 2. Model the probability of attrition using a logistic regression
# 3. Provide recommendations to the company to curb attrition
#############################################################################################

# Set working directory
setwd("~/Downloads/Upgrad-IIIT/Course/Predictive Analytics I/PA-I_Case_Study_HR_Analytics")

# Load files
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE, header = TRUE)
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE, header = TRUE)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE, header = TRUE)
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE, header = TRUE)
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE, header = TRUE)

#############################################################################################
###################################   Data Cleaning  ########################################
#############################################################################################

# Are there any duplicates?
employee_survey_data[which(duplicated(employee_survey_data))]
general_data[which(duplicated(general_data))]
manager_survey_data[which(duplicated(manager_survey_data))]
in_time[which(duplicated(in_time))]
out_time[which(duplicated(out_time))]

# Check for any Outliers in data? 
boxplot(manager_survey_data$JobInvolvement, main = "JobInvolvement") # No outliers
boxplot(manager_survey_data$PerformanceRating, main = "PerformanceRating") # No outliers
boxplot(general_data$DistanceFromHome, main = "DistanceFromHome") # No outliers
boxplot(general_data$Education, main = "Education") # No outliers
boxplot(general_data$JobLevel, main = "JobLevel") # No outliers
boxplot(general_data$MonthlyIncome, main = "MonthlyIncome") # There are few outliers above 1700000
nrow(filter(general_data, MonthlyIncome > "1700000")) 
# 3780 employees have salary above 1700000 which is 85% of the employees so, these records will not be removed 
boxplot(general_data$NumCompaniesWorked, main = "NumCompaniesWorked") # No outliers
boxplot(general_data$PercentSalaryHike, main = "PercentSalaryHike") # No outliers
boxplot(general_data$StockOptionLevel, main = "StockOptionLevel") # No outliers
boxplot(general_data$TotalWorkingYears, main = "TotalWorkingYears") # There are some employees with over 30 years of experience. It is wise to not remove these records
boxplot(general_data$TrainingTimesLastYear, main = "TrainingTimesLastYear") # There are some employees who have received no training and some over 4 times per year. It is wise to not remove these records
boxplot(general_data$YearsAtCompany, main = "YearsAtCompany") # There are some employees who have worked for the same company for over 20 years. It is wise to not remove these records
boxplot(general_data$YearsSinceLastPromotion, main = "YearsSinceLastPromotion") # There are some employees who have not been promoted for over 5 years. Not removing these records.
boxplot(general_data$YearsWithCurrManager, main = "YearsWithCurrManager") # No significant outliers
boxplot(employee_survey_data$EnvironmentSatisfaction, main = "EnvironmentSatisfaction") # No outliers
boxplot(employee_survey_data$JobSatisfaction, main = "JobSatisfaction") # No outliers
boxplot(employee_survey_data$WorkLifeBalance, main = "WorkLifeBalance") # No outliers

# Are there any missing values?
sum(is.na(employee_survey_data)) # There are 83 NAs
sum(is.na(general_data)) # There are 28 NAs
sum(is.na(manager_survey_data)) # There are 0 NAs

sapply(employee_survey_data, function(x) sum(is.na(x))) 
# EnvironmentSatisfaction has 25 NAs, JobSatisfaction has 20 NAs and WorkLifeBalance has 38 NAs
# There are no outliers in all the 3 attributes. Only a few records have NAs i.e 83 out of 4410 which is 1.9%.
# Records with NAs will be replaced with median values because all values are whole numbers

# Replace missing values with median value for employee_survey_data
employee_survey_data$EnvironmentSatisfaction[which(is.na(employee_survey_data$EnvironmentSatisfaction))] <- median(employee_survey_data$EnvironmentSatisfaction,na.rm = TRUE)
employee_survey_data$JobSatisfaction[which(is.na(employee_survey_data$JobSatisfaction))] <- median(employee_survey_data$JobSatisfaction,na.rm = TRUE)
employee_survey_data$WorkLifeBalance[which(is.na(employee_survey_data$WorkLifeBalance))] <- median(employee_survey_data$WorkLifeBalance,na.rm = TRUE)

sapply(general_data, function(x) sum(is.na(x)))
# NumCompaniesWorked has 19 NAs and TotalWorkingYears has NAs
# This a total of 28 NAs out of 4410 i.e 0.6%. As proportion is insignificant and values are whole numbers, these records with NAs will be 
# replaced with median values

# Replace missing values with median value for general_data
general_data$NumCompaniesWorked[which(is.na(general_data$NumCompaniesWorked))] <- median(general_data$NumCompaniesWorked,na.rm = TRUE)
general_data$TotalWorkingYears[which(is.na(general_data$TotalWorkingYears))] <- median(general_data$TotalWorkingYears,na.rm = TRUE)

##################
# In and Out time
##################

# Check for NAs
sapply(in_time, function(x) sum(is.na(x))) #There are 11 columns that only have NAs
sapply(out_time, function(x) sum(is.na(x))) #There are 11 columns that only have NAs
# These 11 columns appear to be holidays as the entire column has missing values

# Common function to remove holiday columns
removeHolidayColumns <- function(data) {
  cols_removed <- subset(data, select = -c(X2015.01.01, X2015.01.14, X2015.01.26, X2015.03.05, X2015.05.01, X2015.07.17, 
                           X2015.09.17, X2015.10.02, X2015.11.09, X2015.11.10, X2015.11.11, X2015.12.25))
  return(cols_removed)
}

# Remove holiday columns from In and Out time data
in_time <- removeHolidayColumns(in_time)
out_time <- removeHolidayColumns(out_time)

# Common function to convert character to date time format
convertToDateTime <- function(data) {
  converted <- sapply(data[2:250], ymd_hms)
  return(converted)
}

# Convert character columns of in_time and out_time to datetime format
conv_in_time <- convertToDateTime(in_time)
conv_out_time <- convertToDateTime(out_time)

######################
# Derived attributes
######################

# Compute hours worked by an employee everyday i.e out_time - in_time
hrs_worked_each_day <- (conv_out_time[ ,1:249] - conv_in_time[, 1:249]) / 3600

# Average hours worked by an employee for the entire year
emp_hours_worked <- data.frame(EmployeeID=in_time$X, AvgHoursWorkedEachDay=rowMeans(hrs_worked_each_day, na.rm = T))

# Has the employee worked overtime? If average hours worked per day is > 8 then set overtime as 1 otherwise 0
emp_hours_worked$OverTime <- ifelse(emp_hours_worked$AvgHoursWorkedEachDay > 8, 1, 0)

# Number of leaves taken by the employee 
num_leaves_by_employee <- data.frame(EmployeeID=in_time$X, LeaveCount=rowSums(is.na(in_time)))

# Remove columns that have a single value
general_data <- general_data[, -which(colnames(general_data) %in% c("EmployeeCount", "StandardHours", "Over18"))]

# Common Functions to Plot
groupByAndSummarise <- function(data, var_name) {
  data %>% group_by(!! var_name) %>% summarise(count = n()) %>% mutate(percentage = (count / sum(count)) * 100) %>% arrange(desc(percentage))
}

plotHistWithPercentages <- function(data, x_var, status, x_lab, y_lab, title) {
  ggplot(data, aes(x_var, fill = status)) + geom_histogram(binwidth = 10) + labs(x=x_lab, y=y_lab, title = title)
}

plotBarChartWithPercentages <- function(data, x_var, status, x_lab, y_lab, title) {
  ggplot(data, aes(x_var, fill = status)) + geom_bar() + stat_count(aes(label = scales::percent((..count..)/sum(..count..))), geom = "text", position=position_stack(vjust=0.5)) + labs(x=x_lab, y=y_lab, title = title)
}

plotBarChartWithFill <- function(data, x_var, y_var, status, x_lab, y_lab, title) {
  ggplot(data) + aes(x_var, y_var, fill = status) + geom_bar(stat = "identity") +  labs(x=x_lab, y=y_lab, title=title)  
}

#############################################################################################
######################## Univariate and Bivariate Analysis  #################################
#############################################################################################

# Univariate Analysis
# Attrition % from the given dataset
emp_data <- groupByAndSummarise(general_data, quo(Attrition))
emp_data
# 16% of employees in the dataset have left the company
plotBarChartWithFill(emp_data, emp_data$Attrition, emp_data$percentage, emp_data$Attrition, "Attrition", "Percentage", "Employee Attrition")
  
# Bivariate Analysis
# Age of the employees in the company vs Attrition
plotHistWithPercentages(general_data, general_data$Age, general_data$Attrition, "Age", "Attrition Count", "Age Vs Attrition")
# Ages between 25 and 45 have the most number of employees leaving the company

# Business Travel vs Attrition
plotBarChartWithPercentages(general_data, as.factor(general_data$BusinessTravel), general_data$Attrition, "Business Travel", "Attrition Count", "Business Travel Vs Attrition")
# Attrition is the highest i.e 11% for employees who rarely travel
# Attrition is the lowest i.e < 1 % for employees who do not travel

# Department vs Attrition
plotBarChartWithPercentages(general_data, as.factor(general_data$Department), general_data$Attrition, "Department", "Attrition Count", "Department Vs Attrition")
# 10.3% of employees from R&D department left the company
# Attrition is the lowest 1.3% in Human Resources department

# Education vs Attrition
plotBarChartWithPercentages(general_data, as.factor(general_data$Education), general_data$Attrition, "Education", "Attrition Count", "Education Vs Attrition")
# Attrition rate is highest with 6.1% of employees with Bachelors degree left the company
# Likewise 4.2% of employees with Masters degree left the company
# Attrition rate is very low for employees who have Below College level education or Doctorate degrees.

# Gender vs Attrition
plotBarChartWithPercentages(general_data, as.factor(general_data$Gender), general_data$Attrition, "Gender", "Attrition Count", "Gender Vs Attrition")
# 10% of Males left the company compared to 6.1% Females

# JobRole vs Attrition
plotBarChartWithPercentages(general_data, as.factor(general_data$JobRole), general_data$Attrition, "Job Role", "Attrition Count", "Job Role Vs Attrition")
# Attrition rate is highest amongst Sales Executives, Research Scientist and Lab Technicians

# MaritalStatus vs Attrition
plotBarChartWithPercentages(general_data, as.factor(general_data$MaritalStatus), general_data$Attrition, "Marital Status", "Attrition Count", "Marital Status Vs Attrition")
# Attrition rate is highest i.e 8.2% amongst Single and 5.7% amongst Married.
# Attrition rate is lowest 2.2% amongst divorcees

# Create a Merged Dataset 
# Check if EmployeeID matches before merging all datasets
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,emp_hours_worked$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,num_leaves_by_employee$EmployeeID) # Identical EmployeeID across these datasets

###############################################
# Merge all data into one dataset for analysis
###############################################
hr_data <- merge(general_data, employee_survey_data, by="EmployeeID", all = F)
hr_data <- merge(hr_data, manager_survey_data, by="EmployeeID", all = F)
hr_data <- merge(hr_data, emp_hours_worked, by="EmployeeID", all = F)
hr_data <- merge(hr_data, num_leaves_by_employee, by="EmployeeID", all = F)
str(hr_data)

# Change chr to factor for categorical variables
hr_data$Gender <- factor(hr_data$Gender)
hr_data$BusinessTravel <- factor(hr_data$BusinessTravel)
hr_data$Department <- factor(hr_data$Department)
hr_data$MaritalStatus <- factor(hr_data$MaritalStatus)
hr_data$Attrition <- factor(hr_data$Attrition)

###################
# Dummy Variables
###################

# Gender - 2 Level variable
levels(hr_data$Gender)<-c(1,0) #Female = 1, Male = 0
hr_data$Gender <- as.numeric(levels(hr_data$Gender))[hr_data$Gender]

# BusinessTravel - 3 Level variable
dummy_var <- model.matrix(~ BusinessTravel -1, hr_data)
hr_data <- cbind(hr_data[-which(colnames(hr_data)=="BusinessTravel")], dummy_var)

# Department - 3 Level variable
dummy_var <- model.matrix(~ Department -1, hr_data)
hr_data <- cbind(hr_data[-which(colnames(hr_data)=="Department")], dummy_var)

# MaritalStatus - 3 Level variable
dummy_var <- model.matrix(~ MaritalStatus -1, hr_data)
hr_data <- cbind(hr_data[-which(colnames(hr_data)=="MaritalStatus")], dummy_var)

# EducationField - 6 Level variable
dummy_var <- model.matrix(~ EducationField -1, hr_data)
hr_data <- cbind(hr_data[-which(colnames(hr_data)=="EducationField")], dummy_var)

# JobRole - 9 Level variable
dummy_var <- model.matrix(~ JobRole -1, hr_data)
hr_data <- cbind(hr_data[-which(colnames(hr_data)=="JobRole")], dummy_var)

# Attrition - Target variable
levels(hr_data$Attrition)<-c(0,1) #No = 0, Yes = 1
hr_data$Attrition <- as.numeric(levels(hr_data$Attrition))[hr_data$Attrition]

# Scale continuous variables
hr_data[, -c(1,3,6,23,25:48)] <- scale(hr_data[, -c(1,3,6,23,25:48)])

#################################################################################
# Understand correlation of independent variables with target variable Attrition
#################################################################################
generateHeatMap <- function(mcorm) {
  plot <- ggplot(data = mcorm, aes(x=Var1, y=Var2, fill=value, label= value))
  plot_tile <- plot + geom_tile()
  plot_fill_color <- plot_tile + scale_fill_gradient2(low = "yellow",high ="red",mid = "orange")
  heatmap <- plot_fill_color + geom_text()
  heatmap
}

# Display correlation of selected attributes with Attrition using a Correlation Matrix
corr_data <- hr_data[c("Attrition", "PercentSalaryHike", "OverTime", "JobSatisfaction", "YearsSinceLastPromotion", "PerformanceRating", "YearsWithCurrManager", "YearsAtCompany")]

# Correlation between variables
corm <- round(cor(corr_data),2)

# Melt
mcorm <- melt(corm)
head(mcorm)

# Graph
generateHeatMap(mcorm)
# Some interesting observations from the heat map:
# 1. Employees tend to remain in the company if they are with one manager for a longer period 
# 2. There is a greater chance of promotion if employee is with the company and the same manager for a longer duration
# 3. Overtime is a significant influencer on employee attrition
# 4. Employees with lower job satisfaction have a higher chance of leaving the company
# 5. Greater the number of years since last promotion the more dissatisfied the employee is
# 6. Salary Hike increases with higher performance rating 

#############################################################################################
###################################   Model Building  #######################################
#############################################################################################

# Build Model
# Set seed
set.seed(100)

# Create Training and Test datasets with a split of 70:30
indices = sample(1:nrow(hr_data), floor(0.7*nrow(hr_data)))
train_hr_data = hr_data[indices,]
test_hr_data = hr_data[-indices,]

# First Logistic Regression Model
log_reg_model_1 <- glm(Attrition~., data=train_hr_data, family = "binomial")
summary(log_reg_model_1)

# Stepwise selection
step <- stepAIC(log_reg_model_1, direction="both")
step

# Build second model using independent variables selected by stepAIC
log_reg_model_2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + `JobRoleHealthcare Representative` + 
                         `JobRoleLaboratory Technician` + `JobRoleManufacturing Director` + 
                         `JobRoleResearch Director` + `JobRoleResearch Scientist` + 
                         `JobRoleSales Executive`, family = "binomial", data = train_hr_data)
summary(log_reg_model_2)

# Removing multicollinearity through VIF check
vif(log_reg_model_2)
# YearsAtCompany has VIF > 2 and also has ** rating with p-value of 0.008645. 
# YearsAtCompany can be removed from the model

# Build third model by removing YearsAtCompany
log_reg_model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear +
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + `JobRoleHealthcare Representative` + 
                         `JobRoleLaboratory Technician` + `JobRoleManufacturing Director` + 
                         `JobRoleResearch Director` + `JobRoleResearch Scientist` + 
                         `JobRoleSales Executive`, family = "binomial", data = train_hr_data)
summary(log_reg_model_3)
# Check VIF
vif(log_reg_model_3)
# JobRoleSales Executive has VIF 2.039844 and ** rating with p-value of 0.002650. 

# Build fourth model by removing JobRoleSales Executive
log_reg_model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear +
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + `JobRoleHealthcare Representative` + 
                         `JobRoleLaboratory Technician` + `JobRoleManufacturing Director` + 
                         `JobRoleResearch Director` + `JobRoleResearch Scientist`, family = "binomial", data = train_hr_data)
summary(log_reg_model_4)
# Check VIF
vif(log_reg_model_4)
# The only independent variable that has VIF > 2 is TotalWorkingYears but appears to be statistically significant
# So, using p-value to remove insignificant variables i.e p-value > 0.05
# On this basis removing JobRoleHealthcare Representative that has a high p-value of 0.969751

# Build fifth model by removing JobRoleHealthcare Representative
log_reg_model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear +
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + 
                         `JobRoleLaboratory Technician` + `JobRoleManufacturing Director` + 
                         `JobRoleResearch Director` + `JobRoleResearch Scientist`, family = "binomial", data = train_hr_data)
summary(log_reg_model_5)
# Remove JobRoleResearch Scientist that has a high p-value of 0.759139 

# Build sixth model by removing JobRoleResearch Scientist
log_reg_model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear +
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + 
                         `JobRoleLaboratory Technician` + `JobRoleManufacturing Director` + 
                         `JobRoleResearch Director`, family = "binomial", data = train_hr_data)
summary(log_reg_model_6)
# Remove JobRoleLaboratory Technician that has a high p-value of 0.812561 

# Build seventh model by removing JobRoleLaboratory Technician
log_reg_model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear +
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + 
                         `JobRoleManufacturing Director` + `JobRoleResearch Director`, family = "binomial", data = train_hr_data)
summary(log_reg_model_7)
# Remove JobRoleResearch Director that has a p-value of 0.293298

# Build eigth model by removing JobRoleResearch Director
log_reg_model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                         TotalWorkingYears + TrainingTimesLastYear +
                         YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                         JobSatisfaction + WorkLifeBalance + OverTime + `BusinessTravelNon-Travel` + 
                         BusinessTravelTravel_Frequently + MaritalStatusDivorced + 
                         MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + 
                         `JobRoleResearch Director`, family = "binomial", data = train_hr_data)
summary(log_reg_model_8)
# Remove StockOptionLevel that has a p-value of 0.20368

# Build ninth model by removing StockOptionLevel
log_reg_model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                         TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                         EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + OverTime + 
                         `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently + 
                         MaritalStatusDivorced + MaritalStatusMarried + `EducationFieldHuman Resources` + 
                         `EducationFieldLife Sciences` + EducationFieldMedical + 
                         `JobRoleResearch Director`, family = "binomial", data = train_hr_data)
summary(log_reg_model_9)
# Remove JobRoleResearch Director that has a p-value of 0.140297

# Build tenth model by removing JobRoleResearch Director
log_reg_model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                          TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                          EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + OverTime + 
                          `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently + 
                          MaritalStatusDivorced + MaritalStatusMarried + `EducationFieldHuman Resources` + 
                          `EducationFieldLife Sciences` + EducationFieldMedical, family = "binomial", data = train_hr_data)
summary(log_reg_model_10)
# Remove EducationFieldMedical that has a p-value of 0.131569

# Build eleventh model by removing EducationFieldMedical
log_reg_model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                          TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                          EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + OverTime + 
                          `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently + 
                          MaritalStatusDivorced + MaritalStatusMarried + `EducationFieldHuman Resources` + 
                          `EducationFieldLife Sciences`, family = "binomial", data = train_hr_data)
summary(log_reg_model_11)
# Remove EducationFieldLife Sciences that has a p-value of 0.183787

# Build twelveth model by removing EducationFieldLife Sciences
log_reg_model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                          TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                          EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + OverTime + 
                          `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently + 
                          MaritalStatusDivorced + MaritalStatusMarried + `EducationFieldHuman Resources`, 
                          family = "binomial", data = train_hr_data)
summary(log_reg_model_12)
# Remove TrainingTimesLastYear that has ** stars and a p-value of 0.003083

# Build thirteenth model by removing TrainingTimesLastYear
log_reg_model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                          YearsSinceLastPromotion + YearsWithCurrManager + 
                          EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + OverTime + 
                          `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently + 
                          MaritalStatusDivorced + MaritalStatusMarried + `EducationFieldHuman Resources`, 
                        family = "binomial", data = train_hr_data)
summary(log_reg_model_13)
# All independent variables are now statistically significant and have a *** rating

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -2.18921    0.11688 -18.731  < 2e-16 ***
#  Age                             -0.37649    0.08036  -4.685 2.80e-06 ***
#  NumCompaniesWorked               0.44514    0.05920   7.519 5.53e-14 ***
#  TotalWorkingYears               -0.48535    0.10593  -4.582 4.61e-06 ***
#  YearsSinceLastPromotion          0.58642    0.07617   7.699 1.37e-14 ***
#  YearsWithCurrManager            -0.51565    0.08633  -5.973 2.33e-09 ***
#  EnvironmentSatisfaction         -0.43625    0.05688  -7.669 1.73e-14 ***
#  JobSatisfaction                 -0.39398    0.05644  -6.981 2.93e-12 ***
#  WorkLifeBalance                 -0.23933    0.05447  -4.394 1.12e-05 ***
#  OverTime                         1.68617    0.11784  14.309  < 2e-16 ***
#  `BusinessTravelNon-Travel`      -0.95339    0.26804  -3.557 0.000375 ***
#  BusinessTravelTravel_Frequently  0.80657    0.13268   6.079 1.21e-09 ***
#  MaritalStatusDivorced           -1.25312    0.16679  -7.513 5.78e-14 ***
#  MaritalStatusMarried            -0.86023    0.12448  -6.910 4.84e-12 ***
#  `EducationFieldHuman Resources`  1.67021    0.31668   5.274 1.33e-07 ***

#############################################################################################
###################################   Model Evaluation  #####################################
#############################################################################################

# Run prediction on the Model using test data
predict_attr <- predict(log_reg_model_13, type = "response", newdata = test_hr_data[,-which(colnames(test_hr_data)=="Attrition")])
summary(predict_attr)

# Change actual Attrition values to Yes/No where Yes = 1 and No = 0
test_actual_attrition <- factor(ifelse(test_hr_data$Attrition == 1, "Yes", "No"))

# Use a Cutoff value of 50%
# Change Predicted Attrition values to Yes/No using a cutoff value of 0.5
# If predicted probability is >= 0.5 then 1 else 0
test_pred_attrition <- factor(ifelse(predict_attr >= 0.5, "Yes", "No"))

# Confusion Matrix of Predicted vs Actual with Cutoff value of 0.5
confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
# Model has an accuracy of 84.73% with Specificity of 95.43% and Sensitivity of 27.05%.
# True Negatives for the model or Specificity is really high, which means it is good at predicting
# the actual employees who are not likeyl to leave the company. However, the sensitivity or True Positive rate 
# is very low. So, it can be improved to predict correctly the employees who are likely to leave the company.

# Use a Lower Cutoff value of 30%
# If predicted probability is >= 0.3 then 1 else 0
test_pred_attrition <- factor(ifelse(predict_attr >= 0.3, "Yes", "No"))

# Confusion Matrix of Predicted vs Actual with Cutoff value of 0.3
confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
# Model now has an accuracy of 82.77% with Specificity of 87.45% and Sensitivity of 57.48%.
# Though the Sensitivity has improved there is still a huge difference between the three values

# Determine optimum Cutoff value to balance Accuracy, Sensitivity and Specificity values
findOptimalCutOff <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(predict_attr >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Create a sequence for cutoff values from 0.01 to 0.80
s = seq(.01,.80,length=100)

# Initialise Matrix with 3 columns and 100 rows
perf_values_matrix = matrix(0,100,3)

for(i in 1:100)
{
  perf_values_matrix[i,] = findOptimalCutOff(s[i])
} 

# Plot line graph for Sensitivity, Specificity and Accuracy for each cutoff value
plot(s, perf_values_matrix[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,perf_values_matrix[,2],col="darkgreen",lwd=2)
lines(s,perf_values_matrix[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Choose Optimal Cutoff value that has Sensitivity and Specificity values within less than 1% range
cutoff <- s[which(abs(perf_values_matrix[,1]-perf_values_matrix[,2])<0.01)]
cutoff

# Using Optimised Cutoff value of 0.177
# If predicted probability is >= 0.177 then 1 else 0
test_pred_attrition <- factor(ifelse(predict_attr >= 0.177, "Yes", "No"))

# Confusion Matrix of Predicted vs Actual with Cutoff value of 0.177
confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
# Accuracy = 74.60%
# Sensitivity = 73.91% 
# Specificity = 74.73%

# ROC, AUC, KS Statistic, Gain and Lift
test_cutoff_attr <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attrition=="Yes",1,0)

test_pred <- prediction(test_cutoff_attr, test_actual_attr)
test_performance_values <- performance(test_pred, "tpr", "fpr")

# ROC
plot(test_performance_values, main = "ROC Curve")
abline(0,1, lty = 8, col = "grey", untf = T)

# AUC
perf <- performance(test_pred, "auc")
attr(perf, "y.values")[[1]][[1]]
# AUC is 74.32%

# Common function to create a table containing deciles, num of churns, num of non churns, cumulative churns, cumulative non churns,
# gain, lift and KS values
lift <- function(labels, predicted_prob, groups) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels, predicted_prob))
  helper[,"decile"] = ntile(-helper[,"predicted_prob"], groups)
  table = helper %>% group_by(decile)  %>%
    summarise_at(vars(labels), funs(obs = n(),
                                     churn=sum(., na.rm = TRUE), non_churn=(obs - churn))) %>%
    
    mutate(cum_churn = cumsum(churn),
           gain=cum_churn/sum(churn)*100,
           cum_lift=gain/(decile*(100/groups)),
           cum_nonchurn = cumsum(non_churn),
           per_cum_nonchurn = (cum_nonchurn/sum(non_churn)) * 100,
           ks = gain - per_cum_nonchurn) 
  return(table)
}
attrition_decile = lift(test_actual_attr, predict_attr, groups = 10)

####################
# Summary of Table
####################
attrition_decile

###################################################################################################################
#  From the table it can be seen that:
#  1. Gain is 78.3% by the 4th decile. This means that if all employees are sorted by probability then
#     among the top 40% of this sorted list we would be able to find 78% of employees who are likely to churn
#
#  2. Lift is 1.96 by 4th decile. This means that the model catches 1.9 times more attritions than a random model
#
#  3. KS Statistic value is 45.3 %, which is above 40%. 
#     It shows that the model discriminates the two classes well and most of the attritions can be found within the
#     top 4 deciles
###################################################################################################################

# Common function to plot Gain and Lift charts
plotGrapWithAbLine <- function(data, x, y, xlab, ylab, title, intercept, slope, vlinex, hliney) {
  ggplot(data) + aes(x, y) + geom_line(col = "blue") + 
    scale_x_continuous(breaks=seq(0, 10, by=1)) + labs(x=xlab, y=ylab, title=title) + 
    geom_abline(intercept = intercept, slope = slope, col = "red") + 
    geom_vline(xintercept = vlinex, linetype = 2) + geom_hline(yintercept = hliney, linetype = 2) +
    theme(plot.title = element_text(hjust = 0.5))
}

# Gain Chart
plotGrapWithAbLine(attrition_decile, attrition_decile$decile, attrition_decile$gain, "Decile", "Gain", "Gain Chart", 26, 7, 4, 78.3)

# Lift Chart
plotGrapWithAbLine(attrition_decile, attrition_decile$decile, attrition_decile$cum_lift, "Decile", "Cum Lift", "Lift Chart", 1, 0, 4, 1.96)

# KS Chart
plotGrapWithAbLine(attrition_decile, attrition_decile$decile, attrition_decile$ks, "Decile", "KS", "Kolmogorov–Smirnov (KS) Chart", 1, 0, 4, 45.3)

#########################################################################################################
##############################   Conclusions and Recommendations  #######################################
#########################################################################################################
# Conclusion:
# Significant drivers for predicting employee attrition are:
# 1. Age                                    8.  WorkLifeBalance    
# 2. NumCompaniesWorked                     9.  OverTime
# 3. TotalWorkingYears                      10. BusinessTravelNon-Travel
# 4. YearsSinceLastPromotion                11. BusinessTravelTravel_Frequently
# 4. YearsWithCurrManager                   12. MaritalStatusDivorced
# 6. EnvironmentSatisfaction                13. MaritalStatusMarried
# 7. JobSatisfaction                        14. EducationFieldHuman Resources

# Recommendations:
# 1. Monitor closely the overtime hours of an employee and provide balance workload in order to reduce work pressure
# 2. Ensure that an employee does travel in a calendar year but not too frequently as it disrupts the work life balance
# 3. An employee remaining for a number of years without promotion must be carefully reviewed by the company
# 4. Focussed interactions must be had with employees having a lower satisfaction level and reasons must be fully understood
# 5. Employees with education in HR must be constantly motivated and provided incentives to perform
# 6. An employee would be more satisfied at work when remaining with one line manager for a longer duration

#######
