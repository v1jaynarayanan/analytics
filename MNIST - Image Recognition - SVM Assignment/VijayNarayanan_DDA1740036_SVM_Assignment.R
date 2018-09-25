#############################################################################################
################## Handwritten Digit Recognition - SVM Assignment ###########################
################## Author : Vijay Narayanan                       ###########################
#############################################################################################

# Problem Statement
# MNIST data files train.csv and test.csv contain gray-scale images of hand-drawn digits, 
# from zero to nine. Each image is 28 pixels in height and 28 pixels in width, with a
# total of 784 pixels. There are 785 variables in total and the first variable a label,
# containing a value of 0 to 9 identifying the digit.
# There are 60000 observations in train dataset and 10000 observations in test dataset. 
# Goal is to develop a model that can correctly identify the digit 0-9 written in an image

# Objective
# Develop a model using Support Vector Machine which should correctly classify the handwritten
# digits based on pixel values given as features

# Install packages
packages_list <- c("caret", "kernlab", "dplyr", "readr", "ggplot2", "gridExtra")
packages_to_install <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Load libraries
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

# Set working directory
setwd("~/Downloads/Upgrad-IIIT/Course/Predictive Analytics II/SVM Assignment")

# Read input data
train <- read.delim("mnist_train.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)
test <- read.delim("mnist_test.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)

# Dimensions of training and test datasets
dim(train)
dim(test)
# Columns are the same

# Structure of training dataset
str(train)

# First row in train dataset
head(train, 1)

# First column is the target variable that has digits from 0-9. Rename this column to digit
colnames(train)[1] <- "digit"
colnames(test)[1] <- "digit"

# Train and test datasets can be combined so that cleaning can be done on one combined dataset rather than two
# First add a new column isTrain to the train dataset with a value 1 and to test dataset with a value 0
train$isTrain <- 1
test$isTrain <- 0

# Now, combine both train and test datasets into one 
combined_data <- rbind(train, test)

# Are there any duplicates?
combined_data[which(duplicated(combined_data))]
# There are no duplicates rows in train and test datasets

# Are there any missing values?
sum(is.na(combined_data))
# There are no NAs

# Check if digit column column has values only from 0-9
nrow(filter(combined_data, digit < 0 | digit > 9))
# All digits are valid and between 0 and 9

# Check for invalid pixel values in any of the columns i.e < 0 or > 255
sum(combined_data[combined_data < 0 | combined_data > 255])
# There are no invalid values

# Extract train and test datasets back from combined dataset
train <- combined_data[which(combined_data$isTrain == 1), ]
test <- combined_data[which(combined_data$isTrain == 0), ]

# Remove isTrain column 
train <- subset(train, select = -c(isTrain))
test <- subset(test, select = -c(isTrain))


# Common function for plot
plotBar <- function(data, title) {
  return(ggplot(data,aes(x=as.factor(digit),fill=digit))+
    geom_bar(stat="count",color="white")+
    scale_fill_gradient(low="lightblue",high="green",guide=FALSE)+
    stat_count(aes(label = scales::percent((..count..)/sum(..count..))), geom = "text", position=position_stack(vjust=0.5))+
    labs(title=title,x="Digits"))
}

# Plot histogram of train and test datasets to check digit
train_plot <- plotBar(train, "Digits in Train Data")
test_plot <- plotBar(test, "Digits in Test Data")
grid.arrange(train_plot, test_plot)
# Both train and test datasets have good representation of digits from 0-9
# Average proportion for each digit in both train and test datasets is 10% of total data

# There are 60000 observations in the train dataset and this could really take a lot of time
# to train the SVM model. So, a stratified sampling technique without replacement is used to 
# randonmly select a total of 5000 observations from the training dataset. 
# As obseved in the train and test data sets each digit is given a proportion of 10% of total observations. 
# So, each digit gets a 10% weight. Each digit 0-9 will have 500 random observations out of a total sample size of 5000

# Set seed
set.seed(123)

# Stratified sample using train dataset
stratified_sample <- train %>%
  group_by(digit) %>%
  mutate(num_rows=n()) %>%
  sample_n(500, weight=num_rows, replace = FALSE) %>%
  ungroup

# Plot stratified sample to check if each digit has equal representation in the dataset
plotBar(stratified_sample, "Digits in Stratified Train Data")

# Remove num_rows column 
stratified_sample <- stratified_sample[, !(names(stratified_sample) %in% c("num_rows"))]

# Convert target column digit to factor
stratified_sample$digit <- as.factor(stratified_sample$digit)
test$digit <- as.factor(test$digit)

#############################################################################################
#################################### Construct SVM Model ####################################
#############################################################################################

# Using Linear Kernel
model_linear <- ksvm(digit ~ ., data = stratified_sample, scale = FALSE, kernel = "vanilladot")
# Ignore warning "cannot scale data"
model_linear
# Number of Support Vectors = 1749
# C = 1

# Predict digit using linear SVM model
eval_linear<- predict(model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(eval_linear,test$digit)
# Accuracy is 91.56%

# Using Non-Linear Radial Kernel
model_RBF <- ksvm(digit ~ ., data = stratified_sample, scale = FALSE, kernel = "rbfdot")
# Ignore warning "cannot scale data"
model_RBF
# Number of Support Vectors = 2422
# C = 1
# sigma =  1.62140530540114e-07 

# Predict digit using RBF SVM model
eval_RBF<- predict(model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(eval_RBF,test$digit)
# Accuracy is 95.06%

# It can be seen that RBF model has a higher accuracy than the linear model and 
# this confirms that there is non linearity in the data

# Trying RBF model with a higher Cost parameter i.e C = 5
model_RBF_2 <- ksvm(digit ~ ., data = stratified_sample, scale = FALSE, kernel = "rbfdot", C=5)
# Ignore warning "cannot scale data"
model_RBF_2
# Number of Support Vectors = 2288
# C = 5
# sigma =  1.61347894207376e-07

# Predict digit using RBF SVM model 2
eval_RBF_2 <- predict(model_RBF_2, test)

#confusion matrix - RBF Kernel
confusionMatrix(eval_RBF_2,test$digit)
# Accuracy is 95.90%

# Trying RBF model with C = 10
model_RBF_3 <- ksvm(digit ~ ., data = stratified_sample, scale = FALSE, kernel = "rbfdot", C=10)
# Ignore warning "cannot scale data"
model_RBF_3
# Number of Support Vectors = 2296
# C = 10
# sigma =  1.61795969703776e-07 

# Predict digit using RBF SVM model 3
eval_RBF_3 <- predict(model_RBF_3, test)

#confusion matrix - RBF Kernel
confusionMatrix(eval_RBF_3,test$digit)
# Accuracy is 95.87%

# Accuracy was highest when C=5. But, Cross Validation needs to be done with Hyperparameters C and Sigma to determine the 
# best SVM model

#############################################################################################
################## Hyperparameter tuning and Cross Validation ###############################
#############################################################################################

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"

set.seed(321)
# Linear Kernel
# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm.lin <- train(digit ~ ., data=stratified_sample, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm.lin)
# Best Accuracy is 90 % and does not improve with varying values of cost C.

# Plotting "fit.svm.lin" results
plot(fit.svm.lin)

# RBF Kernel
grid <- expand.grid(.sigma=c(1.51e-07, 1.61347e-07, 1.71e-07), .C=c(2,3,4,5,6))

fit.svm.rbf <- train(digit ~ ., data=stratified_sample, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
# Ignore warning "cannot scale data"
print(fit.svm.rbf)

# Plot Cost Vs Accuracy
plot(fit.svm.rbf)
# Best accuracy of the SVM model after cross validation is 95% at C = 6 and Sigma = 1.71e-07

#############################################################################################
