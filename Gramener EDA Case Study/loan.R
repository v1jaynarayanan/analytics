library(dplyr)
library(ggplot2)

# Importing file
loan <- read.csv("loan.csv")
# Finding rows with NA
is.na(loan)
# Counting NAs per column
colSums(is.na(loan))
# Keeping only the columns without NA
loan <- loan[ ,colSums(is.na(loan)) == 0]

#------------------------------------------------TWO PROPORTION TEST--------------------------------------------------------------

# Group by term, counting the rows by loan_status (THE IDEA is to get total number of defaulters for 36 months and 60 months)
a <- loan %>%
  group_by(term) %>%
  count(loan_status)
# Remove rows having values "Fully Paid" and "Current"
a <- a[!(a$loan_status == "Fully Paid" | a$loan_status == "Current"),]
# Group by term, counting the rows by term (THE IDEA is to get the number of loans disbursed for 36 months and 60 months)
b <- loan %>%
  group_by(term) %>%
  count(term)
# Perform two proportion test test
# H0 : Both proportions are equal
# H1 : Both proportions are not equal
# Confidence Level is 95%, Alpha is 0.05.
res <- prop.test(x = a$n, n=b$n, alternative = "two.sided", conf.level = 0.95)
# Result of the test
res
# Since the p-value is less than 0.05 we reject the null hypothesis. Thus, we conclude that the two proportions are not equal.
# From this test we can conclude that the significant loan defaulters are those people having 60 months loan duration.
# We can conclude that care should be exercised for those people who have applied for loan with 60 months duration.
# Thus. Loan Duration is one of the Driving Factors.

#---------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------PLOTS-------------------------------------------------------------------

# Breakdown of loan status column
ggplot(loan, aes(loan_status, fill = loan_status)) + geom_bar()
# It can be seen from the plot that out of 39752 loans sanctioned during the said period, 5672 loans have not been cleared.
# That is 14.27 % of the loans sanctioned have been defaulted. 

# Average annual income and loan status, is there any connection? Let's see.
ggplot(loan, aes(mean(annual_inc), fill = loan_status)) + geom_histogram(position = "dodge")
# It can be seen from the above plot that the average income for defaulters, current loan holders and people who paid off is 
# approximately the same. It can be concluded that annual income cannot be taken as a driving factor behind loan default.

# Purpose and loan status, is there any connection? let's see.
ggplot(loan, aes(purpose, fill = loan_status)) + geom_histogram(stat = "count", position = "fill")
# It can be seen from the above plot that the purpose for which the loan was taken for defaulters, current loan holders and 
# people who paid off is approximately the same. It can be concluded that purpose for loan taken cannot be taken as a driving 
# factor behind loan default.

# Verification Status and loan status, is there any connection? let's see.
ggplot(loan, aes(verification_status, fill = loan_status)) + geom_histogram(stat = "count", position = "dodge")
# It can be seen from the above plot that verification status does not have a bearing on defaulters, current loan holders and 
# people who paid off. In fact, a lot of people whose verification has not been done have paid off the loan. 
# It can be concluded that verification status (even if people are not verified, it does not have a bearing on defaulters).
# Therefore, verification status for loan taken cannot be taken as a driving factor behind loan default.



