# import the libraries
install.packages("caret")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")
install.packages("kernlab")

library(caret)
library(kernlab)
library(reshape2)
library(ggplot2)
library(GGally)
library(dplyr)

######################################
# Business understanding
######################################

#--------------#
# Goals:
#--------------#
# Your solutions will be evaluated on 2 criteria:
#   
# The base probability of receiving a premium on a policy without considering any incentive
# The monthly incentives you will provide on each policy to maximize the net revenue 

# Maximize the net revenue from these policies, "netrevenue = (renewals - incentives to agents)"

# The client has provided the following relationships:
# Expected effort in hours put in by an agent for incentives provided; and Exp_Hours_Agents, Mean house
# Expected increase in chances of renewal, given the effort from the agent. and Exp_renewal_chance, "Mean chance of renewal" 

# Cacluation of Net revenue:
# Net_revenue = SUM(
#                   (RENEWAL_PROBABILITY_BY_THE_MODEL + %IMPROVEMENT_IN_RENEWAL_PROBABILITY CALCULATED FROM AGENT_WORKING_OURS) * PREIMUM OF POLICY 
#                   - INCENTIVES ON POLICY)
#                   )
# 'Incentive on policy' -  given to the agent for increasing the chance of renewal (estimated by the pariticipant) for each policy

# 1. Relationship between the the effort-incentives is:
#   Y(effort) = 10*(1-exp(-x/400)) , x = incentives
# 2. Relationship between the %Improvement in renewal probability Vs Agent effort in hours is:
#   Y(%imprvmnt in renewal prob) = 20*(1-exp(-x/5)) , where x = effort (Hours)

# Objectives is to determine
# 1. Renewal Probability
# 2. Improvement In renewal probability
# You have to determine the improvement in the renwal proability, using this you can calcualte Effort Hours, which can be used to calculate incentives
# % Improvement in Renewal probability can be determined from:
# (how many customers with delayed payment renewed/Total customers who had paid despite delayed payment + Total customer w)



#######################
# Data Understanding 
######################

# importing the train and test datasets
train <-  read.csv(file = "Dataset/train.csv",stringsAsFactors = TRUE)
test <- read.csv(file = "Dataset/test_66516Ee.csv",stringsAsFactors = TRUE)


# Split the test date to validation and test datasets 70-30 ration
indices <- sample(x = 1:nrow(test),size = 0.7*nrow(test))

# Splitting the dataset into validation and private datasets
test_validation <- test[indices,]
test_private <- test[-indices,]
# checking the datasets
head(train)
head(test)

str(train)

#--------------------------------#
# Feature Engineering
#--------------------------------#

# Check for duplicates
sum(duplicated(train$id))     # Comments: No duplicates are identified 

#check for NAs
which(sapply(train, function(x) sum(is.na(x))) > 0) #Count_3.6_months_late,Count_6.12_months_late,Count_more_than_12_months_late application_underwriting_score 

#check for outliers
sapply(train, function(x) summary(x))   # outliers found in income

# Percentable of renewals is much much higher compared to non-renewals
100*sum(train$renewal == 0)/nrow(train) # approx 6.23% of the people are not renewing


# Income has large distributions of data, thus using log transformation
train$Income_trns <- log(train$Income)
train$renewal <- as.factor(train$renewal)


#-------------------------------#
# Exploratory Data Analysis
#-------------------------------#
# Univariate Analysis
ggplot(data = train,aes(x = train$age_in_days)) + geom_histogram(aes(fill = renewal)) 
ggplot(data = train,aes(x = train$Income_trns)) + geom_histogram(aes(fill = renewal))
ggplot(data = train,aes(x = train$perc_premium_paid_by_cash_credit, fill = renewal)) + geom_histogram()
ggplot(data = train,aes(x = train$premium, fill = renewal )) + geom_histogram() 


# Analysis of these variables indicate there is no significant dependecy between the varaibale. 
# Emperical Analysis suggests that most of the variable have gaussian spread and renewals are spread around mean with +- 2Signma

# Bivariate Analysis
ggplot(data = train,aes(x = train$age_in_days, y = train$Income_trns,col = renewal)) + geom_point() #age and incomes are randomly distributed
ggplot(data = train,aes(x = train$age_in_days,y = train$premium,col = renewal)) + geom_point() # age and premiums are evenly distributed, this indicates there is less dependency betwen variables

# Segmented Univariate Analysis
library(reshape2)
library(dplyr)
ggplot(data = train,aes(x = train$residence_area_type,y = train$premium,fill = renewal)) + 
  geom_col()  # premiums from Rural ares is lesser compared to urban area, relatively % of non-renewals

# Which sourcing channnel has most non-renewals 
ggplot(data = train,aes(x = sourcing_channel,fill = renewal)) + 
  geom_bar() # Thus sourcing channel A,B,C has most applications

ggplot(data = train,aes(x = sourcing_channel,y = Income_trns,fill = renewal)) + 
  geom_col()
# Thus sourcing channel A,B,C has most applications.
# Sourcing Channel and Incomes are independent of each of other as there is no variation/correlation, 
# E group sourcing channel has no impact

# After how many premiums is the user 
ggplot(data = train,aes(x = factor(premium), fill = renewal )) + 
  geom_bar()  + 
  theme(axis.text.x = element_text(angle = 90)) # Major chunks of non-renewals are around 1200 to 18000


# 
train[,c("Count_3.6_months_late","Count_6.12_months_late","Count_more_than_12_months_late","renewal")] %>% 
  melt() %>% 
  ggplot(aes(x = variable,y = value,fill = renewal)) + geom_col() +
  theme(axis.text.x = element_text(angle = 90)) # largest chunk of late premiums are from 3-6 months







#------------------------------------------------------
# Calcuation for % improvement of renewal probability 
#------------------------------------------------------
# Total delayed  payments by each category
x1 <- sapply(train[,c("Count_3.6_months_late","Count_6.12_months_late","Count_more_than_12_months_late")] , function(x) sum(x,na.rm =  TRUE))
# Count_3.6_months_late         Count_6.12_months_late Count_more_than_12_months_late 
# 19833                           6236                           4786 

# Total delayed delayed payments by each late payment category, when the policy is renewed, that is, when renewal is 1
x2 <- sapply(train[,c("Count_3.6_months_late","Count_6.12_months_late","Count_more_than_12_months_late")]*train$renewal , function(x) sum(x,na.rm =  TRUE))
# Count_3.6_months_late         Count_6.12_months_late Count_more_than_12_months_late
# 15297                           3449                           3048               

# mean improvement of probability when the policy is renewed to the total late payments.(Mean chances of renewal)
mean(x2/x1)
# [1] 0.6537422

train %>% group_by(factor(premium)) %>% summarise(cnt = n())





