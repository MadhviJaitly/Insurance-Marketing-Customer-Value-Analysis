library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

data <- Insurance_Marketing_Customer_Value_Analysis
str(data)
summary(data)

data$State <- as.factor(data$State)
data$Response <- as.factor(data$Response)
data$Coverage <- as.factor(data$Coverage)
data$Education <- as.factor(data$Education)
data$EmploymentStatus <- as.factor(data$EmploymentStatus)
data$Gender <- as.factor(data$Gender)
data$Location_Code  <- as.factor(data$Location_Code )
data$Marital_Status  <- as.factor(data$Marital_Status)
data$Policy_Type  <- as.factor(data$Policy_Type)
data$Renew_Offer_Type  <- as.factor(data$Renew_Offer_Type)
data$Policy  <- as.factor(data$Policy)
data$Sales_Channel  <- as.factor(data$Sales_Channel)

#Converting two categorical variables as factors

data$Number_of_Open_Complaints <- as.factor(data$Number_of_Open_Complaints)
data$Number_of_Policies <- as.factor(data$Number_of_Policies)
data$Vehicle_Class  <- as.factor(data$Vehicle_Class)
data$Vehicle_Size  <- as.factor(data$Vehicle_Size)

str(data)


sapply(data, function(x) sum(is.na(x)))
summary(data)


boxplot(data$Customer_Lifetime-Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Customer_Lifetime_Value <15628.739, ]
boxplot(data$Customer_Lifetime_Value)



boxplot(data$Customer_Lifetime-Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Customer_Lifetime_Value <14956.300, ]
boxplot(data$Customer_Lifetime_Value)

boxplot(data$Customer_Lifetime-Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Customer_Lifetime_Value <14956.300, ]
boxplot(data$Customer_Lifetime_Value)


boxplot(data$Income)

boxplot(data$Monthly_Premium_Auto)
quantile(data$Monthly_Premium_Auto, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Monthly_Premium_Auto <167, ]
boxplot(data$Months_Since_Policy_Inception)


boxplot(data$Total_Claim_Amount)
quantile(data$Total_Claim_Amount, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Total_Claim_Amount <944.994931, ]
boxplot(data$Total_Claim_Amount)

nrow(data)
names(data)


fit<- lm(Customer_Lifetime_Value ~ 	State+Response+	Coverage +
           Education+Effective_To_Date +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class+Vehicle_Size, data=data)

summary(fit)
