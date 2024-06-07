cddata <- read.csv("credit risk data.csv")
head(cddata)
summary(cddata)

cddata$person_home_ownership<- as.factor(cddata$person_home_ownership)
cddata$loan_intent<- as.factor(cddata$loan_intent)
cddata$loan_grade<- as.factor(cddata$loan_grade)
cddata$loan_status<- as.factor(cddata$loan_status)
cddata$cb_person_default_on_file <- ifelse(cddata$cb_person_default_on_file == "Y", 1, 0)
cddata$cb_person_default_on_file<- as.factor(cddata$cb_person_default_on_file)
summary(cddata)
str(cddata)


#EDA

library(ggplot2)

# AGE
n <- nrow(cddata)  
k <- ceiling(log2(n) + 1) 
k

hist(cddata$person_age, breaks = k, main = "Histogram of Age", xlab = "Years", col = "blue")
# I was suspicious why the x-axis of the histogram went all the way to 140 years so I added a scatter plot 
plot(cddata$person_age, main = "Histogram of Age", ylab = "Years",xlab="Count")

# there is outliers above 100 years of age, that I have removed due to it being very few values (only 5)
cddata<- cddata[cddata$person_age <= 100, ]

plot(cddata$person_age, main = "Histogram of Age", ylab = "Years",xlab="Count")
hist(cddata$person_age, breaks = k, main = "Histogram of Age", xlab = "Years", col = "blue")


#Annual Income
hist(cddata$person_income, breaks=k, main = "Histogram of Anual Income", xlab="Income")
#there are some very high values, that could be possibly outliers, but I chose to leave them in because it could just be milionares or extremely wealthy people and not mistakes 

ggplot(cddata, aes(y = person_income)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Boxplot of Income", y = "Income")

boxplot(cddata$person_income,
        main = "Boxplot of Income",
        ylab = "Income",
        col = "blue")


#Home Ownership
ggplot(cddata, aes(x = person_home_ownership)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Bar Chart of Home Ownership", x = "Home Ownership", y = "Count") +
  theme_minimal()
#bi modal data since there are 2 peaks


# Loan Intent
table(cddata$loan_intent)
pie(table(cddata$loan_intent))
# all categories are pretty equally represented
#chose to do a pie table because there are more 6 categories that were hard to see nicely in one bar chart

#Loan Grade
ggplot(cddata, aes(x = loan_grade)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Bar Chart of Loan Grade", x = "Loan Grade", y = "Count") +
  theme_minimal()

#Loan Amount
plot(cddata$loan_amnt, main = " ")
ggplot(cddata, aes(y = loan_amnt)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Loan Amount", y= "Loan Amount")

hist(cddata$loan_amnt, breaks=k, main = "Histogram of Amount of the Loan", xlab="Loan Amount")


#Loan interest rate
ggplot(cddata, aes(x = loan_int_rate)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Density Plot of Income", x = "Income", y = "Density") +
  theme_minimal()

hist(cddata$loan_int_rate, breaks=k, main = "Histogram of INterest Rate of the Loan", xlab="Interest Rate on Loan")


# Loan Status 
ggplot(cddata, aes(x = loan_status)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Bar Chart of Loan Status", x = "Default on loan?", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes"))

#the target variable is unbalanced 

#Loan as Percent Income
ggplot(cddata, aes(x = loan_percent_income )) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Density Plot of Loan as a Percent of the Income", x = "Income", y = "Density") +
  theme_minimal()

hist(cddata$loan_percent_income, breaks=k, main = "Histogram of Loan as a Percent of the Income",xlab="%")

# Default on file for the person
ggplot(cddata, aes(x = cb_person_default_on_file)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Bar Chart of Default on File", x = "Borrower Defaulted ", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes"))

#Credit History Length
hist(cddata$cb_person_cred_hist_length, breaks = 30, main = "Credit History", xlab = "Years")




#MIXED VARIABLE GRAPHS


# Employment length needs to increase with age 
plot(cddata$person_age, cddata$person_emp_length, main = "Scatter Plot", xlab = "Age", ylab = "Employment Length")


#Assumption: The more employment years, the higher the income due to more experience and seniority in positions,
# but we see much of a trend of that in this data
plot(cddata$person_emp_length, cddata$person_income, main = "Scatter Plot", xlab = "Age", ylab = "Income")


# People with higher income will have own rather than rent or have a mortgage
#However the data shows another pattern, that most of the people that own their home don't have a high income
#the majority of people either rent or have a mortgage, even high earners
ggplot(cddata, aes(x = person_income, y = person_home_ownership)) +
  geom_point() +
  xlab("Income") +
  ylab("Home Ownership")

# Assumption: Having defaulted in the past according to the loan history, the person is more likely to have a default on the requested loan
ggplot(cddata, aes(x = loan_status, fill = cb_person_default_on_file)) +
  geom_bar(position = "stack", alpha = 0.7) +
  labs(title = "Stacked Bar Chart of Loan Status by Default on File", x = "Loan Status") +
  theme_minimal()


#CORRELATION 
numeric_data <- cddata[, c(1,2,4,7,8,10,12)]
# Calculating the Pearson correlation coefficients between all numerical variables in the data set
cor(numeric_data,use = "complete.obs")

#low linear correlations so I tried other types of correlation 
# I can see age and the length in years of credit history are correlated, which is expected 

# Spearman correlation
cor(numeric_data,use = "complete.obs", method = "spearman")

# Kendall correlation
cor(numeric_data,use = "complete.obs", method = "kendall")





#DEALING WITH MISSING VALUES
# I have  895 missing values in the employment length and 3115 missing values in the interest rate variable

#KNN for inputing the employment length variable
library(VIM)

numeric_data <- cddata[, c(1, 2, 4, 7,8, 10, 12)]  

imputed_data <- kNN(numeric_data, variable = "person_emp_length", k = 5)
summary(imputed_data)


mean_before <- mean(numeric_data$person_emp_length, na.rm = TRUE)
sd_before <- sd(numeric_data$person_emp_length, na.rm = TRUE)

print(paste("Mean before imputation:", mean_before))
print(paste("Standard deviation before imputation:", sd_before))


mean_after <- mean(imputed_data$person_emp_length)
sd_after <- sd(imputed_data$person_emp_length)

print(paste("Mean after imputation:", mean_after))
print(paste("Standard deviation after imputation:", sd_after))

cddata$person_emp_length <- imputed_data$person_emp_length

summary(cddata)


#Inputing the loan interest rate

# According to the Pearson and Spearman correlation, the loan interest rate has pretty low correlation with all variables, it has the some correlation with the loan amount and the loan to income %

lm1 <- lm(loan_int_rate ~ loan_amnt + loan_percent_income, data = cddata, na.action = na.exclude)
#loan_amnt and loan_percent_income have very low p-values, so they are highly significant predictors of loan_int_rate. 
anova(lm1)
lm2 <- lm(loan_int_rate ~ ., data = cddata, na.action = na.exclude)
anova(lm1, lm2)
#model 2 is better due to lower R squared and better F statistic


# I also did an analysis of the residuals to further help me decide which model is better 
plot(resid(lm1))
abline(h = 0, col = "red")

plot(resid(lm2))
abline(h = 0, col = "red")

# I needed to remove the misisng values form the residuals so that I can find the summary statistics and compare them to the values of a normal distribution
residuals_lm1 <- residuals_lm1[!is.na(residuals_lm1)]
residuals_lm2 <- residuals_lm2[!is.na(residuals_lm2)]

library(e1071)

mean_residuals_lm1 <- mean(residuals_lm1)
sd_residuals_lm1 <- sd(residuals_lm1)
skewness_residuals_lm1 <- skewness(residuals_lm1)
kurtosis_residuals_lm1 <- kurtosis(residuals_lm1)

print(mean_residuals_lm1)
print(sd_residuals_lm1)
print(skewness_residuals_lm1)
print(kurtosis_residuals_lm1)

mean_residuals_lm2 <- mean(residuals_lm2)
sd_residuals_lm2 <- sd(residuals_lm2)
skewness_residuals_lm2 <- skewness(residuals_lm2)
kurtosis_residuals_lm2 <- kurtosis(residuals_lm2)

print(mean_residuals_lm2)
print(sd_residuals_lm2)
print(skewness_residuals_lm2)
print(kurtosis_residuals_lm2)


#GIven that the values expected from a normal distribution,of the the mean, standard deviation, skewness, and kurtosis are zero, one, zero, and three, respectively
# Regression model 2 seems to be better because it has a lower  standard deviation. 
#Both models have mean residuals close to zero. 
# lm1 has a slight positive skewness, while lm2 has a slight negative skewness

predicted_interest_rate <- predict(lm2, newdata = cddata[is.na(cddata$loan_int_rate),])

cddata$loan_int_rate[is.na(cddata$loan_int_rate)] <- predicted_interest_rate

summary(cddata$loan_int_rate)

# compared to the initial statistics before inputation, the minimum, maximum, median, mean and 3rd quantile haven't changed
# the only change is in the 1st quantile that went form 7.90 to 7.88, which is not a significant change

summary(cddata)



#PCA

library(stats)


cddata.scaled <- scale(cddata[, c(1,2,4,7,8,10,12)])
cddata.pca <- prcomp(cddata.scaled)
cddata.pca
summary(cddata.pca) 
# we have 7 principal components 
#The first principal component explains the most variance, followed by the second principal component, and so on.
# I have chosen the first 5 principal components because their cumulative proportion of variance they explain is 95.5%.
head(cddata.pca$x[, 1:5])




#Given that my target attribute of the status of the loan is not that balanced, I have chosen to apply random forest so that I can try to predict if a borrower will default or no on a loan based on all other variables.
#Random Forest

set.seed(123)
n_rows<-nrow(cddata)
n<- round(0.8 * n_rows)

train_indices <- sample(1:n_rows, n, replace = FALSE)

train_data <- cddata[train_indices, ]
test_data <- cddata[-train_indices, ]


library(randomForest)
forest <- randomForest(loan_status ~ ., data = train_data)
predictions <- predict(forest, test_data)


confusion_matrix <- table(test_data$loan_status, predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))








