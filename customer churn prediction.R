# Data Preprocessing

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

churn <- read.csv('Customer-Churn.csv')
str(churn)

# "Churn" column will be the target
# Usage of "Sapply" checks if there are missing values in each columns

sapply(churn, function(x) sum(is.na(x)))

# Removing rows with missing values

churn <- churn[complete.cases(churn), ]

# Wrangling 

# Changing "No internet service" to "No" in sex
# columns; "OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","streamingTv","streamingMovies"

cols_reset <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
    churn[,cols_recode1][,i] <- as.factor(mapvalues(churn[,cols_recode1][,i], from = c("No internet service"), to = c("No")))
}

# "No phone service" to "No" for "MultipleLines" column

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, from = c("No phone service"), to = c("No")))

# Categorization of tenure

min(churn$tenure)
max(churn$tenure)
group_tenure <- function(tenure){
    if(tenure >= 0 & tenure <= 12){
        return('0-12 Month')
    } else if(tenure > 12 & tenure <= 24){
        return('12-24 Month')
    } else if(tenure > 24 & tenure <= 48){
        return('24-48 Month')
    } else if(tenure > 48 & tenure <= 60){
        return('48-60')
    } else if(tenure > 60){
        return('>60 Month')
    }
}

churn$tenure_group <- sapply(churn$tenure, group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

# changing values in colmun "SeniorCitizen" from "0 or 1" to "Yes or No"

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen, from = c("0","1"),to = c("No","Yes")))

# Removing columns that don't serve our objective

churn$customerID <- NULL
churn$tenure <- NULL

# Exploratory data analysis and feature selection
# Correlation between numeric variables

numeric.var <- sapply(churn, is.numeric)
corr.mattrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "number")

# Observation: The "Monthly Charges" and "Total Charges" are correlated. So one of them will be removed from the model.

churn$TotalCharges <- NULL

# Bar plots for categorical variables

plot_1 <- ggplot(churn, aes(x = gender)) + ggtitle("Gender") + xlab("Gender") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_2 <- ggplot(chur, aes(x = SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + geom_bar(aes(y = 100*(..count..)/sum(..sum..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_3 <- ggplot(churn, aes(x = Partner)) + ggtitile("Partner") + xlab("Partner") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_4 <- ggplot(churn, aes(x = Dependents)) + ggtitle("Dependents") + xlab("Dependents") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(plot_1,plot_2,plot_3,plot_4, ncol = 2)

plot_5 <- ggplot(churn, aes(x = PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_6 <- ggplot(churn, aes(x = MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_7 <- ggplot(churn, aes(x = InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_8 <- ggplot(churn, aes(x = OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(plot_5,plot_6,plot_7,plot_8, ncol = 2)

plot_9 <- ggplot(churn, aes(x = OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_10 <- ggplot(churn, aes(x = DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

plot_11 <- ggplot(churn, aes(x = TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_12 <- ggplot(churn, aes(x = StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(plot_9,plot_10,plot_11,plot_12, ncol = 2)

plot_13 <- ggplot(churn, aes(x = StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_14 <- ggplot(churn, aes(x = Contract)) + ggtitle("Contract") + xlab("Contract") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_15 <- ggplot(churn, aes(x = PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) +
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_16 <- ggplot(churn, aes(x = PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

plot_17 <- ggplot(churn, aes(x = tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
    ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(plot_13,plot_14,plot_15,plot_16,plot_17, ncol = 2)

# Observation: All of the categorical variables seem to have a reasonably broad distribution, 
# therefore all of them will be kept

# Logistic Regression

# Data splitting

intrain <- createDataPartition(churn$churn, p = 0.7, list = false)
set.seed(2017)
training <- churn[intrain,]
testing <- churn[-intrain,]

# splitting confirmation

dim(training);
dim(testing);

# Fitting the Logistic Regression Model

LogModel <- glm(Churn ~., family = binomial(link = "logit"), data = training)
print(summary(LogModel))

# Feature Analysis

# The top three most-relevant features include "Contract","tenure_group" and "PaperlessBilling"

anova(LogModel, test = "Chisq")

# Analyzing the deviance table we can see the drop in deviance when adding each variable one at a time. 
# Adding InternetService, Contract and tenure_group significantly reduces the residual deviance. 
# The other variables such as PaymentMethod and Dependents seem to improve the model less even though they all have low p-values.

# Assessing the predictive ability of the Logistic Regression model

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn == "No"] <- "0"
testing$Churn[testing$Churn == "Yes"] <- "1"
fitted.results <- predict(LogModel, newdata = testing, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy', 1-misClasificError))

# Logistic Regression Accuracy 0.789373814041746

# Logistic Regressions Confusion Matrix

print("Confusion Matrix for Logistic Regression");
table(testing$Churn, fitted.results > 0.5)

# Odds Ratio

exp(cbind(OR = coref(LogModel), confint(LogModel)))

# Decision Tree
# Decision Tree visualization
tree <- ctree(Churn ~ Contract + tenure_group + PaperlessBilling, training)
plot(tree)

# 1.Out of three variables, "Contract" is the 
# most important variable to predict customer's
# churn likelihood.

# 2. If a customer in a one-year or two-year contract,
# no matter he (she) has PapelessBilling or not, 
# he (she) is less likely to churn.

# 3. On the other hand, if a customer is in a month-to-month contract,
# and in the tenure group of 0–12 month, and using PaperlessBilling, 
# then this customer is more likely to churn.

# Decision Tree Confusion Matrix

# All the variables will be used to product confusion matrix table and make predictions.

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree");
table(Predicted = pred_tree, Actual = testing$Churn)

# Decision Tree Accuracy

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy', sum(diag(tab2))/sum(tab2)))

#  Decision Tree Accuracy 0.780834914611006
#  The accuracy for Decision Tree has hardly improved. Checking with Random Forest.

# Random Forest
# Random Forest Initial Model

rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

# The error rate is relatively low when predicting “No”,
# and the error rate is much higher when predicting “Yes”.

# Random Forest Prediction and Confusion Matrix
pred_rf <- predict(rdModel, testing)
caret::confusionMatrix(pred_rf, testing$Churn)

# Random Forest Error Rate

plot(rfModel)

# This plot helps determining the number of trees.
# As the number of trees increases, the OOB error rate decreases, 
# and then becomes almost constant. 
# It was not possibl to decrease the OOB error rate after about 100 to 200 trees.

# Tune Random Forest Model

t <- tuneRF(training[, -18], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = true, improve = 0.05)

# this plot displays some ideas on the number of mtry to choose. 
# OOB error rate is at the lowest when mtry is 2. Therefore, we choose mtry=2.

# Fit the Random Forest Model after tuning

rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = true, proximity = True)
print(rfModel_new)

# OOB error rate decreased to 20.41% from 20.61%

# Random Forest Predictions and Confison Matrix After Tuning

pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(pred_rf_new, testing$Churn)

# Both accuracy and sensitivity have improved, comparing with with Figure 15.

# Random Forest Feature Importance

varImpPlot(rfModel_new, sort = T, n.var = 10, main = 'Top 10 Feature Importance')

