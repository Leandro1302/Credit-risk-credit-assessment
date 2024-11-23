library(rlang)
library(caret)
library(dplyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)
library(reshape2)


df <- read.csv('Dataset.csv')
table(df$Loan_Status)

#Head
head(df)

#Tail
tail(df)

#Str
str(df)

#Shape
dim(df)

#vediamo i valori nulli
colSums(is.na(df))

###########################################à

#Checking the outliers
#boxplot(df)

# Number of ApplicantIncome possible outliers
ApplicantIncome_outliers <- sum(df$ApplicantIncome > 10000)
print(ApplicantIncome_outliers)

#Number of CoapplicantIncome possible outliers
CoapplicantIncome_outliers <- sum(df$CoapplicantIncome > 6000)
CoapplicantIncome_outliers

#Non sono outliers

#########################################

# Fill the null values of numerical variables
df$LoanAmount[is.na(df$LoanAmount)] <- median(df$LoanAmount, na.rm = TRUE)
df$Loan_Amount_Term[is.na(df$Loan_Amount_Term)] <- mean(df$Loan_Amount_Term, na.rm = TRUE)
df$Credit_History[is.na(df$Credit_History)] <- mean(df$Credit_History, na.rm = T)


colSums(is.na(df))

##Fill the empty values of categorical variables, 13, 3 , 15, 32
df$Married[df$Married == ""] <- df$Married[which(df$Married != "")][1]
df$Gender[df$Gender == ""] <- df$Gender[which(df$Gender != "")][1]
df$Dependents[df$Dependents == ""] <- df$Dependents[which(df$Dependents != "")][1]
df$Self_Employed[df$Self_Employed == ""] <- df$Self_Employed[which(df$Self_Employed != "")][1]

#Total income
df$Total_Income <- df$ApplicantIncome + df$CoapplicantIncome

############################################

# Print number of people who took loan by Gender
print("Number of people who took loan by Gender:")
print(table(df$Gender))
# Visualize the count of loans taken by gender using bar plot
barplot(table(df$Gender), main = "Number of people who took loan by gender", xlab = "Gender", ylab = "Count", col = "blue")

table(df$Loan_Status, df$Gender)

# Print number of people who took loan by Married
print("Number of people who took loan by Married:")
print(table(df$Married))
# Visualize the count of loans taken by Married using bar plot
barplot(table(df$Married), main = "Number of people who took loan by Married", xlab = "Married", ylab = "Count", col = "red")

table(df$Loan_Status, df$Married)

# Print number of people who took loan by Education
print("Number of people who took loan by Education:")
print(table(df$Education))
# Visualize the count of loans taken by Education using bar plot
barplot(table(df$Education), main = "Number of people who took loan by Education", xlab = "Education", ylab = "Count", col = "green")

table(df$Loan_Status, df$Education)
#####################################################
#CORRELATION

df1<- subset(df, select = -c( Loan_ID, Gender, Married, Dependents, Education, Self_Employed, Property_Area, Loan_Status))

# Calculating correlation matrix
corr <- cor(df1)

# Plotting heatmap
library(ggplot2)

# Plot heatmap
ggplot(data = melt(corr), aes(Var1, Var2, fill = value)) +
  geom_tile(color = 'black') +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(title = "Correlation Heatmap", x = "Numeric Variables", y = "Numeric Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor(df1)

#######################################################

#Distribution of variables
#Testing the Gaussian Distribution

#Shapiro walk
shapiro.test(df$Total_Income)
shapiro.test(df$LoanAmount)

#Kolmogorov-Smirnov
ks.test(df$Total_Income, mean(df$Total_Income), sd(df$Total_Income))
ks.test(df$LoanAmount, mean(df$LoanAmount), sd(df$LoanAmount))

#QQ-plot 
qqnorm(df$Total_Income)
qqline(df$Total_Income, col = 'red')

qqnorm(df$LoanAmount)
qqline(df$LoanAmount, col = 'red')

#LOG TRANSFORMATION
df$LoanAmountlog <- log(df$LoanAmount)
df$Loan_Amount_Term_log <- log(df$Loan_Amount_Term)
df$Total_Income_log <- log(df$Total_Income)

#QQ-plot LoanAmountlog
qqnorm(df$LoanAmountlog)
qqline(df$LoanAmountlog, col = 'red')

#QQ-plot Total_Income_log
qqnorm(df$Total_Income_log)
qqline(df$Total_Income_log, col = 'red')

#Drop unecessary columns
df <- subset(df, select = -c(ApplicantIncome, CoapplicantIncome, Loan_Amount_Term, LoanAmount, Total_Income, Loan_ID))
 

#Encoding variables
df$Gender <- ifelse(df$Gender == 'Male', 1, 0)
df$Married <- ifelse(df$Married == 'Yes', 1, 0)
df$Dependents <- as.numeric(gsub("\\+","", df$Dependents))
df$Education <- ifelse(df$Education == 'Graduate', 1, 0)
df$Self_Employed <- ifelse(df$Self_Employed == 'Yes', 1, 0)
df$Property_Area <- ifelse(df$Property_Area == 'Urban', 2,
                           ifelse(df$Property_Area == 'Semiurban', 1, 0))
df$Loan_Status <- ifelse(df$Loan_Status == 'Y', 1, 0)


#################################################
#Usando l albero di regr al posto della regr log binaria rimane uguale

#MODEL
# Splitting the data into training and testing sets
set.seed(42)
train_index <- sample(1:nrow(df), 0.8 * nrow(df))  # 80% training data
train_data <- df[train_index, ]
test_data <- df[-train_index, ]


# Creating and training the logistic regression model
model <- glm(Loan_Status ~ ., data = train_data, family = binomial)


# Making predictions on the testing set
predicted <- predict(model, newdata = test_data, type = "response")

# Converting probabilities to binary predictions (0 o 1)
threshold <- 0.5
predicted_class <- ifelse(predicted > threshold, 1, 0)


# Evaluating the model
accuracy <- mean(predicted_class == test_data$Loan_Status)
print(paste("Accuracy:", accuracy))

precision <- sum(predicted_class==1 & test_data$Loan_Status==1) / sum(predicted_class == 1)
recall <- sum(predicted_class==1 & test_data$Loan_Status==1) / sum(test_data$Loan_Status==1)
f1_score <- 2 * (precision*recall) / (precision + recall)

library(pROC)
roc_curve <- roc(test_data$Loan_Status, predicted) 
auc_value <- auc(roc_curve)

library(caret)
conf_matrix <- confusionMatrix(as.factor(predicted_class), as.factor(test_data$Loan_Status))


log_loss <- -mean(test_data$Loan_Status * log(predicted) + (1- test_data$Loan_Status) * log(1- predicted))
brier_score<- mean((predicted - test_data$Loan_Status)^2)
