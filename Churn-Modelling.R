library(dplyr)
library(ggplot2)
library(fastDummies)
library(rcompanion)
library(caret)
train_data <- read.csv("C:/Users/Jason/Desktop/Udemy Data Science A-Z/Modelling/Multiple Logistic Regression/Churn-Modelling.csv")
test_data <- read.csv("C:/Users/Jason/Desktop/Udemy Data Science A-Z/Modelling/Multiple Logistic Regression/Churn-Modelling-Test-Data.csv")

#feaure engineering
#get rid of row number column, factorize appropriate columns
train_data <- train_data %>% select(-c(RowNumber, CustomerId, Surname))
to_factor <- c("HasCrCard", "IsActiveMember", "Exited")
train_data[,to_factor] <- lapply(train_data[,to_factor], as.factor)

test_data <- test_data %>% select(-c(RowNumber, CustomerId, Surname))
to_factor <- c("HasCrCard", "IsActiveMember", "Exited")
test_data[,to_factor] <- lapply(test_data[,to_factor], as.factor)

#EDA

ggplot(train_data, aes(x = EstimatedSalary, y = Age, color = Geography)) + 
  geom_boxplot()+
  theme_classic()+
  coord_flip()

ggplot(train_data, aes(x = EstimatedSalary, y = Age, color = Gender)) + 
  geom_boxplot()+
  theme_classic()+
  coord_flip()

#normal distribution after the first bar at $0
ggplot(train_data, aes(x = Balance)) +
  geom_histogram(bins = 40)+
  theme_classic()

#no clear distribtion among estimated salaries
ggplot(train_data, aes(x = EstimatedSalary)) +
  geom_histogram(bins = 100)+
  theme_classic()

#since we're more curious about who exited and who didn't here are some graphs based on that criteria
ggplot(train_data, aes(x = Exited, y = Age, color = Gender)) + 
  geom_boxplot()+
  theme_classic()

#approximately 20% of people are leaving within 6 months
ggplot(train_data, aes(x = Exited)) + 
  geom_bar(color = "black", fill = "blue")+
  theme_classic()

#make applicable factors into Dummy variables with fastDummy package
train_data_dummy <- dummy_cols(train_data, select_columns = c("Geography", "Gender"))
test_data_dummy <- dummy_cols(test_data, select_columns = c("Geography", "Gender"))

#using backward elimination using P Value of .05 to create model iterations
#model 1
model1 <- glm(Exited ~ . -Geography -Gender -Gender_Male -Geography_France -Exited, data = train_data_dummy, family = binomial, maxit = 100)
summary(model1)

#showing our R Squared (using McFaddenn's)
nagelkerke(model1)

#model 2 eliminated Geography_Spain
model2 <- glm(Exited ~ . -Geography -Gender -Gender_Male -Geography_France -Geography_Spain -Exited, data = train_data_dummy, family = binomial, maxit = 100)
summary(model2)
nagelkerke(model2)

#model 3 eliminated HasCrCard
model3 <- glm(Exited ~ . -Geography -Gender -Gender_Male -Geography_France -Geography_Spain -HasCrCard -Exited, data = train_data_dummy, family = binomial, maxit = 100)
summary(model3)
nagelkerke(model3)

#model 4 eliminated EstimatedSalary
model4 <- glm(Exited ~ . -Geography -Gender -Gender_Male -Geography_France -Geography_Spain -HasCrCard -EstimatedSalary -Exited, data = train_data_dummy, family = binomial, maxit = 100)
summary(model4)
nagelkerke(model4)

#model 5 eliminated Tenure 
model5 <- glm(Exited ~ . -Geography -Gender -Gender_Male -Geography_France -Geography_Spain -HasCrCard -EstimatedSalary -Tenure -Exited, data = train_data_dummy, family = binomial, maxit = 100)
summary(model5)
nagelkerke(model5)

#Pseudo R Squared decreased after removing Tenure - expecting that Tenure would be a good indicator of staying with a bank so why would Tenure at this bank be insignificant?
#taking the log of Balance variable to normalize it
train_data_dummy <- train_data_dummy %>% mutate(log_balance = log10(Balance+1))
test_data_dummy <- test_data_dummy %>% mutate(log_balance = log10(Balance+1))

#model 6 using log_balance instead of Balance and adding back Tenure
model6 <- glm(Exited ~ . -Geography -Gender -Gender_Male -Geography_France -Geography_Spain -HasCrCard -EstimatedSalary -Balance -Exited, data = train_data_dummy, family = binomial, maxit = 100)
summary(model6)
nagelkerke(model6)

#predict with our model
probs <- predict(model6, newdata = test_data_dummy, type = "response")
#probabilities over 50% get a 1 (left the bank within 6 months) and less than 50% get a 0 (did not leave the bank within 6 months)
predict_test <- as.factor(ifelse(probs > 0.5, 1, 0))
#confusion matrix from the caret package
confusionMatrix(predict_test, test_data_dummy$Exited, dnn = c("Prediction", "Reference"))
